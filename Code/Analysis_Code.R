################################**Load Packages**###############################
packages <- c("dplyr", "expss", "Hmisc", "lavaan", "lsr", "magrittr", "psych", "report", "semTools", "stringr")
for (package in packages){
  if(!is.element(package, .packages(all.available = TRUE))){install.packages(package)}
  library(package, character.only = TRUE)}

options(warn = -1)

#########################**Load and Pre-Process Data**##########################
data <- read.csv(Sys.glob("C:\\Users\\*\\Downloads\\Waldren_RASD_Data.csv"))

#Order by sex, first 500 = Female (0), second 500 = Male (1)
data <- data[order(data$Sex),]

#List containing Total, Male & Female split data
sex_data <- c(list(Whole = data, Male = filter(data, Sex == 1), Female = filter(data, Sex == 0))) 

#List of columns for each item plus sex
measure_cols <- c(list(                                              
  AQ50 = c(paste0("AQ50_Q", 1:50), "Sex"),
  AQ28 = c(paste0("AQ50_Q", c(1:4,6,8:11,13:15,19,20,22,23,25,32,34,36,37,41,42,44:47,50)), "Sex"),
  AQ26 = c(paste0("AQ50_Q", c(5:7,9,11:13,15,17,19,20,22,23,25,26,34,35,37:40,43:45,47,50)), "Sex"),
  AQ20 = c(paste0("AQ50_Q", c(2,5,7,9,11:13,15,18,20:22,31:33,36,41,43,44,49)), "Sex"),
  AQ10 = c(paste0("AQ50_Q", c(5,20,27,28,31,32,36,37,41,45)), "Sex"),
  AQ9  = c(paste0("AQ50_Q", c(6,12,15,17,19,22,23,44,47)), "Sex"),
  CATI = c(paste0("CATI_Q", 1:42), "Sex"),
  BAPQ = c(paste0("BAPQ_Q", 1:36), "Sex"), 
  ITEM = c("OATS", "Sex"))) 

#Create Whole Sample, Male, amd Female data frames for each measure
measures <- c(list())                             
for (mes in 1:length(measure_cols)){                     #For each the columns in each measure
  for(df in 1:length(sex_data)){                  #For the Whole, Male, and Female data sets separately
    #Add new data frame to list for each measure and sex split
    measures <- append(measures, list(sex_data[[df]][,c(measure_cols[[mes]])])) 
    #Label with correct Sex_Measure combination
    names(measures)[[length(measures)]] <- paste0(names(sex_data[df]), "_", names(measure_cols[mes]))}}  

###########################**Descriptive Statistics**###########################
psych::describe(data["Age"])                         #Sample Age

totals <- matrix(0, nrow(data), length(measures))    #Matrix for total scores
colnames(totals) <- c(names(measures))               #Data frame for descriptive statistics
descriptives <- data.frame(Data = NA, Mean = NA, SD = NA, Min = NA, Max = NA, Range = NA, Alpha = NA, Omega = NA)

for (m in 1:length(measures)){                                                  
  measure <- as.data.frame(measures[m])              #Extract each data frame from the Sex_Measure list
  total <- sum_row(measure[1:(ncol(measure) -1)])    #Create total score of measure for each ppt (excluding Sex column)
  #Put Male Totals in second half of matrix to match ppt order in data
  if(str_detect(names(measure)[1], "Male")){totals[c(501:1000), m] <- total} 
  else{totals[c(1:length(total)), m] <- total}                                 
  desc <- psych::describe(total)                     #Output descriptive statistics for all data frames
  descriptives[nrow(descriptives) +1,1:6] <- c(names(measures)[m], desc[[3]], desc[[4]], desc[[8]], desc[[9]], desc[[10]])
  if(ncol(measure) != 2){    
    #Output reliability for all data frames except the OATS
    alph <- psych::alpha(measure[,1:(ncol(measure)-1)])$total 
    omega <- psych::omega(measure[,1:(ncol(measure) -1)], nfactor = 1)
    descriptives[nrow(descriptives), 7:8] <- c(alph[[1]], omega$omega.tot)}}

####**Correlations**####
Whole_Total <- as.data.frame(totals) %>% select_if(grepl("Whole", names(.)))
Female_Total <- as.data.frame(totals[1:500,]) %>% select_if(grepl("Female", names(.)))
Male_Total <- as.data.frame(totals[501:1000,]) %>% select_if(grepl("Male", names(.))) 

Whole_Cor <- corCi(Whole_Total, method = "spearman", n = 1000, plot = F)    
Male_Cor <- corCi(Male_Total, method = "spearman", n = 500, plot = F)
Female_Cor <- corCi(Female_Total, method = "spearman", n = 500, plot = F)

####**T-Tests**####
TTest <- data.frame(t = NA, df = NA, p = NA, d = NA, CI_Low = NA, CI_High = NA) #Data frame to store T-Tests
Whole_Total$Sex[1:500] <- 0; Whole_Total$Sex[501:1000] <- 1            #Re-introduce Sex variable for comparisons
for (m in colnames(Whole_Total[1:9])){                                 #For each measure
  t <- t(unlist(t.test(Whole_Total[[m]] ~ Whole_Total$Sex)))           #T-test comparing total score by sex
  d <- cohensD(Whole_Total[[m]] ~ Whole_Total$Sex, Whole_Total, method = "unequal") #Calculate Cohen's d
  ci <- cohen.d.ci(d, n=1000, n2=500, n1=500,alpha=.05)                             #Calculate 95% CI
  TTest[nrow(TTest) +1,] <- c(t[[1]], t[[2]], t[[3]], d, ci[[1]], ci[[3]])          #Add to data frame
  rownames(TTest)[nrow(TTest)] <- m}

###########################**Measurement Invariance**###########################
####**MI Functions**####
get_output <- function(models, measures, m, constraints, n){
  name <- c("Config", "Metric", "Scalar", "Strict")
  thres <- list(c(NA), c(1), c(1:2), c(1:3))
  if(n == 1){model <- cfa(models[[m]], data = measures[[m]], ordered = T, estimator = "WLSMV", group = "Sex")
  }else{model <- cfa(models[[m]], data = measures[[m]], ordered = T, estimator = "WLSMV", group = "Sex", 
                     group.equal = constraints[thres[[n]]])}
  mfit <- t(as.data.frame(fitMeasures(model)))
  chisq <- data.frame(ChiSq_0 = lavInspect(model, "test")$standard$stat.group[[1]],
                      ChiSq_1 = lavInspect(model, "test")$standard$stat.group[[2]],
                      ChiSq_STD_0 = lavInspect(model, "test")$scaled.shifted$stat.group[[1]],
                      ChiSq_STD_1 = lavInspect(model, "test")$scaled.shifted$stat.group[[2]])
  latmean <- as.data.frame(lavInspect(model, "est")$`1`$alpha)
  rel <- as.data.frame(t(compRelSEM(model)))
  rel_latmean <- data.frame(Model = rep(paste0(names(models)[[m]], "_", name[n]), length(latmean)), 
                            Var = rownames(latmean), 
                            F_Rel = rel[-c(1), 1], 
                            M_Rel = rel[-c(1), 2], 
                            LatMean = latmean[,1])
  rownames(chisq) <-  paste0(names(models)[[m]], "_", name[n])
  rownames(mfit) <-  paste0(names(models)[[m]], "_", name[n])
  return(list(model, mfit, chisq, rel_latmean))}

MI <- function(models, measures, constraints){
  output <- list()
  mfit <- as.data.frame(matrix(0, ncol = 70, nrow = 1, dimnames=list(NULL, mfit_cols)))
  chisq <- data.frame(ChiSq_0 = NA, ChiSq_1 = NA, ChiSq_STD_0 = NA, ChiSq_STD_1 = NA)
  rel_latmean <- data.frame(Model = NA, Var = NA, F_Rel = NA, M_Rel = NA, LatMean = NA)
  for(m in 1:length(models)){
    #Generate Models
    config <- get_output(models, measures, m, constraints, 1)
    metric <- get_output(models, measures, m, constraints, 2)
    scalar <- get_output(models, measures, m, constraints, 3)
    strict <- get_output(models, measures, m, constraints,4)
    #Append to output
    output[[paste0(names(models)[m], "_Config")]] <- config[[1]]
    output[[paste0(names(models)[m], "_Metric")]] <- metric[[1]]
    output[[paste0(names(models)[m], "_Scalar")]] <- scalar[[1]]
    output[[paste0(names(models)[m], "_Strict")]] <- strict[[1]]
    #Append to metric data frames
    mfit <- rbind(mfit, config[[2]], metric[[2]], scalar[[2]], strict[[2]])
    chisq <- rbind(chisq, config[[3]], metric[[3]], scalar[[3]], strict[[3]])
    rel_latmean <- rbind(rel_latmean, config[[4]], metric[[4]], scalar[[4]], strict[[4]])}
  return(list("Output" = output, "Model_Fit" = mfit, "X2_Contribution" = chisq, "Reliability_LatentMeans" = rel_latmean))}

####**User Input**####
#models = List containing the lavaan models, in same order as measures
factors <- readRDS(Sys.glob("C:\\Users\\*\\Downloads\\Factor_Models.RDS"))
factors <- factors[c(1:4, 6:8)]

one <- readRDS(Sys.glob("C:\\Users\\*\\Downloads\\One_Models.RDS"))
one <- one[c(1:4, 6:8)]

#measures = List containing the whole sample data sets for each Measure plus sex, in same order as 'models' input
whole_measures <- measures[c(seq(1, 12, 3), seq(16, 24, 3))]

#constraints = Vector of the group.equal constraints to be applied, in the order they should be implemented
traditional <- c("loadings", "intercepts", "residuals")   #As reported in Main Text
new <- c("thresholds", "loadings", "residuals")           #As reported in Supplementary Materials 

#mfit column names 
mfit_cols <- c("npar" ,  "fmin" ,  "chisq" ,  "df" ,  "pvalue" ,  "chisq.scaled" ,  "df.scaled" ,  
"pvalue.scaled" ,  "chisq.scaling.factor" ,  "baseline.chisq" ,  "baseline.df" ,  "baseline.pvalue" ,  
"baseline.chisq.scaled" ,  "baseline.df.scaled" ,  "baseline.pvalue.scaled" ,  "baseline.chisq.scaling.factor" ,  
"cfi" ,  "tli" ,  "cfi.scaled" ,  "tli.scaled" ,  "cfi.robust" ,  "tli.robust" ,  "nnfi" ,  "rfi" ,  "nfi" ,  
"pnfi" ,  "ifi" ,  "rni" ,  "nnfi.scaled" ,  "rfi.scaled" ,  "nfi.scaled" ,  "pnfi.scaled" ,  "ifi.scaled" ,  
"rni.scaled" ,  "nnfi.robust" ,  "rni.robust" ,  "rmsea" ,  "rmsea.ci.lower" ,  "rmsea.ci.upper" ,  "rmsea.ci.level" ,  
"rmsea.pvalue" ,  "rmsea.close.h0" ,  "rmsea.notclose.pvalue" ,  "rmsea.notclose.h0" ,  "rmsea.scaled" ,  
"rmsea.ci.lower.scaled" ,  "rmsea.ci.upper.scaled" ,  "rmsea.pvalue.scaled" ,  "rmsea.notclose.pvalue.scaled" ,  
"rmsea.robust" ,  "rmsea.ci.lower.robust" ,  "rmsea.ci.upper.robust" ,  "rmsea.pvalue.robust" ,  "rmsea.notclose.pvalue.robust" ,  
"rmr" ,  "rmr_nomean" ,  "srmr" ,  "srmr_bentler" ,  "srmr_bentler_nomean" ,  "crmr" ,  "crmr_nomean" ,  "srmr_mplus" ,  
"srmr_mplus_nomean" ,  "cn_05" ,  "cn_01" ,  "gfi" ,  "agfi" ,  "pgfi" ,  "mfi" ,  "wrmr")


##**Run MI Analyses**##
factors_traditional <- MI(factors, whole_measures, traditional) 
factors_new <- MI(factors, whole_measures, new)

one_traditional <- MI(one, whole_measures, traditional)   #Run separately for AQ10, setting std.lv to TRUE
one_new <- MI(one, whole_measures, new)

rm(list=setdiff(ls(), c(Whole_Total, Whole_Cor, Male_Total, Male_Cor, Female_Total, Female_Cor,
                        descriptives, TTest,
                        factors_new, factors_traditional, one_new, one_traditional)))
############################**Session Information**#############################
session <- sessionInfo()
report_system(session)
as.data.frame(report(session))
