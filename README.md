# Is there an optimal self-report measure to investigate autism-related sex differences?

This Supplementary Material document contains the additional output, analysis code, and data set associated with Waldren et al. (2025). Please select the relevant tabs to find the information of interest.

## Table of Contents

### [Study Analysis](https://github.com/Lucy-Wal/Autism_Measurement_Invariance#study-analysis-1)
Study Analysis contains the commented analysis R script associated with this research.

### [Data](https://github.com/Lucy-Wal/Autism_Measurement_Invariance#data-1)
Data contains the open-access data set to accompany this study, and the model factor .RDS files needed for the analysis script to run. Data structure and relevant information are reported.


##



### Study Analysis
https://github.com/Lucy-Wal/Autism_Measurement_Invariance/blob/64d7b3cca2fb774ade957c0adbfd57533ae7fde3/Code/Analysis_Code.R#L1-L167

### Data 

#### Multi-Factor Models
File containing the multi-factor model structures needed for measurement invariance analysis.

[Download Factor_Models.RDS](https://github.com/Lucy-Wal/Autism_Measurement_Invariance/blob/main/Data/Factor_Models.rds)

#### One Factor Models 
File containing the uni-factor model structures needed for measurement invariance analysis.

[Download Factor_Models.RDS](https://github.com/Lucy-Wal/Autism_Measurement_Invariance/blob/main/Data/One_Models.rds)

#### Study Data 
All data was collected between September 12th 2022 and September 22nd 2022.

Waldren_RASD_Data.csv:
All items have been reversed scored as required, with higher scores on each item indicating greater endorsement of autistic traits.

- AQ50_Q1-AQ50_Q50:All items in the AQ50 in the order outlined in Baron-Cohen et al. (2001).
- CATI_Q1-CATI_Q42: All items in the CATI in the order outlined in English et al. (2021).
- BAPQ_Q1-BAPQ_Q36: All items in the BAPQ in the order outlined in Hurley et al. (2007).
- OATS: The Overall Autistic Trait Scale. Scores can range from 0-100.
- Age: Years.
- Sex: 0 = Female, 1 = Male.
- Education: 0 = No formal Education, 8 = PhD, 9 = Other (for further details see UNESCO Institute for Statistics, 2012).

[Download Study Data (.csv)](https://github.com/Lucy-Wal/Autism_Measurement_Invariance/blob/main/Data/Waldren_RASD_Data.csv)

