---
title: "Quantifying the Future of Work: A Data-Driven Analysis of Automation Risk Across Industry Sectors"
author: "Claire Ackerman and Ashley Becker"
date: "04/30/2026"
output: html_document
---

## Introduction
This repository contains the code and data required to reproduce the results found in "Quantifying the Future of Work: A Data-Driven Analysis of Automation Risk Across Industry Sectors". The objective is to identify trends in characteristics in different occupations and how they relate to the probabability of AI automation.

## Requirements
To install the required R packages, run the following code in R:


```r
install.packages(c("tidyverse", "dplyr", "olsrr", "interactions", "ggplot2",
                   "scales", "glmnet", "readxl", "readr", "stringr","tidyr"))
```

## Data

We use two sources of data: Wocke's Probability of Automation Data Set and the Bureau of Labor's Occupational Employment and Wage Statistics (2024). This data can be found in the [Data](./Data) sub-directory:  
 - **[Probability of Automation Data Set](Data/Probability%20of%20Automation%20Data%20Set.csv)**

 - **[National Occupational Employment and Wage Estimates (2024)](Data/national_M2024_dl.xlsx)**  

However, the final dataset to be used in the analysis includes total employment data from the Bureau of Labor dataset and AI generated characteristics for each occupation. This data can be found in the [Data](./Data) sub-directory as well: 
 - **[final_prob_auto_w_weights_industry](Data/final_prob_auto_w_weights_industry.csv)**

## Reproduce
1. Run `Probability of Auomation Final Code.R` found in sub-directory [Code](./Code) and load [final_prob_auto_w_weights_industry](Data/final_prob_auto_w_weights_industry.csv) when prompted to reproduce all plots and predictive models.


### Optional Steps to Achieve employment totals in final dataset, [final_prob_auto_w_weights_industry](Data/final_prob_auto_w_weights_industry.csv): 
1. Run `prob_auto_merged_national_code.R` to create a file combing the original Probability of Automation Data Set and the total employment count.
2. Utilize AI chatbot of your choice to create the following fields based on context clues in the occupation field:

- **education_level** *(character)*  
  Highest typical education level required for the occupation.

- **no_ed_requirement** *(integer / binary)*  
  Indicator (0/1) for occupations with no formal education requirement.

- **hs_diploma_or_equiv** *(integer / binary)*  
  Indicator (0/1) for high school diploma or equivalent.

- **post_secondary_no_degree** *(integer / binary)*  
  Indicator (0/1) for postsecondary education without a degree.

- **associates** *(integer / binary)*  
  Indicator (0/1) for associate’s degree requirement.

- **bachelors** *(integer / binary)*  
  Indicator (0/1) for bachelor’s degree requirement.

- **masters** *(integer / binary)*  
  Indicator (0/1) for master’s degree requirement.

- **doctoral** *(integer / binary)*  
  Indicator (0/1) for doctoral or professional degree requirement.

- **occ_clean** *(character)*  
  Cleaned occupation title used for merging across datasets.

- **TOT_EMP** *(integer)*  
  Total national employment for the occupation.

- **industry** *(character)*  
  Broad industry classification for the occupation.

- **is_medical** *(integer / binary)*  
  Indicator (0/1) for medical or healthcare-related occupations.

- **is_programming** *(integer / binary)*  
  Indicator (0/1) for programming or software-related occupations.

- **is_blue_collar** *(integer / binary)*  
  Indicator (0/1) for blue-collar occupations.

- **is_service** *(integer / binary)*  
  Indicator (0/1) for service-sector occupations.


## References

U.S. Bureau of Labor Statistics (2024), “National Occupational Employment and Wage Estimates”,​ U.S. Department of Labor, OEWS Data (national_M2024_dl.xlsx), https://www.bls.gov/oes/tables.htm​

​Wocke, Albert (2019), “Probability of Automation of Occupations 2036”, Mendeley Data, V1, doi: 10.17632/czbvhmzwm3.1, https://data.mendeley.com/datasets/czbvhmzwm3/1​

​

