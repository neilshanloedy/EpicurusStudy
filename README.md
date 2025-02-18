# R PROJECT README
# The Role of Frailty in Shaping Social Contact Patterns in Belgium, 2022-2023 ##

Curent Version: 1.0.0.

## Summary of the study
The primary aim of this study is to present the contact survey focused on older adults as original results and demonstrate their potential role in mathematical models of disease transmission. This is done by making simplifying assumptions on assortativity and examining several scenarios to explore the potential impact of incorporating frailty into the models. We found distinct contact patterns across different frailty levels. By integrating these patterns into mathematical models, we show that accounting for frailty-related heterogeneity significantly impacts disease transmission. These findings demonstrate the importance of considering frailty in infectious disease modelling and suggest the need for further research in other populations.

## Scientific output related to this model:
* [The Role of Frailty in Shaping Social Contact Patterns in Belgiu, 2022-2023 (Loedy et al, 2023)] (https://www.medrxiv.org/content/10.1101/2024.10.10.24315233v1.full)

## Github repository ##
We have developed and continuously managed this R project using GitHub for robust version control. Upon acceptance of our manuscript we made all the code and input data necessary to replicate the published scenarios available in a public archive on ZENODO.

## Code version
Manual version number 1.0

## Overview of folders and files
This Git repository is divided into four folders, code, data, output, and rds folders.
In the code_final folder, you will find all the necessary codes to reproduce the results in the manuscript.
In the data folder, you will find all the data utilised to run the analyses.

| Folder          | Content                                                                                     |
|-----------------|---------------------------------------------------------------------------------------------|
| code            | All necessary codes to reproduce the results in the manuscript				|
| data            | All the data utilised to run the analyses 							|
| output          | 												|
| rds    	  | 												|


### 1. data
#### 1 avg_cnt_frail_agegroup.csv
The summary of the number of contacts reported per age group and frailty levels, for different sampling method.

### 2. rds
#### 2.1. output_compartment
This folder contains all the outputs of the mathematical compartment model, both for the one q value scenario, and for the same R0 = 2.90 scenario.
- sir_recovered_**scenario**_with_ci.rds will give you the results of the recovered individuals within the simulations.
- sir_ttp_**scenario**_with_ci.rds will give you the results of the time to peak of the simulations.
- sir_result_**scenario**_with_ci.rds will give you the whole results of the simulations.

#### 2.2. matrix_decomposition
This folder contains all the outputs of the matrix decomposition (full assortative, uniform, and proportionate mixing assumptions).
- data_outside_home.rds --> cleaned dataset for contacts not at home

- model_alone_home.rds --> GAMLSS model for those living alone for the reported number of contacts at home
- model_alone_nothome.rds --> GAMLSS model for those living alone for the reported number of contacts at not home
- model_household_home.rds --> GAMLSS model for those living with others for the reported number of contacts at home
- model_household_nothome.rds --> GAMLSS model for those living with others for the reported number of contacts at not home
- model_rob_home.rds --> GAMLSS model for those living in healthcare facilities for the reported number of contacts at home
- model_rob_nothome.rds --> GAMLSS model for those living healthcare facilities for the reported number of contacts at not home
- model_inside_home --> GAMLSS model for the reported number of contacts at not home
- model_outside_home --> GAMLSS model for the reported number of contacts at not home
- dummy_part_cont_datasets.rds --> the dummy dataset obtained from 500 parametric bootstrap (obtained from create_dummy_cm function)
- boot_scenario_rates_full/proportionate/uniform.rds --> the contact rates from different assumptions based on dummy_part_cont_datasets.rds
- matrix_decompose_scenario_rates_full/proportionate/uniform.rds --> the average contact rates from the original dataset
- matrix_decompose_scenario_crude_full/proportionate/uniform.rds --> the average crude number of contact from the original dataset
- be_2010_social_contact_analysis.RData --> social contact matrices for 2010 Belgian contact survey
- proportion_frail_per_age_group.RData --> proportion of frail individuals per age group
- prediction_intervals_bootstrap.rds --> prediction intervals of the average number of contacts based on the GAMLSS model (Figure 1, red dashed lines)
- plot_model_alone_household.rds --> Cleaned data (to be plotted) from the GAMLSS model for the reported number of contacts for those living in alone
- plot_model_households.rds --> Cleaned data (to be plotted) from the GAMLSS model for the reported number of contacts for those living with others
- plot_model_rob.rds --> Cleaned data (to be plotted) from the GAMLSS model for the reported number of contacts for those living in the healthcare facilities
- model_list.rds --> Cleaned data (to be plotted) from the GAMLSS model for the reported number of contacts 

### 3. output

### 4. code

#### 4.0. main.R
The main code to produce all the plots within the manuscript

### 4.1. helper
- deterministic_model_seir_subpop_agg.R
Code for the mathematical compartmental models within the analysis

- format_data_to_zenodo.R
Code to clean the dataset to obtain those uploaded in the zenodo (10.5281/zenodo.14810628)

- function_r0_decompose.R
R functions to decompose contact matrices with different degrees of assortativity based on the frailty levels of the participants and contactees

- functions.R
All the functions needed to run the analysis

- ggplot_functions.R
All the ggplot functions needed to produce outputs

- matrix_decomposition.R
R code to decompose contact matrices with different degrees of assortativity based on the frailty levels of the participants and contactees (with output)

- model_building.R
Scrap of the model building

- model_plot.R
R code to produce model plots for the GAMLSS results.

### 4.2. scm
- scm_chronic.rds
Social contact matrices for those reported having chronic disease(s), for contacts at home and not at home.

- scm_contacts.rds
Social contact matrices for contacts at home and not at home.

- scm_frailty_home.rds
Social contact matrices for different frailty levels, for contacts at home and not at home.

- scm_frailty_nonhh_home.rds
Social contact matrices for contacts with non-household members for those living at home for different frailty levels, for contacts at home and not at home.

- scm_frailty_nonhh_rob.rds
Social contact matrices for contacts with non-household members for those living at nursing homes for different frailty levels, for contacts at home and not at home.

- scm_frailty_one.rds
Social contact matrices for those living alone by different frailty levels, for contacts at home and not at home.

- scm_frailty_othershh.rds
Social contact matrices for those living with others by different frailty levels, for contacts at home and not at home.

- scm_frailty_rob.rds
Social contact matrices for those living in the healthcare facilities by different frailty levels, for contacts at home and not at home.

- scm_noans_chronic.rds
Social contact matrices for those provided no answer on the question whether they have chronic disease(s), for contacts at home and not at home.

- scm_nonchronic.rds
Social contact matrices for those reported not having chronic disease(s), for contacts at home and not at home.

 -------------------
| Contributors	    |
 -------------------
| Neilshan Loedy    |
| Lisa Hermans	    |
| Maikel Bosschaert |
| Andrea Torneri    |
| Niel Hens.        |
 -------------------
