###############################################################
# File for formatting the Epicurus data   
# For Zenodo. Works from the aggregated data in full form and just selects 
# some columns. It does not do any cleaning or formatting, apart from aggregation
# of some variables.
# Created by Neilshan Loedy on 15/01/2025
# -----------------------------------------------------------

### Cleaning 
rm(list=ls(all=TRUE))

options(scipen=5)
mydata <- file.path(paste0(dir, "/../../data/Data_clean_30062023/"))
### Automatically set working directory
if(require(rstudioapi) && isAvailable()){
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
}

source("functions.R")

library(data.table)
library(stringr)
library(dplyr) # for "mutate"
library(readr)
`%notin%` <- function(a,b) ! a %in% b 

# load the cleaned data
# --------------------------

# Create a vector of weekdays from Sunday to Saturday
weekdays_vector <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# input data
participant_data <- read.csv(paste0(mydata, "to_analyze_participant.csv"))[,-1]
contact_data <- read.csv(paste0(mydata, "to_analyze_contact.csv"))[,-1]

participant_data <- set_participant_factor(set_data_structure(participant_data))
contact_data <- set_contact_factor(set_data_structure(contact_data))

participant_consent <- participant_data %>% 
  filter(!consent %in% "No consent") %>% 
  select(-c(survey_score))

contact_consent <- contact_data[contact_data$part_id %in% participant_consent$part_id,]

# ------------------------------------
# Loedy_Epicurus_participant_common.csv
# ------------------------------------
part_common <- participant_consent[ ,c("part_id", "age_group", "gender")]
names(part_common)[names(part_common) == 'age_group'] <- 'part_age'
names(part_common)[names(part_common) == 'gender'] <- 'part_gender'

write.csv(part_common, "../../../data/Zenodo/LOEDY_Epicurus_participant_common.csv",
          row.names = F)

# ------------------------------------
# Loedy_Epicurus_participant_extra.csv
# ------------------------------------
part_extra <- participant_consent[ ,c("wave_season", "part_id", "household_size", "sample_source", "sample_method",
                                      "help_required", "education", "occupation", "covid_vaccine",
                                      "flu_vaccine", "chronic_status", "ili_status", "resident_status_rlvl",
                                      "wzc_resident_info",
                                      "Q1", "Q11A", "Q11B", "Q11C", "Q11D", "Q2",
                                      "Q3A", "Q3B", "Q3C", "Q3D", "Q3E",
                                      "Q3F", "Q3G", "Q3H", "Q3I", "Q3J",
                                      "Q9A", "Q9B", "Q9C", "Q9D", "Q9E",
                                      "Q9F", "Q9G", "Q9H", "Q9I",
                                      "EQ5D_MOB", "EQ5D_ANX", "EQ5D_SELF", "EQ5D_ACT", "EQ5D_DIS",
                                      "MH01", "MH02", "MH03", "MH04", "MH05", "MH06",
                                      "MH07", "MH08", "MH09", "MH10", "MH11", "MH12")]
names(part_extra)[names(part_extra) == 'household_size'] <- 'hh_size'
names(part_extra)[names(part_extra) == 'resident_status_rlvl'] <- 'resident_status'
names(part_extra)[names(part_extra) == 'wave_season'] <- 'wave'
names(part_extra)[names(part_extra) == 'wzc_resident_info'] <- 'lhf_info'

write.csv(part_extra, "../../../data/Zenodo/LOEDY_Epicurus_participant_extra.csv",
          row.names = F)

# ------------------------------------
# Loedy_Epicurus_contact_common.csv
# ------------------------------------

# create cont_id
# Create a numeric part_id indicator
contact_consent <- contact_consent %>%
  mutate(part_id_numeric = as.numeric(factor(part_id)))

# Add a row number per part_id
contact_consent <- contact_consent %>%
  group_by(part_id) %>%
  mutate(row_number = row_number())

# Combine part_id_numeric and row_number to create cont_id
contact_consent <- contact_consent %>%
  ungroup() %>%
  mutate(cnt_id = sprintf("%d%02d", part_id_numeric, row_number))

contact_consent <- contact_consent %>% 
  mutate(cnt_age = case_when(
    cnt_age %in% seq(0, 9) ~ "0-9",
    cnt_age %in% seq(10, 19) ~ "10-19",
    cnt_age %in% seq(20, 29) ~ "20-29",
    cnt_age %in% seq(30, 39) ~ "30-39",
    cnt_age %in% seq(40, 49) ~ "40-49",
    cnt_age %in% seq(50, 59) ~ "50-59",
    cnt_age %in% seq(60, 69) ~ "60-69",
    cnt_age %in% seq(70, 79) ~ "70-79",
    cnt_age %in% seq(80, 89) ~ "80-89",
    cnt_age %in% seq(90, 110) ~ "90-100",
    is.na(cnt_age) ~ "No answer",  # Replace NA with "No answer"
    TRUE ~ as.character(cnt_age)   # Keeps the original value if it doesn't fit any range
  ))

contact_consent <- contact_consent %>% 
  mutate(cnt_age = as.factor(cnt_age)) %>% 
  mutate(
  cnt_age = fct_relevel(cnt_age,
                        c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                          "60-69", "70-79", "80-89", "90-100")))

contact_consent <- contact_consent %>% 
  dplyr::mutate(
    cnt_est_min = as.numeric(sub("^(\\d+)-.*", "\\1", cnt_age)),
    cnt_est_max = as.numeric(sub(".*-(\\d+)$", "\\1", cnt_age)))

cnt_common <- contact_consent[, c("wave_season", "part_id", "cnt_id", "cnt_age", "cnt_est_min", "cnt_est_max", "cnt_gender", "cnt_day",
                                  "cnt_weekend", "cnt_role", "is_physical", "cnt_home", 
                                  "cnt_work", "cnt_school", "cnt_transport", "cnt_leisure", "cnt_others",
                                  "frequency_multi", "cnt_duration", "is_facemask")]
names(cnt_common)[names(cnt_common) == 'wave_season'] <- 'wave'

write.csv(cnt_common, "../../../data/Zenodo/LOEDY_Epicurus_contact_common.csv",
          row.names = F)

# ------------------------------------
# Loedy_Epicurus_sday.csv
# ------------------------------------

sday <- participant_consent[ ,c("part_id", "sday_date", "wave", "is_holiday")]

write.csv(sday, "../../../data/Zenodo/LOEDY_Epicurus_sday.csv",
          row.names = F)



