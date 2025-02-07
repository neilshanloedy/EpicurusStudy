rm(list = ls())

# library
#----------
library(rstudioapi)
library(xtable)
library(ggplot2)
library(stringr)
library(gamlss)
library(lubridate)
library(tidyr)
library(dplyr)
library(forcats)
library(survival)
library(gamlss.cens)
library(car)
library(tibble)
library(grid)
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(dir))
options(scipen=5)
gen.cens(NBI, type = "right")
gen.cens(PO, type = "right")
mydata <- file.path(paste0(dir, "/../../data/Zenodo/"))
source("helper/functions.R")
source("helper/ggplot_functions.R")
theme_set(theme_classic(base_size = 16))
set.seed(123)
dodge <- position_dodge(width = 0.4)

# Create a vector of weekdays from Sunday to Saturday
weekdays_vector <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# input data
part_data <- read.csv(paste0(mydata, "LOEDY_Epicurus_participant_common.csv"))
part_comm <- read.csv(paste0(mydata, "LOEDY_Epicurus_participant_extra.csv"))
contact_data <- read.csv(paste0(mydata, "LOEDY_Epicurus_contact_common.csv"))
sday <- read.csv(paste0(mydata, "LOEDY_Epicurus_sday.csv"))

sday <- sday %>%
  mutate(
    sday_date = ymd(sday_date),      # Convert to Date format
    year = year(sday_date)          # Extract the year
  )
sday$year <- ifelse(is.na(sday$year), "No answer", sday$year)
participant_data <- merge(merge(part_data, sday[,-which(names(sday) == "wave")], by = c("part_id")), part_comm)

# ---------

participant_data <- set_participant_factor(set_data_structure(participant_data))
contact_data <- set_contact_factor(set_data_structure(contact_data))

# ---------

if(!file.exists("../../data/frailty_levels.csv")) {
  participant_data <- data_with_frailty_index(participant_data)
} else {
  frailty_levels <- read.csv("../../data/frailty_levels.csv")[,-1]
  participant_data <- merge(participant_data, frailty_levels)
  participant_data <- participant_data %>% 
    dplyr::select(-matches("^EQ5D|^Q|^MH"))
  
  participant_data$frailty_score <- factor(participant_data$frailty_score,
                                           levels = c("frail", "pre-frail", "non-frail", "missing"))
  participant_data$is_holiday <- factor(participant_data$is_holiday,
                                           levels = c("Yes", "No", "No answer"))
}

participant_data %>% 
  filter(sample_method == "PAPER") %>%  # Filter early to simplify the pipeline
  mutate(age_regroup = ifelse(part_age %in% c("60-69", "70-79", "80-89", "90-100"), "older", "younger")) %>% 
  group_by(age_regroup) %>% 
  summarise(n = n()) %>%  # Summarize within each age_regroup
  mutate(pctg = n / sum(n)) %>%  # Calculate percentages
  ungroup()  # Ungroup to avoid unintended side effects

participant_data %>% 
  filter(sample_method %in% c("CAWI", "APP")) %>%  # Filter early to simplify the pipeline
  mutate(age_regroup = ifelse(part_age %in% c("60-69", "70-79", "80-89", "90-100"), "older", "younger")) %>% 
  group_by(age_regroup) %>% 
  summarise(n = n()) %>%  # Summarize within each age_regroup
  mutate(pctg = n / sum(n)) %>%  # Calculate percentages
  ungroup()  # Ungroup to avoid unintended side effects

table(participant_data$part_age, participant_data$sample_method)
round(table(participant_data$part_age, participant_data$sample_method)/rowSums(table(participant_data$part_age, participant_data$sample_method)) * 100, 2)
table(participant_data$part_age)/sum(table(participant_data$part_age)) * 100
table(participant_data$education)/sum(table(participant_data$education)) * 100
table(participant_data$part_age, participant_data$sample_method)/sum(table(participant_data$part_age)) * 100
table(participant_data$sample_source, participant_data$sample_method)

# -------------------
# To produce GAMLSS output in the appendix
# source(helper/model_plot.R)

# Figure 1
# load data for contacts not at home
data_out <- readRDS("../rds/data_outside_home.rds")
model_outside <- readRDS("../rds/model_outside_home.rds")
model_inside <- readRDS("../rds/model_outside_home.rds")

output_bp <- ggplot_average_cnt(data_out = data_out)

ggsave("../output/boxplot_contacts_epicurus_frail.png",
       width = 3800, height = 1500, units = "px")

# GAMLSS model for inside and outside home
# -----------------------------------------

if(!file.exists("../rds/model_list.rds")) {
  model.list <- c("model_outside", "model_inside")
  list.model <- list()
  for(i in model.list){
    temp <- get(i)
    temp_summary <- data.frame(summary(temp))
    list.model[[i]] <- temp_summary[1:nrow(temp_summary)-1,] %>%
      mutate(est = exp(Estimate),
             xmin = exp((Estimate) - 1.96 * Std..Error),
             xmax = exp((Estimate) + 1.96 * Std..Error))
  }
  
  model.print <- cbind(
    data.frame(rownames_to_column(data.frame(list.model[[1]]), "Covariates")) %>% 
      mutate(sig = ifelse(Pr...t.. < 0.05, "*", "")) %>% 
      mutate(est = paste0(format(round(est,3), nsmall = 3), sig, " ", "[", format(round(xmin, 3), nsmall = 3),
                          ";", format(round(xmax, 3), nsmall = 3), "]"))  %>% 
      select(Covariates, est),
    data.frame(rownames_to_column(data.frame(list.model[[2]]), "Covariates")) %>% 
      mutate(sig = ifelse(Pr...t.. < 0.05, "*", "")) %>% 
      mutate(est_home = paste0(format(round(est,3), nsmall = 3), sig, " ", "[", format(round(xmin, 3), nsmall = 3),
                               ";", format(round(xmax, 3), nsmall = 3), "]"))  %>% 
      select(est_home))
  
  rownames(model.print) <- NULL
  saveRDS(list.model, "../rds/model_list.rds")
  list.model.short <- lapply(list.model, function(x) x[1:(which(rownames(x) == "X.Intercept..1") - 1), c("xmin", "est", "xmax")])
  list.model.short <- lapply(list.model.short, function(x) x[rownames(x) != "X.Intercept.",])
  list.model.short <- lapply(list.model.short, function(x) {
    x$Covariates <- rownames(x)  # Create a new column 'Covariates' with rownames as values
    rownames(x) <- NULL  # Remove the rownames from the data frame
    return(x)
  })
  
} else {
  list.model <- readRDS("../rds/model_list.rds")
  list.model.short <- lapply(list.model, function(x) x[1:(which(rownames(x) == "X.Intercept..1") - 1), c("xmin", "est", "xmax")])
  list.model.short <- lapply(list.model.short, function(x) x[rownames(x) != "X.Intercept.",])
  list.model.short <- lapply(list.model.short, function(x) {
    x$Covariates <- rownames(x)  # Create a new column 'Covariates' with rownames as values
    rownames(x) <- NULL  # Remove the rownames from the data frame
    return(x)
  })
  
}

output_gamlss <- f_plot_gamlss(data = list.model.short)

file_name = paste("../output/gamlss_result.png", sep="")
png(file_name, width = 30, height = 35, units = "cm", res = 250)
print(output_gamlss)
dev.off()

# GAMLSS plots
# -----------
source("helper/model_plot.R")

# Social contact matrices
# --------------------------
# Social contact matrices for contacts at home and not at home
scm_contacts <- readRDS("scm/scm_contacts.rds")
scm_chronic <- readRDS("scm/scm_chronic.rds")
scm_nonchronic <- readRDS("scm/scm_nonchronic.rds")
scm_noans_chronic <- readRDS("scm/scm_noans_chronic.rds")

# Get a list of all objects in the environment
all_objects <- ls()

# Use grep to filter objects based on naming pattern
matching_objects <- grep("^scm", all_objects, value = TRUE)

# Create a list of the matching objects
matching_lists <- lapply(matching_objects, get)

# plot all the SCM
#------------------
legendBrks=seq(from = 0,
               to = max(do.call("rbind", lapply(matching_lists, function(x) do.call("rbind", x)))$mean, na.rm = T)*2,
               by = 2)

legendLabels=seq(from = 0,
                 to = max(do.call("rbind", lapply(matching_lists, function(x) do.call("rbind", x)))$mean, na.rm = T)*2,
                 by = 2)

Ncolors=length(legendBrks)
color_scm_min <- min(do.call("rbind", lapply(matching_lists, function(x) do.call("rbind", x)))$mean, na.rm = T)
color_scm_max <- log(max(do.call("rbind", lapply(matching_lists, function(x) do.call("rbind", x)))$mean, na.rm = T))*1.55

# additional plots
# ------------------
plot_home <- lapply(matching_lists, function(x) fplot.scm(dum_ci = x$home,
                                                          color_scm_min =  color_scm_min,
                                                          color_scm_max = color_scm_max,
                                                          legendBrks = legendBrks,
                                                          legendLabels = legendLabels))

plot_nothome <- lapply(matching_lists, function(x) fplot.scm(dum_ci = x$not_home,
                                                             color_scm_min =  color_scm_min,
                                                             color_scm_max = color_scm_max,
                                                             legendBrks = legendBrks,
                                                             legendLabels = legendLabels))

# plots
# -----------------
scm_frailty_home <- readRDS("scm/scm_frailty_home.rds")
figure_output <- f_plot_frailty_home(data = scm_frailty_home)

if(!file.exists("../output/all_scm_frail.png")) {
  file_name = paste("../output/all_scm_frail.png", sep="")
  png(file_name, width = 52.5, height = 57, units = "cm", res = 250)
  print(figure_output)
  dev.off()
}

# additional plots for SCM
# -----------------
scm_nonhh_frailty_home <- readRDS("scm/scm_frailty_nonhh_home.rds")
figure_nonhh_frailty_home <- f_plot_nonhh_frailty_home(data = scm_nonhh_frailty_home)

if(!file.exists("../output/all_scm_nonhh_frail_home.png")) {
  file_name = paste("../output/all_scm_nonhh_frail_home.png", sep="")
  png(file_name, width = 52.5, height = 57, units = "cm", res = 250)
  print(figure_nonhh_frailty_home)
  dev.off()
}

# Contacts with household and non-household members inside healthcare facilities
scm_nonhh_frailty_rob <- readRDS("scm/scm_frailty_nonhh_rob.rds")
figure_nonhh_frailty_rob <- f_plot_nonhh_frailty_rob(data = scm_nonhh_frailty_rob)

if(!file.exists("../output/all_scm_rob_frail.png")) {
  file_name = paste("../output/all_scm_rob_frail.png", sep="")
  png(file_name, width = 52.5, height = 35, units = "cm", res = 250)
  print(figure_nonhh_frailty_rob)
  dev.off()
}

scm_frailty_onehh <- readRDS("scm/scm_frailty_one.rds")
figure_frailty_onehh <- f_plot_nonhh_frailty_onehh(data = scm_frailty_onehh)

if(!file.exists("../output/all_scm_community_frail_one.png")) {
  file_name = paste("../output/all_scm_community_frail_one.png", sep="")
  png(file_name, width = 52.5, height = 57, units = "cm", res = 250)
  print(figure_frailty_onehh)
  dev.off()
}

scm_frailty_othershh <- readRDS("scm/scm_frailty_othershh.rds")
figure_frailty_others <- f_plot_nonhh_frailty_othershh(data = scm_frailty_othershh)

if(!file.exists("../output/all_scm_community_frail_others.png")) {
  file_name = paste("../output/all_scm_community_frail_others.png", sep="")
  png(file_name, width = 52.5, height = 57, units = "cm", res = 250)
  print(figure_frailty_others)
  dev.off()
}

scm_frailty_rob <- readRDS("scm/scm_frailty_rob.rds")
figure_nonhh_frailty_rob <- f_plot_frailty_othershh(data = scm_frailty_rob)

if(!file.exists("../output/all_scm_community_frail_rob.png")) {
  file_name = paste("../output/all_scm_community_frail_rob.png", sep="")
  png(file_name, width = 52.5, height = 35, units = "cm", res = 250)
  print(figure_nonhh_frailty_rob)
  dev.off()
}

# -----------------
# matrix decomposition
# -----------------

# the code for decomposing matrix can be found at
# helper/matrix_decomposition.R

# example of using the function
# dec_out_frail <- decompose_contact(participants = participants_decompose,
#                                    contacts = contact_consent,
#                                    age_input = c(0, 50, 60, 70, 80, 90, 101),
#                                    frailty_i = "frail",
#                                    assortativity_frail = 0.333,
#                                    assortativity_prefrail = 0.333,
#                                    per_capita = T)

# -----------------
# Mathematical models presented in the manuscript
# can be found at helper/deterministic_model_seir_subpop_agerates.R
# -----------------
# load dummy dataset for developing the CI for mathematical models
n_boot = 500
if(!file.exists("../rds/matrix_decomposition/dummy_part_cont_dataset.rds")) {
  boot_dummy_cm <- create_dummy_cm(input_part = participants,
                                   input_cont = contact,
                                   nboot = n_boot)
  saveRDS(boot_dummy_cm, "../rds/matrix_decomposition/dummy_part_cont_dataset.rds")
} else {
  boot_dummy_cm <- readRDS("../rds/matrix_decomposition/dummy_part_cont_dataset.rds")
}

source("helper/deterministic_model_seir_subpop_agerates.rds")

# ----------------
scenario <- c("proportionate", "full", "uniform")
age_range <- c("[0-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)", "90+")

for(i in 1:length(scenario)){
  temp <- env()
  load(sprintf("../rds/matrix_decomposition/matrix_decompose_scenario_crude_%s.RData", scenario[i]),
       envir = temp)
  output_decompose <- decompose_matrix_output(data = temp$output_decomposition)
  
  file_name = paste("../output/decompose_matrix_", scenario[i], ".png", sep="")
  png(file_name, width = 40, height = 30, units = "cm", res = 250)
  print(output_decompose)
  dev.off()
}