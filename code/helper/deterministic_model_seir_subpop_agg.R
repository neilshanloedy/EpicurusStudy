# rm(list = ls())
library (deSolve) 
library (MASS)
library (rstudioapi)
library (magic)
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(dir))
source("function_r0_decompose.R")

#--- load data
n_boot = 500
population_vector <- c(6973796, 1590628, 1347139, 924291, 539390, 117397)
load("../rds/matrix_decomposition/proportion_frail_per_age_group.RData")
output_proportions <- output_proportion[[2]]

pop_frail <- population_vector * output_proportions[,1]
mat_frail <- matrix(rep(pop_frail, 6), nrow = 6, ncol = 6, byrow = T)
pop_prefrail <- population_vector * output_proportions[,2]
mat_prefrail <- matrix(rep(pop_prefrail, 6), nrow = 6, ncol = 6, byrow = T)
pop_nonfrail <- population_vector * output_proportions[,3]
mat_nonfrail <- matrix(rep(pop_nonfrail, 6), nrow = 6, ncol = 6, byrow = T)

initial_incidence <- c(11716, 1119, 466, 372, 264, 101)
init_frail <- initial_incidence * output_proportions[,1]
init_prefrail <- initial_incidence * output_proportions[,2]
init_nonfrail <- initial_incidence * output_proportions[,3]

#---
initial_values = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                    S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                    S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                    # Exposed
                    E1_f = init_frail[1], E2_f = init_frail[2], E3_f = init_frail[3], E4_f = init_frail[4], E5_f = init_frail[5], E6_f = init_frail[6],
                    E1_p = init_prefrail[1], E2_p = init_prefrail[2], E3_p = init_prefrail[3], E4_p = init_prefrail[4], E5_p = init_prefrail[5], E6_p = init_prefrail[6],
                    E1_n = init_nonfrail[1], E2_n = init_nonfrail[2], E3_n = init_nonfrail[3], E4_n = init_nonfrail[4], E5_n = init_nonfrail[5], E6_n = init_nonfrail[6],
                    # I pre-symptomatic
                    I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                    I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                    I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                    # I asymptomatic
                    I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                    I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                    I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                    # I mild
                    I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                    I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                    I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                    # I severe
                    I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                    I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                    I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                    # I hospitalised
                    I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                    I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                    I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                    # I I0U
                    I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                    I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                    I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                    # I recovered
                    R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                    R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                    R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)

# frail individuals
initial_values_frail = c (S1 = pop_frail [1], S2 = pop_frail [2], S1 = pop_frail [3], S2 = pop_frail [4], S5 = pop_frail [5], S6 = pop_frail [6],
                          E1 = init_frail [1], E2 = init_frail [2], E1 = init_frail [3], E2 = init_frail [4], E5 = init_frail [5], E6 = init_frail [6],
                          I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                          I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                          I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                          I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                          I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                          I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                          R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)

# pre-frail individuals
initial_values_prefrail = c (S1 = pop_prefrail [1], S2 = pop_prefrail [2], S1 = pop_prefrail [3], S2 = pop_prefrail [4], S5 = pop_prefrail [5], S6 = pop_prefrail [6],
                             E1 = init_prefrail [1], E2 = init_prefrail [2], E1 = init_prefrail [3], E2 = init_prefrail [4], E5 = init_prefrail [5], E6 = init_prefrail [6],
                             I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                             I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                             I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                             I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                             I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                             I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                             R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)

# non-frail individuals
initial_values_nonfrail = c (S1 = pop_nonfrail [1], S2 = pop_nonfrail [2], S1 = pop_nonfrail [3], S2 = pop_nonfrail [4], S5 = pop_nonfrail [5], S6 = pop_nonfrail [6],
                             E1 = init_nonfrail [1], E2 = init_nonfrail [2], E1 = init_nonfrail [3], E2 = init_nonfrail [4], E5 = init_nonfrail [5], E6 = init_nonfrail [6],
                             I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                             I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                             I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                             I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                             I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                             I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                             R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)

# input model
#-------------
mu_sev = 0.4736 # Willem et al Table S4 ; old value: 0.4736 -- in paper: 0.276
p_value = 0.1293 #  Abrams et al B2; old value: 0.1293 -- in paper: 0.407
delta_1_value = 0.423 # Willem et al Table S3; old value: 0.423 -- in paper : 0.454
delta_2_value = 0.155 # Willem et al Table S3: old value: 0.155 -- in paper : 0.222
delta_3_value = (1-mu_sev) * 0.082 # correct --> Willem et al (12 days)
delta_4_value = mu_sev * 0.082 # correct --> Willem et al (12 days)
psi_value = 0.290 # mild to severe from Abrams et al (3.5 days) --> Table B3
omega_value = 0.5976 
tau_1_value = 0.0014
tau_2_value = 0.0014
phi_1_value = 0.8801

# number of contacts per day
infectious_period = 2.896                 # correct from Abrams et al (main) infectious period; old = 2.896 -- in paper: 3.481
latent_period = 1.487                     # correct from Abrams et al (main) latent period: old = 1.487 -- in paper: 6.284
theta_value = 1 / infectious_period
gamma_value = 1 / latent_period
q_value = 0.051

timepoints = seq (0, 200, by=1)
# run the model
#------------------
# proportionate mixing

if(!file.exists("../rds/output_compartment/sir_result_sameq_withci.rds")) {

  load("../rds/matrix_decomposition/matrix_decompose_scenario_rates_proportionate.RData")
  matrix_proportionate <- output_decomposition
  
  prop_frail_frail <- matrix_proportionate$frail_output$frail_frail_matrix
  prop_frail_prefrail <- matrix_proportionate$frail_output$frail_prefrail_matrix
  prop_frail_nonfrail <- matrix_proportionate$frail_output$frail_nonfrail_matrix
  
  prop_prefrail_frail <- matrix_proportionate$prefrail_output$prefrail_frail_matrix
  prop_prefrail_prefrail <- matrix_proportionate$prefrail_output$prefrail_prefrail_matrix
  prop_prefrail_nonfrail <- matrix_proportionate$prefrail_output$prefrail_nonfrail_matrix
  
  prop_nonfrail_frail <- matrix_proportionate$nonfrail_output$nonfrail_frail_matrix
  prop_nonfrail_prefrail <- matrix_proportionate$nonfrail_output$nonfrail_prefrail_matrix
  prop_nonfrail_nonfrail <- matrix_proportionate$nonfrail_output$nonfrail_nonfrail_matrix
  
  R0_prop <- R0_sub_full_agestr_mixed(transmission_val = 0.051,
                                      betaff_val = prop_frail_frail,
                                      betann_val = prop_nonfrail_nonfrail,
                                      betapp_val = prop_prefrail_prefrail,
                                      betanf_val = prop_nonfrail_frail,
                                      betafn_val = prop_frail_nonfrail,
                                      betafp_val = prop_frail_prefrail,
                                      betapf_val = prop_prefrail_frail,
                                      betapn_val = prop_prefrail_nonfrail,
                                      betanp_val = prop_nonfrail_prefrail)
  
  parameter_list_prop = c (beta_f_f = prop_frail_frail, 
                           beta_n_n = prop_nonfrail_nonfrail, 
                           beta_p_p = prop_prefrail_prefrail,
                           beta_n_f = prop_nonfrail_frail,
                           beta_f_n = prop_frail_nonfrail,
                           beta_f_p = prop_frail_prefrail, 
                           beta_p_f = prop_prefrail_frail,
                           beta_p_n = prop_prefrail_nonfrail,
                           beta_n_p = prop_nonfrail_prefrail,
                           theta = theta_value, gamma = gamma_value,
                           p = p_value, delta_1 = delta_1_value, psi = psi_value,
                           delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                           delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                           tau_2 = tau_2_value,
                           N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                           q = q_value)
  
  output_prop = ode (initial_values, timepoints, seir_model_agestr, parameter_list_prop)
  
  #------------------
  # uniform mixing
  
  load("../rds/matrix_decomposition/matrix_decompose_scenario_rates_uniform.RData")
  matrix_uniform <- output_decomposition
  
  uni_frail_frail <- matrix_uniform$frail_output$frail_frail_matrix
  uni_frail_prefrail <- matrix_uniform$frail_output$frail_prefrail_matrix
  uni_frail_nonfrail <- matrix_uniform$frail_output$frail_nonfrail_matrix
  
  uni_prefrail_frail <- matrix_uniform$prefrail_output$prefrail_frail_matrix
  uni_prefrail_prefrail <- matrix_uniform$prefrail_output$prefrail_prefrail_matrix
  uni_prefrail_nonfrail <- matrix_uniform$prefrail_output$prefrail_nonfrail_matrix
  
  uni_nonfrail_frail <- matrix_uniform$nonfrail_output$nonfrail_frail_matrix
  uni_nonfrail_prefrail <- matrix_uniform$nonfrail_output$nonfrail_prefrail_matrix
  uni_nonfrail_nonfrail <- matrix_uniform$nonfrail_output$nonfrail_nonfrail_matrix
  
  R0_uni <- R0_sub_full_agestr_mixed(transmission_val = 0.051,
                                     betaff_val = uni_frail_frail,
                                     betann_val = uni_nonfrail_nonfrail,
                                     betapp_val = uni_prefrail_prefrail,
                                     betanf_val = uni_nonfrail_frail,
                                     betafn_val = uni_frail_nonfrail,
                                     betafp_val = uni_frail_prefrail,
                                     betapf_val = uni_prefrail_frail,
                                     betapn_val = uni_prefrail_nonfrail,
                                     betanp_val = uni_nonfrail_prefrail)
  
  parameter_list_uni = c (beta_f_f = uni_frail_frail, 
                          beta_n_n = uni_nonfrail_nonfrail, 
                          beta_p_p = uni_prefrail_prefrail,
                          beta_n_f = uni_nonfrail_frail,
                          beta_f_n = uni_frail_nonfrail,
                          beta_f_p = uni_frail_prefrail, 
                          beta_p_f = uni_prefrail_frail,
                          beta_p_n = uni_prefrail_nonfrail,
                          beta_n_p = uni_nonfrail_prefrail,
                          theta = theta_value, gamma = gamma_value,
                          p = p_value, delta_1 = delta_1_value, psi = psi_value,
                          delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                          delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                          tau_2 = tau_2_value,
                          N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                          q = q_value)
  
  output_uni = ode (initial_values, timepoints, seir_model_agestr, parameter_list_uni)
  
  #------------------
  # fully assortative mixing
  
  load("../rds/matrix_decomposition/matrix_decompose_scenario_rates_full.RData")
  matrix_isolate <- output_decomposition
  
  matrix_frail_frail <- matrix_isolate$frail_output$frail_frail_matrix
  matrix_frail_prefrail <- matrix_isolate$frail_output$frail_prefrail_matrix
  matrix_frail_nonfrail <- matrix_isolate$frail_output$frail_nonfrail_matrix
  
  matrix_prefrail_frail <- matrix_isolate$prefrail_output$prefrail_frail_matrix
  matrix_prefrail_prefrail <- matrix_isolate$prefrail_output$prefrail_prefrail_matrix
  matrix_prefrail_nonfrail <- matrix_isolate$prefrail_output$prefrail_nonfrail_matrix
  
  matrix_nonfrail_frail <- matrix_isolate$nonfrail_output$nonfrail_frail_matrix
  matrix_nonfrail_prefrail <- matrix_isolate$nonfrail_output$nonfrail_prefrail_matrix
  matrix_nonfrail_nonfrail <- matrix_isolate$nonfrail_output$nonfrail_nonfrail_matrix
  
  # frail individuals
  initial_values_frail = c (S1 = pop_frail [1], S2 = pop_frail [2], S1 = pop_frail [3], S2 = pop_frail [4], S5 = pop_frail [5], S6 = pop_frail [6],
                            E1 = init_frail [1], E2 = init_frail [2], E1 = init_frail [3], E2 = init_frail [4], E5 = init_frail [5], E6 = init_frail [6],
                            I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                            I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                            I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                            I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                            I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                            I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                            R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)
  
  parameter_list_frail_fa = c (beta = matrix_frail_frail,
                               theta = theta_value, gamma = gamma_value,
                               p = p_value, delta_1 = delta_1_value, psi = psi_value,
                               delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                               delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                               tau_2 = tau_2_value, 
                               q = q_value)
  
  output_frail_fa = ode (initial_values_frail, timepoints, seir_model_run, parameter_list_frail_fa)
  
  # pre-frail individuals
  initial_values_prefrail = c (S1 = pop_prefrail [1], S2 = pop_prefrail [2], S1 = pop_prefrail [3], S2 = pop_prefrail [4], S5 = pop_prefrail [5], S6 = pop_prefrail [6],
                            E1 = init_prefrail [1], E2 = init_prefrail [2], E1 = init_prefrail [3], E2 = init_prefrail [4], E5 = init_prefrail [5], E6 = init_prefrail [6],
                            I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                            I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                            I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                            I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                            I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                            I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                            R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)
  
  parameter_list_prefrail_fa = c (beta = matrix_prefrail_prefrail,
                                  theta = theta_value, gamma = gamma_value,
                                  p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                  delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                  delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                  tau_2 = tau_2_value, 
                                  q = q_value)
  
  output_prefrail_fa = ode (initial_values_prefrail, timepoints, seir_model_run, parameter_list_prefrail_fa)
  
  # non-frail individuals
  initial_values_nonfrail = c (S1 = pop_nonfrail [1], S2 = pop_nonfrail [2], S1 = pop_nonfrail [3], S2 = pop_nonfrail [4], S5 = pop_nonfrail [5], S6 = pop_nonfrail [6],
                               E1 = init_nonfrail [1], E2 = init_nonfrail [2], E1 = init_nonfrail [3], E2 = init_nonfrail [4], E5 = init_nonfrail [5], E6 = init_nonfrail [6],
                               I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                               I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                               I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                               I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                               I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                               I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                               R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)
  
  parameter_list_nonfrail_fa = c (beta = matrix_nonfrail_nonfrail,
                                  theta = theta_value, gamma = gamma_value,
                                  p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                  delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                  delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                  tau_2 = tau_2_value, 
                                  q = q_value)
  
  output_nonfrail_fa = ode (initial_values_nonfrail, timepoints, seir_model_run, parameter_list_nonfrail_fa)
  
  # For confidence interval ------- (stratified non-parametric bootstrap)
  # 1. uniform mixing
  boot_uni <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_uniform.rds")
  output_uni_boot <- list()
  
  for(b in 1:n_boot) {
    print(b)
    par = c (beta_f_f = boot_uni$frail_output[[b]]$frail_frail_matrix, 
             beta_n_n = boot_uni$nonfrail_output[[b]]$nonfrail_nonfrail_matrix, 
             beta_p_p = boot_uni$prefrail_output[[b]]$prefrail_prefrail_matrix,
             beta_n_f = boot_uni$nonfrail_output[[b]]$nonfrail_frail_matrix,
             beta_f_n = boot_uni$frail_output[[b]]$frail_nonfrail_matrix,
             beta_f_p = boot_uni$frail_output[[b]]$frail_prefrail_matrix, 
             beta_p_f = boot_uni$prefrail_output[[b]]$prefrail_frail_matrix,
             beta_p_n = boot_uni$prefrail_output[[b]]$prefrail_nonfrail_matrix,
             beta_n_p = boot_uni$nonfrail_output[[b]]$nonfrail_prefrail_matrix,
             theta = theta_value, gamma = gamma_value,
             p = p_value, delta_1 = delta_1_value, psi = psi_value,
             delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
             delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
             tau_2 = tau_2_value,
             N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
             q = q_value)
    
    output_uni_boot[[b]] = ode (initial_values, timepoints, seir_model_agestr, par)
    
  }
  
  uni_nonfrail <- lapply(output_uni_boot, function(x) rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T))
  uni_prefrail <- lapply(output_uni_boot, function(x) rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T))
  uni_frail <- lapply(output_uni_boot, function(x) rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T))
  
  ttp_uni_nonfrail <- lapply(output_uni_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T)))
  ttp_uni_prefrail <- lapply(output_uni_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T)))
  ttp_uni_frail <- lapply(output_uni_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T)))
  
  rec_uni <- quantile(sapply(output_uni_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                       probs = c(0.025, 0.975), na.rm = T)

  # Combine the lists into a matrix where each column corresponds to one list
  ci_uni_nonfrail <- apply(
    do.call(cbind, uni_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_uni_prefrail <- apply(
    do.call(cbind, uni_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_uni_frail <- apply(
    do.call(cbind, uni_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  ttpci_uni_nonfrail <- apply(
    do.call(cbind, ttp_uni_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_uni_prefrail <- apply(
    do.call(cbind, ttp_uni_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_uni_frail <- apply(
    do.call(cbind, ttp_uni_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  R0_uni_list <- list()
  for(b in 1:n_boot) {
    print(b)
    
    result <- 
      try({
        as.numeric(R0_sub_full_agestr_mixed(transmission_val = 0.051,
                                                  betaff_val = boot_uni$frail_output[[b]]$frail_frail_matrix,
                                                  betann_val = boot_uni$nonfrail_output[[b]]$nonfrail_nonfrail_matrix,
                                                  betapp_val = boot_uni$prefrail_output[[b]]$prefrail_prefrail_matrix,
                                                  betanf_val = boot_uni$nonfrail_output[[b]]$nonfrail_frail_matrix,
                                                  betafn_val = boot_uni$frail_output[[b]]$frail_nonfrail_matrix,
                                                  betafp_val = boot_uni$frail_output[[b]]$frail_prefrail_matrix,
                                                  betapf_val = boot_uni$prefrail_output[[b]]$prefrail_frail_matrix,
                                                  betapn_val = boot_uni$prefrail_output[[b]]$prefrail_nonfrail_matrix,
                                                  betanp_val = boot_uni$nonfrail_output[[b]]$nonfrail_prefrail_matrix))
      }, silent = T)
    # Assign result to R0_uni_list or NA if error
    R0_uni_list[[b]] <- if (inherits(result, "try-error")) NA else result
  }
  
  mean(unlist(R0_uni_list), na.rm = T)
  quantile(unlist(R0_uni_list), 0.025, na.rm = T)
  quantile(unlist(R0_uni_list), 0.975, na.rm = T)
  
  # 2. proportionate mixing
  boot_prop <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_proportionate.rds")
  output_prop_boot <- list()
  
  for(b in 1:n_boot) {
    print(b)
    par = c (beta_f_f = boot_prop$frail_output[[b]]$frail_frail_matrix, 
             beta_n_n = boot_prop$nonfrail_output[[b]]$nonfrail_nonfrail_matrix, 
             beta_p_p = boot_prop$prefrail_output[[b]]$prefrail_prefrail_matrix,
             beta_n_f = boot_prop$nonfrail_output[[b]]$nonfrail_frail_matrix,
             beta_f_n = boot_prop$frail_output[[b]]$frail_nonfrail_matrix,
             beta_f_p = boot_prop$frail_output[[b]]$frail_prefrail_matrix, 
             beta_p_f = boot_prop$prefrail_output[[b]]$prefrail_frail_matrix,
             beta_p_n = boot_prop$prefrail_output[[b]]$prefrail_nonfrail_matrix,
             beta_n_p = boot_prop$nonfrail_output[[b]]$nonfrail_prefrail_matrix,
             theta = theta_value, gamma = gamma_value,
             p = p_value, delta_1 = delta_1_value, psi = psi_value,
             delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
             delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
             tau_2 = tau_2_value,
             N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
             q = q_value)
    
    output_prop_boot[[b]] = ode (initial_values, timepoints, seir_model_agestr, par)
    
  }
  
  prop_nonfrail <- lapply(output_prop_boot, function(x) rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T))
  prop_prefrail <- lapply(output_prop_boot, function(x) rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T))
  prop_frail <- lapply(output_prop_boot, function(x) rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T))
  
  ttp_prop_nonfrail <- lapply(output_prop_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T)))
  ttp_prop_prefrail <- lapply(output_prop_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T)))
  ttp_prop_frail <- lapply(output_prop_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T)))
  
  rec_prop <- quantile(sapply(output_prop_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                       probs = c(0.025, 0.975), na.rm = T)
  
  # Combine the lists into a matrix where each column corresponds to one list
  ci_prop_nonfrail <- apply(
    do.call(cbind, prop_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_prop_prefrail <- apply(
    do.call(cbind, prop_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_prop_frail <- apply(
    do.call(cbind, prop_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  ttpci_prop_nonfrail <- apply(
    do.call(cbind, ttp_prop_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_prop_prefrail <- apply(
    do.call(cbind, ttp_prop_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_prop_frail <- apply(
    do.call(cbind, ttp_prop_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  R0_prop_list <- list()
  for(b in 1:n_boot) {
    print(b)
    
    result <- 
      try({
        as.numeric(R0_sub_full_agestr_mixed(transmission_val = 0.051,
                                            betaff_val = boot_prop$frail_output[[b]]$frail_frail_matrix,
                                            betann_val = boot_prop$nonfrail_output[[b]]$nonfrail_nonfrail_matrix,
                                            betapp_val = boot_prop$prefrail_output[[b]]$prefrail_prefrail_matrix,
                                            betanf_val = boot_prop$nonfrail_output[[b]]$nonfrail_frail_matrix,
                                            betafn_val = boot_prop$frail_output[[b]]$frail_nonfrail_matrix,
                                            betafp_val = boot_prop$frail_output[[b]]$frail_prefrail_matrix,
                                            betapf_val = boot_prop$prefrail_output[[b]]$prefrail_frail_matrix,
                                            betapn_val = boot_prop$prefrail_output[[b]]$prefrail_nonfrail_matrix,
                                            betanp_val = boot_prop$nonfrail_output[[b]]$nonfrail_prefrail_matrix))
      }, silent = T)
    # Assign result to R0_uni_list or NA if error
    R0_prop_list[[b]] <- if (inherits(result, "try-error")) NA else result
  }
  
  mean(unlist(R0_prop_list), na.rm = T)
  quantile(unlist(R0_prop_list), 0.025, na.rm = T)
  quantile(unlist(R0_prop_list), 0.975, na.rm = T)
  
  # 3. Full assortativity
  boot_fa <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_full.rds")
  output_frail_boot <- list(); output_prefrail_boot <- list(); output_nonfrail_boot <- list()
  
  for(b in 1:n_boot) {
    print(b)
    # frail individuals
    parameter_list_frail_boot = c (beta = boot_fa$frail_output[[b]]$frail_frail_matrix,
                                 theta = theta_value, gamma = gamma_value,
                                 p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                 delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                 delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                 tau_2 = tau_2_value, 
                                 q = q_value)
    
    output_frail_boot[[b]] = ode (initial_values_frail, timepoints, seir_model_run, parameter_list_frail_boot)
    
    # pre-frail individuals
    parameter_list_prefrail_boot = c (beta = boot_fa$prefrail_output[[b]]$prefrail_prefrail_matrix,
                                    theta = theta_value, gamma = gamma_value,
                                    p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                    delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                    delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                    tau_2 = tau_2_value, 
                                    q = q_value)
    
    output_prefrail_boot[[b]] = ode (initial_values_prefrail, timepoints, seir_model_run, parameter_list_prefrail_boot)
    
    # non-frail individuals
    parameter_list_nonfrail_boot = c (beta = boot_fa$nonfrail_output[[b]]$nonfrail_nonfrail_matrix,
                                    theta = theta_value, gamma = gamma_value,
                                    p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                    delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                    delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                    tau_2 = tau_2_value, 
                                    q = q_value)
    
    output_nonfrail_boot[[b]] = ode (initial_values_nonfrail, timepoints, seir_model_run, parameter_list_nonfrail_boot)
    
  }
  
  full_nonfrail <- lapply(output_nonfrail_boot, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  full_prefrail <- lapply(output_prefrail_boot, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  full_frail <- lapply(output_frail_boot, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  
  ttp_full_nonfrail <- lapply(output_nonfrail_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  ttp_full_prefrail <- lapply(output_prefrail_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  ttp_full_frail <- lapply(output_frail_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  
  rec_full_nonfrail <- quantile(sapply(output_nonfrail_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                       probs = c(0.025, 0.975), na.rm = T)
  rec_full_prefrail <- quantile(sapply(output_prefrail_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                       probs = c(0.025, 0.975), na.rm = T)
  rec_full_frail <- quantile(sapply(output_frail_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                       probs = c(0.025, 0.975), na.rm = T)
  
  # Combine the lists into a matrix where each column corresponds to one list
  ci_full_nonfrail <- apply(
    do.call(cbind, full_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_full_prefrail <- apply(
    do.call(cbind, full_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_full_frail <- apply(
    do.call(cbind, full_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  ttpci_full_nonfrail <- apply(
    do.call(cbind, ttp_full_nonfrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_full_prefrail <- apply(
    do.call(cbind, ttp_full_prefrail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_full_frail <- apply(
    do.call(cbind, ttp_full_frail), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  # --- output manuscript
  # ----
  data_sameq <- rbind(
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_nonfrail[1,], 
      case = rowSums(output_prop[, grep("E.*_n.*", colnames(output_prop))]),
      upper_case = ci_prop_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_prefrail[1,], 
      case = rowSums(output_prop[, grep("E.*_p.*", colnames(output_prop))]),
      upper_case = ci_prop_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_frail[1,], 
      case = rowSums(output_prop[, grep("E.*_f.*", colnames(output_prop))]),
      upper_case = ci_prop_frail[2,], 
      type = "Frail",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_nonfrail[1,], 
      case = rowSums(output_uni[, grep("E.*_n.*", colnames(output_uni))]),
      upper_case = ci_uni_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_prefrail[1,], 
      case = rowSums(output_uni[, grep("E.*_p.*", colnames(output_uni))]),
      upper_case = ci_uni_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_frail[1,], 
      case = rowSums(output_uni[, grep("E.*_f.*", colnames(output_uni))]),
      upper_case = ci_uni_frail[2,], 
      type = "Frail",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_nonfrail[1,], 
      case = rowSums(output_nonfrail_fa[, grep("E.*", colnames(output_nonfrail_fa))]),
      upper_case = ci_full_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_prefrail[1,], 
      case = rowSums(output_prefrail_fa[, grep("E.*", colnames(output_prefrail_fa))]),
      upper_case = ci_full_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_frail[1,], 
      case = rowSums(output_frail_fa[, grep("E.*", colnames(output_frail_fa))]),
      upper_case = ci_full_frail[2,], 
      type = "Frail",
      assumption = "Full assortativity"
    )
    
  )
  
  data_sameq$type <- factor(data_sameq$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  data_sameq$assumption <- factor(data_sameq$assumption,
                                  levels = c("Proportionate mixing", "Uniform mixing",
                                             "Full assortativity"))
  
  # For the recovered dataset.
  recovered_sameq <- rbind(
    data.frame(
      low_recov = rec_prop[1], 
      recov = sum(output_prop[nrow(output_prop), grep(paste0("R.*"), colnames(output_prop))]),
      upper_recov = rec_prop[2], 
      type = "All",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      low_recov = rec_uni[1], 
      recov = sum(output_uni[nrow(output_uni), grep(paste0("R.*"), colnames(output_uni))]),
      upper_recov = rec_uni[2], 
      type = "All",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      low_recov = rec_full_nonfrail[1], 
      recov = sum(output_nonfrail_fa[nrow(output_nonfrail_fa), grep(paste0("R.*"), colnames(output_nonfrail_fa))]),
      upper_recov = rec_full_nonfrail[2], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_recov = rec_full_prefrail[1], 
      recov = sum(output_prefrail_fa[nrow(output_prefrail_fa), grep(paste0("R.*"), colnames(output_prefrail_fa))]),
      upper_recov = rec_full_prefrail[2], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_recov = rec_full_frail[1], 
      recov = sum(output_frail_fa[nrow(output_frail_fa), grep(paste0("R.*"), colnames(output_frail_fa))]),
      upper_recov = rec_full_frail[2], 
      type = "Frail",
      assumption = "Full assortativity"
    )
    
  )
  
  recovered_sameq$type <- factor(recovered_sameq$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  recovered_sameq$assumption <- factor(recovered_sameq$assumption,
                                  levels = c("Proportionate mixing", "Uniform mixing",
                                             "Full assortativity"))
  
  ttp_sameq <- rbind(
    data.frame(
      low_case = ttpci_prop_nonfrail[1,], 
      case = which.max(rowSums(output_prop[, grep("E.*_n.*", colnames(output_prop))])),
      upper_case = ttpci_prop_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      low_case = ttpci_prop_prefrail[1,], 
      case = which.max(rowSums(output_prop[, grep("E.*_p.*", colnames(output_prop))])),
      upper_case = ttpci_prop_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      low_case = ttpci_prop_frail[1,], 
      case = which.max(rowSums(output_prop[, grep("E.*_f.*", colnames(output_prop))])),
      upper_case = ttpci_prop_frail[2,], 
      type = "Frail",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      low_case = ttpci_uni_nonfrail[1,], 
      case = which.max(rowSums(output_uni[, grep("E.*_n.*", colnames(output_uni))])),
      upper_case = ttpci_uni_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      low_case = ttpci_uni_prefrail[1,], 
      case = which.max(rowSums(output_uni[, grep("E.*_p.*", colnames(output_uni))])),
      upper_case = ttpci_uni_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      low_case = ttpci_uni_frail[1,], 
      case = which.max(rowSums(output_uni[, grep("E.*_f.*", colnames(output_uni))])),
      upper_case = ttpci_uni_frail[2,], 
      type = "Frail",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      low_case = ttpci_full_nonfrail[1,], 
      case = which.max(rowSums(output_nonfrail_fa[, grep("E.*", colnames(output_nonfrail_fa))])),
      upper_case = ttpci_full_nonfrail[2,], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_case = ttpci_full_prefrail[1,], 
      case = which.max(rowSums(output_prefrail_fa[, grep("E.*", colnames(output_prefrail_fa))])),
      upper_case = ttpci_full_prefrail[2,], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_case = ttpci_full_frail[1,], 
      case = which.max(rowSums(output_frail_fa[, grep("E.*", colnames(output_frail_fa))])),
      upper_case = ttpci_full_frail[2,], 
      type = "Frail",
      assumption = "Full assortativity"
    )
  )
  
  ttp_sameq$type <- factor(ttp_sameq$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  ttp_sameq$assumption <- factor(ttp_sameq$assumption,
                                  levels = c("Proportionate mixing", "Uniform mixing",
                                             "Full assortativity"))
  
  
  saveRDS(data_sameq, "../rds/output_compartment/sir_result_sameq_withci.rds")
  saveRDS(recovered_sameq, "../rds/output_compartment/sir_recovered_sameq_withci.rds")
  saveRDS(ttp_sameq, "../rds/output_compartment/sir_ttp_sameq_withci.rds")
} else {
  data_sameq <- readRDS("../rds/output_compartment/sir_result_sameq_withci.rds")
  recovered_sameq <- readRDS("../rds/output_compartment/sir_recovered_sameq_withci.rds")
  ttp_sameq <- readRDS("../rds/output_compartment/sir_ttp_sameq_withci.rds")
}

ggplot_sameq <- ggplot(data = data_sameq, aes(x = time, y = case / 100000, colour = type, fill = type)) +
  geom_line(size = 1) +  # Increase line size for better visibility
  geom_ribbon(aes(ymin = low_case / 100000, ymax = upper_case / 100000), alpha = 0.1) +
  facet_grid(assumption ~ type) +
  scale_colour_manual(
    values = c(
      "Non-frail" = "darkblue",
      "Pre-frail" = "darkgreen",
      "Frail" = "darkred"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Non-frail" = "darkblue",
      "Pre-frail" = "darkgreen",
      "Frail" = "darkred"
    )
  ) +
  labs(
    x = "Time in days since March 1, 2020",
    y = "Incidence per 100,000",
    colour = NULL,  # Removes legend title
    fill = NULL     # Removes fill legend title
  ) +
  guides(
    colour = guide_legend(override.aes = list(fill = NA)),  # Align colour and fill legends
    fill = "none"  # Suppress separate fill legend
  ) +
  theme(
    text = element_text(size = 18),  # Increases font size
    legend.position = "none",  # Moves legend above the plot
    legend.key = element_blank(),  # Removes legend key background
    panel.background = element_rect(fill = "white"),  # Makes background white
    panel.grid.minor = element_blank()  # Removes minor grid lines
  )

file_name = paste("../output/compartmental_output_sameq_ci.png", sep="")
png(file_name, width = 40, height = 30, units = "cm", res = 250)
print(ggplot_sameq)
dev.off()

# peak cases stop
assumption <- c("Proportionate mixing", "Uniform mixing", "Full assortativity")
type <- c("Non-frail", "Pre-frail", "Frail")

peak_sameq <- list()
count = 1
for(a in assumption) {
  for(t in type) {
    peak_sameq[[count]] <- data_sameq[data_sameq$assumption == a & data_sameq$type == t & 
                                        data_sameq$case == max(data_sameq$case[data_sameq$assumption == a & data_sameq$type == t]), ]
    count = count + 1
  }
  peak_sameq_df <- do.call("rbind", peak_sameq)
  peak_sameq_df$low_case <- round(peak_sameq_df$low_case / 100000, 2)
  peak_sameq_df$case <- round(peak_sameq_df$case / 100000, 2)
  peak_sameq_df$upper_case <- round(peak_sameq_df$upper_case / 100000, 2)
}

# attack rates 
recovered_sameq <- recovered_sameq %>% 
  mutate(lower_ar = ifelse(assumption != "Full assortativity", low_recov/sum(population_vector), 
                           ifelse(assumption == "Full assortativity" & type == "Non-frail", low_recov/sum(pop_nonfrail),
                                  ifelse(assumption == "Full assortativity" & type == "Pre-frail", low_recov/sum(pop_prefrail),
                                         low_recov/sum(pop_nonfrail)))),
         ar = ifelse(assumption != "Full assortativity", recov/sum(population_vector), 
                     ifelse(assumption == "Full assortativity" & type == "Non-frail", recov/sum(pop_nonfrail),
                            ifelse(assumption == "Full assortativity" & type == "Pre-frail", recov/sum(pop_prefrail),
                                   recov/sum(pop_nonfrail)))),
         upper_ar = ifelse(assumption != "Full assortativity", upper_recov/sum(population_vector), 
                           ifelse(assumption == "Full assortativity" & type == "Non-frail", upper_recov/sum(pop_nonfrail),
                                  ifelse(assumption == "Full assortativity" & type == "Pre-frail", upper_recov/sum(pop_prefrail),
                                         upper_recov/sum(pop_nonfrail))))
  )

ar_df <- rbind(
  recovered_sameq[recovered_sameq$assumption != "Full assortativity", c("assumption", "lower_ar", "ar", "upper_ar")],
  data.frame(
    assumption = "Full assortativity",
    lower_ar = weighted.mean(recovered_sameq[recovered_sameq$assumption == "Full assortativity",]$lower_ar,
                             w = c(0.62, 0.27, 0.11)),
    ar = weighted.mean(recovered_sameq[recovered_sameq$assumption == "Full assortativity",]$ar,
                       w = c(0.62, 0.27, 0.11)),
    upper_ar = weighted.mean(recovered_sameq[recovered_sameq$assumption == "Full assortativity",]$upper_ar,
                             w = c(0.62, 0.27, 0.11))
  )
)

# time to peak
ttp_sameq

# Calculation of R0
proof_R0 = F

if (proof_R0 == T) {

  # proof the R0
  #-----------------
  timepoints = seq (0, 100, by=1)
  # full assortative
  transmission_probability_frail_noepi <- calculate_q(R0_input = 0.9,
                                                      beta_val = matrix_frail_frail,
                                                      n_val = mat_frail)
  
  transmission_probability_prefrail_noepi <- calculate_q(R0_input = 0.9,
                                                         beta_val = matrix_prefrail_prefrail,
                                                         n_val = mat_prefrail)
  
  transmission_probability_nonfrail_noepi <- calculate_q(R0_input = 0.9,
                                                         beta_val = matrix_nonfrail_nonfrail,
                                                         n_val = mat_nonfrail)
  
  par(mfrow = c(3, 6))
  for(j in c("frail", "prefrail", "nonfrail")) {
    for(i in 1:length(init_frail)) {
      
      if (j %in% "frail") {
        population <- pop_frail
        matrix_input <- matrix_frail_frail
        qval <- transmission_probability_frail_noepi
      } else if (j %in% "prefrail") {
        population <- pop_prefrail
        matrix_input <- matrix_prefrail_prefrail
        qval <- transmission_probability_prefrail_noepi
      } else {
        population <- pop_nonfrail
        matrix_input <- matrix_nonfrail_nonfrail
        qval <- transmission_probability_nonfrail_noepi
      }
      
      initial_values = c (S1 = population [1], S2 = population [2], S1 = population [3], S2 = population [4], S5 = population [5], S6 = population [6],
                          E1 = 0, E2 = 0, E1 = 0, E2 = 0, E5 = 0, E6 = 0,
                          I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                          I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                          I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                          I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                          I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                          I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                          R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)
      
      initial_values[6 + i] <- 1
      
      parameter_list = c (beta = matrix_input,
                          theta = theta_value, gamma = gamma_value,
                          p = p_value, delta_1 = delta_1_value, psi = psi_value,
                          delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                          delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                          tau_2 = tau_2_value, 
                          q = qval)
      
      output = ode (initial_values,
                    timepoints, seir_model_run, parameter_list)
      
      plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
            ylab = "Number of incidence",
            data = output, type='b', col = 'dodgerblue3',
            main = paste0("Init incidence ", names(initial_values[18 + i]), " = 1"))
    }
  }
  dev.off()
  
  #------
  
  transmission_probability_frail_epi <- calculate_q(R0_input = 1.1,
                                                    beta_val = matrix_frail_frail,
                                                    n_val = mat_frail)
  
  transmission_probability_prefrail_epi <- calculate_q(R0_input = 1.1,
                                                       beta_val = matrix_prefrail_prefrail,
                                                       n_val = mat_prefrail)
  
  transmission_probability_nonfrail_epi <- calculate_q(R0_input = 1.1,
                                                       beta_val = matrix_nonfrail_nonfrail,
                                                       n_val = mat_nonfrail)
  
  par(mfrow = c(3, 6))
  for(j in c("frail", "prefrail", "nonfrail")) {
    for(i in 1:length(init_frail)) {
      
      if (j %in% "frail") {
        population <- pop_frail
        matrix_input <- matrix_frail_frail
        qval <- transmission_probability_frail_epi
      } else if (j %in% "prefrail") {
        population <- pop_prefrail
        matrix_input <- matrix_prefrail_prefrail
        qval <- transmission_probability_prefrail_epi
      } else {
        population <- pop_nonfrail
        matrix_input <- matrix_nonfrail_nonfrail
        qval <- transmission_probability_nonfrail_epi
      }
      
      initial_values = c (S1 = population [1], S2 = population [2], S1 = population [3], S2 = population [4], S5 = population [5], S6 = population [6],
                          E1 = 0, E2 = 0, E1 = 0, E2 = 0, E5 = 0, E6 = 0,
                          I_pre1 = 0, I_pre2 = 0, I_pre1 = 0, I_pre2 = 0, I_pre5 = 0, I_pre6 = 0,
                          I_asy1 = 0, I_asy2 = 0, I_asy1 = 0, I_asy2 = 0, I_asy5 = 0, I_asy6 = 0,
                          I_mld1 = 0, I_mld2 = 0, I_mld1 = 0, I_mld2 = 0, I_mld5 = 0, I_mld6 = 0,
                          I_sev1 = 0, I_sev2 = 0, I_sev1 = 0, I_sev2 = 0, I_sev5 = 0, I_sev6 = 0,
                          I_hsp1 = 0, I_hsp2 = 0, I_hsp1 = 0, I_hsp2 = 0, I_hsp5 = 0, I_hsp6 = 0,
                          I_icu1 = 0, I_icu2 = 0, I_icu1 = 0, I_icu2 = 0, I_icu5 = 0, I_icu6 = 0,
                          R1 = 0, R2 = 0, R1 = 0, R2 = 0, R5 = 0, R6 = 0)
      
      initial_values[6 + i] <- 1
      
      parameter_list = c (beta = matrix_input,
                          theta = theta_value, gamma = gamma_value,
                          p = p_value, delta_1 = delta_1_value, psi = psi_value,
                          delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                          delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                          tau_2 = tau_2_value, 
                          q = qval)
      
      output = ode (initial_values,
                    timepoints, seir_model_run, parameter_list)
      
      plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
            ylab = "Number of incidence",
            data = output, type='b', col = 'dodgerblue3',
            main = paste0("Init incidence ", names(initial_values[18 + i]), " = 1"))
    }
  }
  dev.off()
  
  
  # proportionate mixing
  # Here we use different formula of R0.
  transmission_probability = calculate_q_full_agestr_mixed(R0_input = 0.9,
                                                           betaff_vals = prop_frail_frail,
                                                           betann_vals = prop_nonfrail_nonfrail,
                                                           betapp_vals = prop_prefrail_prefrail,
                                                           betanf_vals = prop_nonfrail_frail,
                                                           betafn_vals = prop_frail_nonfrail,
                                                           betafp_vals = prop_frail_prefrail,
                                                           betapf_vals = prop_prefrail_frail,
                                                           betapn_vals = prop_prefrail_nonfrail,
                                                           betanp_vals = prop_nonfrail_prefrail)
  
  par(mfrow = c(3, 6))
  for(i in 1:(3*length(init_frail))) {
    print(i)
    initial_values_sens = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                             S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                             S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                             # Exposed
                             E1_f = 0, E2_f = 0, E3_f = 0, E4_f = 0, E5_f = 0, E6_f = 0,
                             E1_p = 0, E2_p = 0, E3_p = 0, E4_p = 0, E5_p = 0, E6_p = 0,
                             E1_n = 0, E2_n = 0, E3_n = 0, E4_n = 0, E5_n = 0, E6_n = 0,
                             # I pre-symptomatic
                             I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                             I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                             I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                             # I asymptomatic
                             I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                             I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                             I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                             # I mild
                             I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                             I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                             I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                             # I severe
                             I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                             I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                             I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                             # I hospitalised
                             I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                             I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                             I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                             # I I0U
                             I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                             I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                             I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                             # I recovered
                             R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                             R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                             R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)
    
    initial_values_sens[18 + i] <- 1
    
    parameter_list = c (beta_f_f = prop_frail_frail, 
                        beta_n_n = prop_nonfrail_nonfrail, 
                        beta_p_p = prop_prefrail_prefrail,
                        beta_n_f = prop_nonfrail_frail,
                        beta_f_n = prop_frail_nonfrail,
                        beta_f_p = prop_frail_prefrail, 
                        beta_p_f = prop_prefrail_frail,
                        beta_p_n = prop_prefrail_nonfrail,
                        beta_n_p = prop_nonfrail_prefrail,
                        theta = theta_value, gamma = gamma_value,
                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                        tau_2 = tau_2_value,
                        N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                        q = transmission_probability)
    
    output = ode (initial_values_sens,
                  timepoints, seir_model_agestr, parameter_list)
    
    
    plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
          ylab = "Number of incidence",
          data = output, type='b', col = 'dodgerblue3',
          main = paste0("Init incidence ", names(initial_values_sens[18 + i]), " = 1"))
  }
  
  dev.off()
  
  # proportionate mixing
  # Here we use different formula of R0.
  transmission_probability = calculate_q_full_agestr_mixed(R0_input = 1.1,
                                                           betaff_vals = prop_frail_frail,
                                                           betann_vals = prop_nonfrail_nonfrail,
                                                           betapp_vals = prop_prefrail_prefrail,
                                                           betanf_vals = prop_nonfrail_frail,
                                                           betafn_vals = prop_frail_nonfrail,
                                                           betafp_vals = prop_frail_prefrail,
                                                           betapf_vals = prop_prefrail_frail,
                                                           betapn_vals = prop_prefrail_nonfrail,
                                                           betanp_vals = prop_nonfrail_prefrail)
  
  par(mfrow = c(3, 6))
  for(i in 1:(3*length(init_frail))) {
    print(i)
    initial_values_sens = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                             S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                             S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                             # Exposed
                             E1_f = 0, E2_f = 0, E3_f = 0, E4_f = 0, E5_f = 0, E6_f = 0,
                             E1_p = 0, E2_p = 0, E3_p = 0, E4_p = 0, E5_p = 0, E6_p = 0,
                             E1_n = 0, E2_n = 0, E3_n = 0, E4_n = 0, E5_n = 0, E6_n = 0,
                             # I pre-symptomatic
                             I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                             I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                             I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                             # I asymptomatic
                             I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                             I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                             I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                             # I mild
                             I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                             I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                             I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                             # I severe
                             I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                             I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                             I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                             # I hospitalised
                             I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                             I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                             I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                             # I I0U
                             I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                             I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                             I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                             # I recovered
                             R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                             R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                             R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)
    
    initial_values_sens[18 + i] <- 1
    
    parameter_list = c (beta_f_f = prop_frail_frail, 
                        beta_n_n = prop_nonfrail_nonfrail, 
                        beta_p_p = prop_prefrail_prefrail,
                        beta_n_f = prop_nonfrail_frail,
                        beta_f_n = prop_frail_nonfrail,
                        beta_f_p = prop_frail_prefrail, 
                        beta_p_f = prop_prefrail_frail,
                        beta_p_n = prop_prefrail_nonfrail,
                        beta_n_p = prop_nonfrail_prefrail,
                        theta = theta_value, gamma = gamma_value,
                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                        tau_2 = tau_2_value,
                        N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                        q = transmission_probability)
    
    output = ode (initial_values_sens,
                  timepoints, seir_model_agestr, parameter_list)
    
    plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
          ylab = "Number of incidence",
          data = output, type='b', col = 'dodgerblue3',
          main = paste0("Init incidence ", names(initial_values_sens[18 + i]), " = 1"))
  }
  
  dev.off()
  
  # uniform mixing
  # Here we use different formula of R0.
  transmission_probability = calculate_q_full_agestr_mixed(R0_input = 0.9,
                                                           betaff_vals = uni_frail_frail,
                                                           betann_vals = uni_nonfrail_nonfrail,
                                                           betapp_vals = uni_prefrail_prefrail,
                                                           betanf_vals = uni_nonfrail_frail,
                                                           betafn_vals = uni_frail_nonfrail,
                                                           betafp_vals = uni_frail_prefrail,
                                                           betapf_vals = uni_prefrail_frail,
                                                           betapn_vals = uni_prefrail_nonfrail,
                                                           betanp_vals = uni_nonfrail_prefrail)
  
  par(mfrow = c(3, 6))
  for(i in 1:(3*length(init_frail))) {
    print(i)
    initial_values_sens = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                             S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                             S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                             # Exposed
                             E1_f = 0, E2_f = 0, E3_f = 0, E4_f = 0, E5_f = 0, E6_f = 0,
                             E1_p = 0, E2_p = 0, E3_p = 0, E4_p = 0, E5_p = 0, E6_p = 0,
                             E1_n = 0, E2_n = 0, E3_n = 0, E4_n = 0, E5_n = 0, E6_n = 0,
                             # I pre-symptomatic
                             I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                             I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                             I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                             # I asymptomatic
                             I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                             I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                             I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                             # I mild
                             I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                             I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                             I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                             # I severe
                             I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                             I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                             I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                             # I hospitalised
                             I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                             I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                             I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                             # I I0U
                             I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                             I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                             I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                             # I recovered
                             R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                             R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                             R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)
    
    initial_values_sens[18 + i] <- 1
    
    parameter_list = c (beta_f_f = uni_frail_frail, 
                        beta_n_n = uni_nonfrail_nonfrail, 
                        beta_p_p = uni_prefrail_prefrail,
                        beta_n_f = uni_nonfrail_frail,
                        beta_f_n = uni_frail_nonfrail,
                        beta_f_p = uni_frail_prefrail, 
                        beta_p_f = uni_prefrail_frail,
                        beta_p_n = uni_prefrail_nonfrail,
                        beta_n_p = uni_nonfrail_prefrail,
                        theta = theta_value, gamma = gamma_value,
                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                        tau_2 = tau_2_value,
                        N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                        q = transmission_probability)
    
    output = ode (initial_values_sens,
                  timepoints, seir_model_agestr, parameter_list)
    
    
    plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
          ylab = "Number of incidence",
          data = output, type='b', col = 'dodgerblue3',
          main = paste0("Init incidence ", names(initial_values_sens[18 + i]), " = 1"))
  }
  dev.off()
  
  # Here we use different formula of R0.
  transmission_probability = calculate_q_full_agestr_mixed(R0_input = 1.1,
                                                           betaff_vals = uni_frail_frail,
                                                           betann_vals = uni_nonfrail_nonfrail,
                                                           betapp_vals = uni_prefrail_prefrail,
                                                           betanf_vals = uni_nonfrail_frail,
                                                           betafn_vals = uni_frail_nonfrail,
                                                           betafp_vals = uni_frail_prefrail,
                                                           betapf_vals = uni_prefrail_frail,
                                                           betapn_vals = uni_prefrail_nonfrail,
                                                           betanp_vals = uni_nonfrail_prefrail)
  
  par(mfrow = c(3, 6))
  for(i in 1:(3*length(init_frail))) {
    print(i)
    initial_values_sens = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                             S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                             S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                             # Exposed
                             E1_f = 0, E2_f = 0, E3_f = 0, E4_f = 0, E5_f = 0, E6_f = 0,
                             E1_p = 0, E2_p = 0, E3_p = 0, E4_p = 0, E5_p = 0, E6_p = 0,
                             E1_n = 0, E2_n = 0, E3_n = 0, E4_n = 0, E5_n = 0, E6_n = 0,
                             # I pre-symptomatic
                             I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                             I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                             I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                             # I asymptomatic
                             I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                             I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                             I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                             # I mild
                             I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                             I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                             I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                             # I severe
                             I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                             I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                             I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                             # I hospitalised
                             I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                             I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                             I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                             # I I0U
                             I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                             I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                             I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                             # I recovered
                             R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                             R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                             R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)
    
    initial_values_sens[18 + i] <- 1
    
    parameter_list = c (beta_f_f = uni_frail_frail, 
                        beta_n_n = uni_nonfrail_nonfrail, 
                        beta_p_p = uni_prefrail_prefrail,
                        beta_n_f = uni_nonfrail_frail,
                        beta_f_n = uni_frail_nonfrail,
                        beta_f_p = uni_frail_prefrail, 
                        beta_p_f = uni_prefrail_frail,
                        beta_p_n = uni_prefrail_nonfrail,
                        beta_n_p = uni_nonfrail_prefrail,
                        theta = theta_value, gamma = gamma_value,
                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                        tau_2 = tau_2_value,
                        N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                        q = transmission_probability)
    
    output = ode (initial_values_sens,
                  timepoints, seir_model_agestr, parameter_list)
    
    plot (rowSums(output[, grep(paste0("E.*"), colnames(output))]) ~ time,
          ylab = "Number of incidence",
          data = output, type='b', col = 'dodgerblue3',
          main = paste0("Init incidence ", names(initial_values_sens[18 + i]), " = 1"))
  }
  dev.off()
}

# ----- what if we set the same R0 = 2.90 (Abrams et al, 2021)
timepoints = seq (0, 200, by=1)

if(!file.exists("../rds/output_compartment/sir_result_samer0_withci.rds")) {
  # --- for frail
  trans_prob_samer0_frail <- calculate_q(R0_input = 2.90,
                                         beta_val = matrix_frail_frail,
                                         n_val = mat_frail)
  
  parameter_list_frail_samer0 = c (beta = matrix_frail_frail,
                                   theta = theta_value, gamma = gamma_value,
                                   p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                   delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                   delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                   tau_2 = tau_2_value, 
                                   q = trans_prob_samer0_frail)
  
  output_frail_samer0 = ode (initial_values_frail, timepoints, seir_model_run, parameter_list_frail_samer0)
  
  # --- for prefrail
  trans_prob_samer0_prefrail <- calculate_q(R0_input = 2.90,
                                            beta_val = matrix_prefrail_prefrail,
                                            n_val = mat_prefrail)
  
  parameter_list_prefrail_samer0 = c (beta = matrix_prefrail_prefrail,
                                      theta = theta_value, gamma = gamma_value,
                                      p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                      delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                      delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                      tau_2 = tau_2_value, 
                                      q = trans_prob_samer0_prefrail)
  
  output_prefrail_samer0 = ode (initial_values_prefrail, timepoints, seir_model_run, parameter_list_prefrail_samer0)
  
  # --- for nonfrail
  trans_prob_samer0_nonfrail <- calculate_q(R0_input = 2.90,
                                            beta_val = matrix_nonfrail_nonfrail,
                                            n_val = mat_nonfrail)
  
  parameter_list_nonfrail_samer0 = c (beta = matrix_nonfrail_nonfrail,
                                      theta = theta_value, gamma = gamma_value,
                                      p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                      delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                      delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                      tau_2 = tau_2_value, 
                                      q = trans_prob_samer0_nonfrail)
  
  output_nonfrail_samer0 = ode (initial_values_nonfrail, timepoints, seir_model_run, parameter_list_nonfrail_samer0)
  
  # --- 
  initial_values_samer0 = c (S1_f = pop_frail[1], S2_f = pop_frail[2], S3_f = pop_frail[3], S4_f = pop_frail[4], S5_f = pop_frail[5], S6_f = pop_frail[6],
                             S1_p = pop_prefrail[1], S2_p = pop_prefrail[2], S3_p = pop_prefrail[3], S4_p = pop_prefrail[4], S5_p = pop_prefrail[5], S6_p = pop_prefrail[6],
                             S1_n = pop_nonfrail[1], S2_n = pop_nonfrail[2], S3_n = pop_nonfrail[3], S4_n = pop_nonfrail[4], S5_n = pop_nonfrail[5], S6_n = pop_nonfrail[6],
                             # Exposed
                             E1_f = init_frail[1], E2_f = init_frail[2], E3_f = init_frail[3], E4_f = init_frail[4], E5_f = init_frail[5], E6_f = init_frail[6],
                             E1_p = init_prefrail[1], E2_p = init_prefrail[2], E3_p = init_prefrail[3], E4_p = init_prefrail[4], E5_p = init_prefrail[5], E6_p = init_prefrail[6],
                             E1_n = init_nonfrail[1], E2_n = init_nonfrail[2], E3_n = init_nonfrail[3], E4_n = init_nonfrail[4], E5_n = init_nonfrail[5], E6_n = init_nonfrail[6],
                             # I pre-symptomatic
                             I_pre1_f = 0, I_pre2_f = 0, I_pre3_f = 0, I_pre4_f = 0, I_pre5_f = 0, I_pre6_f = 0,
                             I_pre1_p = 0, I_pre2_p = 0, I_pre3_p = 0, I_pre4_p = 0, I_pre5_p = 0, I_pre6_p = 0,
                             I_pre1_n = 0, I_pre2_n = 0, I_pre3_n = 0, I_pre4_n = 0, I_pre5_n = 0, I_pre6_n = 0,
                             # I asymptomatic
                             I_asy1_f = 0, I_asy2_f = 0, I_asy3_f = 0, I_asy4_f = 0, I_asy5_f = 0, I_asy6_f = 0,
                             I_asy1_p = 0, I_asy2_p = 0, I_asy3_p = 0, I_asy4_p = 0, I_asy5_p = 0, I_asy6_p = 0,
                             I_asy1_n = 0, I_asy2_n = 0, I_asy3_n = 0, I_asy4_n = 0, I_asy5_n = 0, I_asy6_n = 0,
                             # I mild
                             I_mld1_f = 0, I_mld2_f = 0, I_mld3_f = 0, I_mld4_f = 0, I_mld5_f = 0, I_mld6_f = 0,
                             I_mld1_p = 0, I_mld2_p = 0, I_mld3_p = 0, I_mld4_p = 0, I_mld5_p = 0, I_mld6_p = 0,
                             I_mld1_n = 0, I_mld2_n = 0, I_mld3_n = 0, I_mld4_n = 0, I_mld5_n = 0, I_mld6_n = 0,
                             # I severe
                             I_sev1_f = 0, I_sev2_f = 0, I_sev3_f = 0, I_sev4_f = 0, I_sev5_f = 0, I_sev6_f = 0,
                             I_sev1_p = 0, I_sev2_p = 0, I_sev3_p = 0, I_sev4_p = 0, I_sev5_p = 0, I_sev6_p = 0,
                             I_sev1_n = 0, I_sev2_n = 0, I_sev3_n = 0, I_sev4_n = 0, I_sev5_n = 0, I_sev6_n = 0,
                             # I hospitalised
                             I_hsp1_f = 0, I_hsp2_f = 0, I_hsp3_f = 0, I_hsp4_f = 0, I_hsp5_f = 0, I_hsp6_f = 0,
                             I_hsp1_p = 0, I_hsp2_p = 0, I_hsp3_p = 0, I_hsp4_p = 0, I_hsp5_p = 0, I_hsp6_p = 0,
                             I_hsp1_n = 0, I_hsp2_n = 0, I_hsp3_n = 0, I_hsp4_n = 0, I_hsp5_n = 0, I_hsp6_n = 0,
                             # I I0U
                             I_icu1_f = 0, I_icu2_f = 0, I_icu3_f = 0, I_icu4_f = 0, I_icu5_f = 0, I_icu6_f = 0,
                             I_icu1_p = 0, I_icu2_p = 0, I_icu3_p = 0, I_icu4_p = 0, I_icu5_p = 0, I_icu6_p = 0,
                             I_icu1_n = 0, I_icu2_n = 0, I_icu3_n = 0, I_icu4_n = 0, I_icu5_n = 0, I_icu6_n = 0,
                             # I recovered
                             R1_f = 0, R2_f = 0, R3_f = 0, R4_f = 0, R5_f = 0, R6_f = 0,
                             R1_p = 0, R2_p = 0, R3_p = 0, R4_p = 0, R5_p = 0, R6_p = 0,
                             R1_n = 0, R2_n = 0, R3_n = 0, R4_n = 0, R5_n = 0, R6_n = 0)
  
  trans_uni_samer0 <- calculate_q_full_agestr_mixed(R0_input = 2.9,
                                                   betaff_vals = uni_frail_frail,
                                                   betann_vals = uni_nonfrail_nonfrail,
                                                   betapp_vals = uni_prefrail_prefrail,
                                                   betanf_vals = uni_nonfrail_frail,
                                                   betafn_vals = uni_frail_nonfrail,
                                                   betafp_vals = uni_frail_prefrail,
                                                   betapf_vals = uni_prefrail_frail,
                                                   betapn_vals = uni_prefrail_nonfrail,
                                                   betanp_vals = uni_nonfrail_prefrail)
  
  parameter_list_uni_samer0 = c (beta_f_f = uni_frail_frail, 
                                 beta_n_n = uni_nonfrail_nonfrail, 
                                 beta_p_p = uni_prefrail_prefrail,
                                 beta_n_f = uni_nonfrail_frail,
                                 beta_f_n = uni_frail_nonfrail,
                                 beta_f_p = uni_frail_prefrail, 
                                 beta_p_f = uni_prefrail_frail,
                                 beta_p_n = uni_prefrail_nonfrail,
                                 beta_n_p = uni_nonfrail_prefrail,
                                 theta = theta_value, gamma = gamma_value,
                                 p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                 delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                 delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                 tau_2 = tau_2_value,
                                 N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                                 q = trans_uni_samer0)
  
  output_uni_samer0 = ode (initial_values_samer0,
                           timepoints, seir_model_agestr, parameter_list_uni_samer0)
  
  # ---
  trans_prop_samer0 <- calculate_q_full_agestr_mixed(R0_input = 2.9,
                                                    betaff_vals = prop_frail_frail,
                                                    betann_vals = prop_nonfrail_nonfrail,
                                                    betapp_vals = prop_prefrail_prefrail,
                                                    betanf_vals = prop_nonfrail_frail,
                                                    betafn_vals = prop_frail_nonfrail,
                                                    betafp_vals = prop_frail_prefrail,
                                                    betapf_vals = prop_prefrail_frail,
                                                    betapn_vals = prop_prefrail_nonfrail,
                                                    betanp_vals = prop_nonfrail_prefrail)
  
  parameter_list_prop_samer0 = c (beta_f_f = prop_frail_frail, 
                                  beta_n_n = prop_nonfrail_nonfrail, 
                                  beta_p_p = prop_prefrail_prefrail,
                                  beta_n_f = prop_nonfrail_frail,
                                  beta_f_n = prop_frail_nonfrail,
                                  beta_f_p = prop_frail_prefrail, 
                                  beta_p_f = prop_prefrail_frail,
                                  beta_p_n = prop_prefrail_nonfrail,
                                  beta_n_p = prop_nonfrail_prefrail,
                                  theta = theta_value, gamma = gamma_value,
                                  p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                  delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                  delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                  tau_2 = tau_2_value,
                                  N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
                                  q = trans_prop_samer0)
  
  output_prop_samer0 = ode (initial_values_samer0,
                           timepoints, seir_model_agestr, parameter_list_prop_samer0)
  
  # ----
  # For confidence interval ------- (stratified non-parametric bootstrap)
  # 1. uniform mixing
  boot_uni <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_uniform.rds")
  output_unir0_boot <- list()
  
  for(b in 1:n_boot) {
    print(b)
    par = c (beta_f_f = boot_uni$frail_output[[b]]$frail_frail_matrix, 
             beta_n_n = boot_uni$nonfrail_output[[b]]$nonfrail_nonfrail_matrix, 
             beta_p_p = boot_uni$prefrail_output[[b]]$prefrail_prefrail_matrix,
             beta_n_f = boot_uni$nonfrail_output[[b]]$nonfrail_frail_matrix,
             beta_f_n = boot_uni$frail_output[[b]]$frail_nonfrail_matrix,
             beta_f_p = boot_uni$frail_output[[b]]$frail_prefrail_matrix, 
             beta_p_f = boot_uni$prefrail_output[[b]]$prefrail_frail_matrix,
             beta_p_n = boot_uni$prefrail_output[[b]]$prefrail_nonfrail_matrix,
             beta_n_p = boot_uni$nonfrail_output[[b]]$nonfrail_prefrail_matrix,
             theta = theta_value, gamma = gamma_value,
             p = p_value, delta_1 = delta_1_value, psi = psi_value,
             delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
             delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
             tau_2 = tau_2_value,
             N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
             q = trans_uni_samer0)
    
    output_unir0_boot[[b]] = ode (initial_values, timepoints, seir_model_agestr, par)
    
  }
  
  uni_nonfrail_r0 <- lapply(output_unir0_boot, function(x) rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T))
  uni_prefrail_r0 <- lapply(output_unir0_boot, function(x) rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T))
  uni_frail_r0 <- lapply(output_unir0_boot, function(x) rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T))
  
  ttp_uni_nonfrail_r0 <- lapply(output_unir0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T)))
  ttp_uni_prefrail_r0 <- lapply(output_unir0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T)))
  ttp_uni_frail_r0 <- lapply(output_unir0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T)))
  
  rec_uni_r0 <- quantile(sapply(output_unir0_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                      probs = c(0.025, 0.975), na.rm = T)
  
  # Combine the lists into a matrix where each column corresponds to one list
  ci_uni_nonfrail_r0 <- apply(
    do.call(cbind, uni_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_uni_prefrail_r0 <- apply(
    do.call(cbind, uni_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_uni_frail_r0 <- apply(
    do.call(cbind, uni_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  ttpci_uni_nonfrail_r0 <- apply(
    do.call(cbind, ttp_uni_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_uni_prefrail_r0 <- apply(
    do.call(cbind, ttp_uni_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_uni_frail_r0 <- apply(
    do.call(cbind, ttp_uni_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  # 2. proportionate mixing
  boot_prop <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_proportionate.rds")
  output_propr0_boot <- list()
  
  for(b in 1:n_boot) {
    print(b)
    par = c (beta_f_f = boot_prop$frail_output[[b]]$frail_frail_matrix, 
             beta_n_n = boot_prop$nonfrail_output[[b]]$nonfrail_nonfrail_matrix, 
             beta_p_p = boot_prop$prefrail_output[[b]]$prefrail_prefrail_matrix,
             beta_n_f = boot_prop$nonfrail_output[[b]]$nonfrail_frail_matrix,
             beta_f_n = boot_prop$frail_output[[b]]$frail_nonfrail_matrix,
             beta_f_p = boot_prop$frail_output[[b]]$frail_prefrail_matrix, 
             beta_p_f = boot_prop$prefrail_output[[b]]$prefrail_frail_matrix,
             beta_p_n = boot_prop$prefrail_output[[b]]$prefrail_nonfrail_matrix,
             beta_n_p = boot_prop$nonfrail_output[[b]]$nonfrail_prefrail_matrix,
             theta = theta_value, gamma = gamma_value,
             p = p_value, delta_1 = delta_1_value, psi = psi_value,
             delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
             delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
             tau_2 = tau_2_value,
             N_f = pop_frail, N_n = pop_nonfrail, N_p = pop_prefrail,
             q = trans_prop_samer0)
    
    output_propr0_boot[[b]] = ode (initial_values, timepoints, seir_model_agestr, par)
    
  }
  
  prop_nonfrail_r0 <- lapply(output_propr0_boot, function(x) rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T))
  prop_prefrail_r0 <- lapply(output_propr0_boot, function(x) rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T))
  prop_frail_r0 <- lapply(output_propr0_boot, function(x) rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T))
  
  ttp_prop_nonfrail_r0 <- lapply(output_propr0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_n.*"), colnames(x))], na.rm = T)))
  ttp_prop_prefrail_r0 <- lapply(output_propr0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_p.*"), colnames(x))], na.rm = T)))
  ttp_prop_frail_r0 <- lapply(output_propr0_boot, function(x) which.max(rowSums(x[, grep(paste0("E.*_f.*"), colnames(x))], na.rm = T)))
  
  rec_prop_r0 <- quantile(sapply(output_propr0_boot, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                         probs = c(0.025, 0.975), na.rm = T)
  
  # Combine the lists into a matrix where each column corresponds to one list
  ci_prop_nonfrail_r0 <- apply(
    do.call(cbind, prop_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_prop_prefrail_r0 <- apply(
    do.call(cbind, prop_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_prop_frail_r0 <- apply(
    do.call(cbind, prop_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  ttpci_prop_nonfrail_r0 <- apply(
    do.call(cbind, ttp_prop_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_prop_prefrail_r0 <- apply(
    do.call(cbind, ttp_prop_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_prop_frail_r0 <- apply(
    do.call(cbind, ttp_prop_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  # 3. Full assortativity
  boot_fa <- readRDS("../rds/matrix_decomposition/boot_scenario_rates_full.rds")
  output_frail_boot_r0 <- list(); output_prefrail_boot_r0 <- list(); output_nonfrail_boot_r0 <- list()
  
  for(b in 1:n_boot) {
    print(b)
    # --- for frail
    parameter_list_frail_samer0 = c (beta = boot_fa$frail_output[[b]]$frail_frail_matrix,
                                     theta = theta_value, gamma = gamma_value,
                                     p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                     delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                     delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                     tau_2 = tau_2_value, 
                                     q = trans_prob_samer0_frail)
    
    output_frail_boot_r0[[b]] = ode (initial_values_frail, timepoints, seir_model_run, parameter_list_frail_samer0)
    
    # --- for prefrail
    parameter_list_prefrail_samer0 = c (beta = boot_fa$prefrail_output[[b]]$prefrail_prefrail_matrix,
                                        theta = theta_value, gamma = gamma_value,
                                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                        tau_2 = tau_2_value, 
                                        q = trans_prob_samer0_prefrail)
    
    output_prefrail_boot_r0[[b]] = ode (initial_values_prefrail, timepoints, seir_model_run, parameter_list_prefrail_samer0)
    
    # --- for nonfrail
    parameter_list_nonfrail_samer0 = c (beta = boot_fa$nonfrail_output[[b]]$nonfrail_nonfrail_matrix,
                                        theta = theta_value, gamma = gamma_value,
                                        p = p_value, delta_1 = delta_1_value, psi = psi_value,
                                        delta_2 = delta_2_value, omega = omega_value, phi_1 = phi_1_value,
                                        delta_3 = delta_3_value, tau_1 = tau_1_value, delta_4 = delta_4_value,
                                        tau_2 = tau_2_value, 
                                        q = trans_prob_samer0_nonfrail)
    
    output_nonfrail_boot_r0[[b]] = ode (initial_values_nonfrail, timepoints, seir_model_run, parameter_list_nonfrail_samer0)
    
  }
  
  full_nonfrail_r0 <- lapply(output_nonfrail_boot_r0, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  full_prefrail_r0 <- lapply(output_prefrail_boot_r0, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  full_frail_r0 <- lapply(output_frail_boot_r0, function(x) rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T))
  
  ttp_full_nonfrail_r0 <- lapply(output_nonfrail_boot_r0, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  ttp_full_prefrail_r0 <- lapply(output_prefrail_boot_r0, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  ttp_full_frail_r0 <- lapply(output_frail_boot_r0, function(x) which.max(rowSums(x[, grep(paste0("E.*"), colnames(x))], na.rm = T)))
  
  rec_full_nonfrail_r0 <- quantile(sapply(output_nonfrail_boot_r0, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                                probs = c(0.025, 0.975), na.rm = T)
  rec_full_prefrail_r0 <- quantile(sapply(output_prefrail_boot_r0, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                                probs = c(0.025, 0.975), na.rm = T)
  rec_full_frail_r0 <- quantile(sapply(output_frail_boot_r0, function(x) sum(x[nrow(x), grep(paste0("R.*"), colnames(x))])),
                             probs = c(0.025, 0.975), na.rm = T)
  
  
  # Combine the lists into a matrix where each column corresponds to one list
  ci_full_nonfrail_r0 <- apply(
    do.call(cbind, full_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_full_prefrail_r0 <- apply(
    do.call(cbind, full_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ci_full_frail_r0 <- apply(
    do.call(cbind, full_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_full_nonfrail_r0 <- apply(
    do.call(cbind, ttp_full_nonfrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_full_prefrail_r0 <- apply(
    do.call(cbind, ttp_full_prefrail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  ttpci_full_frail_r0 <- apply(
    do.call(cbind, ttp_full_frail_r0), 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  )
  
  # --- output manuscript
  # ----
  data_samer0 <- rbind(
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_nonfrail_r0[1,], 
      case = rowSums(output_prop_samer0[, grep(paste0("E.*_n.*"), colnames(output_prop_samer0))]),
      upper_case = ci_prop_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_prefrail_r0[1,], 
      case = rowSums(output_prop_samer0[, grep(paste0("E.*_p.*"), colnames(output_prop_samer0))]),
      upper_case = ci_prop_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_prop_frail_r0[1,], 
      case = rowSums(output_prop_samer0[, grep(paste0("E.*_f.*"), colnames(output_prop_samer0))]),
      upper_case = ci_prop_frail_r0[2,], 
      type = "Frail",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_nonfrail_r0[1,], 
      case = rowSums(output_uni_samer0[, grep(paste0("E.*_n.*"), colnames(output_uni_samer0))]),
      upper_case = ci_uni_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_prefrail_r0[1,], 
      case = rowSums(output_uni_samer0[, grep(paste0("E.*_p.*"), colnames(output_uni_samer0))]),
      upper_case = ci_uni_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_uni_frail_r0[1,], 
      case = rowSums(output_uni_samer0[, grep(paste0("E.*_f.*"), colnames(output_uni_samer0))]),
      upper_case = ci_uni_frail_r0[2,], 
      type = "Frail",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_nonfrail_r0[1,], 
      case = rowSums(output_nonfrail_samer0[, grep(paste0("E.*"), colnames(output_nonfrail_samer0))]),
      upper_case = ci_full_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_prefrail_r0[1,], 
      case = rowSums(output_prefrail_samer0[, grep(paste0("E.*"), colnames(output_prefrail_samer0))]),
      upper_case = ci_full_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      time = seq(0, 200),
      low_case = ci_full_frail_r0[1,], 
      case = rowSums(output_frail_samer0[, grep(paste0("E.*"), colnames(output_frail_samer0))]),
      upper_case = ci_full_frail_r0[2,], 
      type = "Frail",
      assumption = "Full assortativity"
    )
    
  )
  
  data_samer0$type <- factor(data_samer0$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  data_samer0$assumption <- factor(data_samer0$assumption,
                                  levels = c("Proportionate mixing", "Uniform mixing",
                                             "Full assortativity"))
  
  # For the recovered dataset.
  recovered_samer0 <- rbind(
    data.frame(
      low_recov = rec_prop_r0[1], 
      recov = sum(output_prop_samer0[nrow(output_prop_samer0), grep(paste0("R.*"), colnames(output_prop_samer0))]),
      upper_recov = rec_prop_r0[2], 
      type = "All",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      low_recov = rec_uni_r0[1], 
      recov = sum(output_uni_samer0[nrow(output_uni_samer0), grep(paste0("R.*"), colnames(output_uni_samer0))]),
      upper_recov = rec_uni_r0[2], 
      type = "All",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      low_recov = rec_full_nonfrail_r0[1], 
      recov = sum(output_nonfrail_samer0[nrow(output_nonfrail_samer0), grep(paste0("R.*"), colnames(output_nonfrail_samer0))]),
      upper_recov = rec_full_nonfrail_r0[2], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_recov = rec_full_prefrail_r0[1], 
      recov = sum(output_prefrail_samer0[nrow(output_prefrail_samer0), grep(paste0("R.*"), colnames(output_prefrail_samer0))]),
      upper_recov = rec_full_prefrail_r0[2], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_recov = rec_full_frail_r0[1], 
      recov = sum(output_frail_samer0[nrow(output_frail_samer0), grep(paste0("R.*"), colnames(output_frail_samer0))]),
      upper_recov = rec_full_frail_r0[2], 
      type = "Frail",
      assumption = "Full assortativity"
    )
    
  )
  
  recovered_samer0$type <- factor(recovered_samer0$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  recovered_samer0$assumption <- factor(recovered_samer0$assumption,
                                       levels = c("Proportionate mixing", "Uniform mixing",
                                                  "Full assortativity"))
  
  ttp_samer0 <- rbind(
    data.frame(
      low_case = ttpci_prop_nonfrail_r0[1,], 
      case = which.max(rowSums(output_prop_samer0[, grep("E.*_n.*", colnames(output_prop_samer0))])),
      upper_case = ttpci_prop_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      low_case = ttpci_prop_prefrail_r0[1,], 
      case = which.max(rowSums(output_prop_samer0[, grep("E.*_p.*", colnames(output_prop_samer0))])),
      upper_case = ttpci_prop_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Proportionate mixing"
    ),
    data.frame(
      low_case = ttpci_prop_frail_r0[1,], 
      case = which.max(rowSums(output_prop_samer0[, grep("E.*_f.*", colnames(output_prop_samer0))])),
      upper_case = ttpci_prop_frail_r0[2,], 
      type = "Frail",
      assumption = "Proportionate mixing"
    ),
    
    data.frame(
      low_case = ttpci_uni_nonfrail_r0[1,], 
      case = which.max(rowSums(output_uni_samer0[, grep("E.*_n.*", colnames(output_uni_samer0))])),
      upper_case = ttpci_uni_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      low_case = ttpci_uni_prefrail_r0[1,], 
      case = which.max(rowSums(output_uni_samer0[, grep("E.*_p.*", colnames(output_uni_samer0))])),
      upper_case = ttpci_uni_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Uniform mixing"
    ),
    data.frame(
      low_case = ttpci_uni_frail_r0[1,], 
      case = which.max(rowSums(output_uni_samer0[, grep("E.*_f.*", colnames(output_uni_samer0))])),
      upper_case = ttpci_uni_frail_r0[2,], 
      type = "Frail",
      assumption = "Uniform mixing"
    ),
    
    data.frame(
      low_case = ttpci_full_nonfrail_r0[1,], 
      case = which.max(rowSums(output_nonfrail_samer0[, grep("E.*", colnames(output_nonfrail_samer0))])),
      upper_case = ttpci_full_nonfrail_r0[2,], 
      type = "Non-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_case = ttpci_full_prefrail_r0[1,], 
      case = which.max(rowSums(output_prefrail_samer0[, grep("E.*", colnames(output_prefrail_samer0))])),
      upper_case = ttpci_full_prefrail_r0[2,], 
      type = "Pre-frail",
      assumption = "Full assortativity"
    ),
    data.frame(
      low_case = ttpci_full_frail_r0[1,], 
      case = which.max(rowSums(output_frail_samer0[, grep("E.*", colnames(output_frail_samer0))])),
      upper_case = ttpci_full_frail_r0[2,], 
      type = "Frail",
      assumption = "Full assortativity"
    )
  )
  
  ttp_samer0$type <- factor(ttp_samer0$type, levels = c("Non-frail", "Pre-frail", "Frail"))
  ttp_samer0$assumption <- factor(ttp_samer0$assumption,
                                 levels = c("Proportionate mixing", "Uniform mixing",
                                            "Full assortativity"))
  
  saveRDS(data_samer0, "../rds/output_compartment/sir_result_samer0_withci.rds")
  saveRDS(recovered_samer0, "../rds/output_compartment/sir_recovered_samer0_withci.rds")
  saveRDS(ttp_samer0, "../rds/output_compartment/sir_ttp_samer0_withci.rds")
  
} else {
  data_samer0 <- readRDS("../rds/output_compartment/sir_result_samer0_withci.rds")
  recovered_samer0 <- readRDS("../rds/output_compartment/sir_recovered_samer0_withci.rds")
  ttp_samer0 <- readRDS("../rds/output_compartment/sir_ttp_samer0_withci.rds")
}

ggplot_samer0 <- ggplot(data = data_samer0, aes(x = time, y = case / 100000, colour = type, fill = type)) +
  geom_line(size = 1) +  # Increase line size for better visibility
  geom_ribbon(aes(ymin = low_case / 100000, ymax = upper_case / 100000), alpha = 0.1) +
  facet_grid(assumption ~ type) +
  scale_colour_manual(
    values = c(
      "Non-frail" = "darkblue",
      "Pre-frail" = "darkgreen",
      "Frail" = "darkred"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Non-frail" = "darkblue",
      "Pre-frail" = "darkgreen",
      "Frail" = "darkred"
    )
  ) +
  labs(
    x = "Time in days since March 1, 2020",
    y = "Incidence per 100,000",
    colour = NULL,  # Removes legend title
    fill = NULL     # Removes fill legend title
  ) +
  guides(
    colour = guide_legend(override.aes = list(fill = NA)),  # Align colour and fill legends
    fill = "none"  # Suppress separate fill legend
  ) +
  theme(
    text = element_text(size = 18),  # Increases font size
    legend.position = "none",  # Moves legend above the plot
    legend.key = element_blank(),  # Removes legend key background
    panel.background = element_rect(fill = "white"),  # Makes background white
    panel.grid.minor = element_blank()  # Removes minor grid lines
  )

file_name = paste("../output/compartmental_output_ci.png", sep="")
png(file_name, width = 40, height = 30, units = "cm", res = 250)
print(ggplot_samer0)
dev.off()

# peak cases
assumption <- c("Proportionate mixing", "Uniform mixing", "Full assortativity")
type <- c("Non-frail", "Pre-frail", "Frail")

peak_samer0 <- list()
count = 1
for(a in assumption) {
  for(t in type) {
    peak_samer0[[count]] <- data_samer0[data_samer0$assumption == a & data_samer0$type == t & 
                                          data_samer0$case == max(data_samer0$case[data_samer0$assumption == a & data_samer0$type == t]), ]
    count = count + 1
  }
  peak_samer0_df <- do.call("rbind", peak_samer0)
  peak_samer0_df$low_case <- round(peak_samer0_df$low_case / 100000, 2)
  peak_samer0_df$case <- round(peak_samer0_df$case / 100000, 2)
  peak_samer0_df$upper_case <- round(peak_samer0_df$upper_case / 100000, 2)
}

# attack rates 
recovered_samer0 <- recovered_samer0 %>% 
  mutate(lower_ar = ifelse(assumption != "Full assortativity", low_recov/sum(population_vector), 
                           ifelse(assumption == "Full assortativity" & type == "Non-frail", low_recov/sum(pop_nonfrail),
                                  ifelse(assumption == "Full assortativity" & type == "Pre-frail", low_recov/sum(pop_prefrail),
                                         low_recov/sum(pop_nonfrail)))),
         ar = ifelse(assumption != "Full assortativity", recov/sum(population_vector), 
                     ifelse(assumption == "Full assortativity" & type == "Non-frail", recov/sum(pop_nonfrail),
                            ifelse(assumption == "Full assortativity" & type == "Pre-frail", recov/sum(pop_prefrail),
                                   recov/sum(pop_nonfrail)))),
         upper_ar = ifelse(assumption != "Full assortativity", upper_recov/sum(population_vector), 
                           ifelse(assumption == "Full assortativity" & type == "Non-frail", upper_recov/sum(pop_nonfrail),
                                  ifelse(assumption == "Full assortativity" & type == "Pre-frail", upper_recov/sum(pop_prefrail),
                                         upper_recov/sum(pop_nonfrail))))
  )

ar_dfr0 <- rbind(
  recovered_samer0[recovered_samer0$assumption != "Full assortativity", c("assumption", "lower_ar", "ar", "upper_ar")],
  data.frame(
    assumption = "Full assortativity",
    lower_ar = weighted.mean(recovered_samer0[recovered_samer0$assumption == "Full assortativity",]$lower_ar,
                             w = c(0.62, 0.27, 0.11)),
    ar = weighted.mean(recovered_samer0[recovered_samer0$assumption == "Full assortativity",]$ar,
                       w = c(0.62, 0.27, 0.11)),
    upper_ar = weighted.mean(recovered_samer0[recovered_samer0$assumption == "Full assortativity",]$upper_ar,
                             w = c(0.62, 0.27, 0.11))
  )
)

# time to peak
ttp_samer0
