# define the model
#-------------------

seir_model_agestr = function (current_timepoint, state_values, parameters) {
  
  # create state variables (local variables)
  S_f = state_values [1:6]        # susceptibles frail
  S_p = state_values [7:12]        # susceptibles pre-frail
  S_n = state_values [13:18]        # susceptibles non-frail
  
  E_f = state_values [19:24]        # exposed frail
  E_p = state_values [25:30]        # exposed pre-frail
  E_n = state_values [31:36]        # exposed non-frail
  
  I_pre_f = state_values [37:42]        # infectious presymp frail
  I_pre_p = state_values [43:48]        # infectious presymp pre-frail
  I_pre_n = state_values [49:54]        # infectious presymp non-frail
  
  I_asy_f = state_values [55:60]        # infectious asymptomatic frail
  I_asy_p = state_values [61:66]        # infectious asymptomatic pre-frail
  I_asy_n = state_values [67:72]        # infectious asymptomatic non-frail
  
  I_mld_f = state_values [73:78]        # infectious mild frail
  I_mld_p = state_values [79:84]        # infectious mild pre-frail
  I_mld_n = state_values [85:90]        # infectious mild non-frail
  
  I_sev_f = state_values [91:96]        # infectious severe frail
  I_sev_p = state_values [97:102]        # infectious severe pre-frail
  I_sev_n = state_values [103:108]        # infectious severe non-frail
  
  I_hsp_f = state_values [109:114]        # infectious hospitalized frail
  I_hsp_p = state_values [115:120]        # infectious hospitalized pre-frail
  I_hsp_n = state_values [121:126]        # infectious hospitalized non-frail
  
  I_icu_f = state_values [127:132]        # infectious ICU frail
  I_icu_p = state_values [133:138]        # infectious ICU pre-frail
  I_icu_n = state_values [139:144]        # infectious ICU non-frail
  
  R_f = state_values [145:150]        # recovered frail
  R_p = state_values [151:156]        # recovered pre-frail
  R_n = state_values [157:162]        # recovered non-frail
  
  q = parameters [355]
  
  beta_f_f = matrix(parameters [1:36], 6, 6) * q
  beta_n_n = matrix(parameters [37:72], 6, 6) * q
  beta_p_p = matrix(parameters [73:108], 6, 6) * q
  beta_n_f = matrix(parameters [109:144], 6, 6) * q
  beta_f_n = matrix(parameters [145:180], 6, 6) * q
  beta_f_p = matrix(parameters [181:216], 6, 6) * q
  beta_p_f = matrix(parameters [217:252], 6, 6) * q
  beta_p_n = matrix(parameters [253:288], 6, 6) * q
  beta_n_p = matrix(parameters [289:324], 6, 6) * q
  
  N_f = parameters [337:342]
  N_n = parameters [343:348]
  N_p = parameters [349:354]
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivative
      dS_f = S_f * (-(I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_f_f)  +
                      - (I_pre_n  + I_asy_n + I_mld_n + I_sev_n) %*% (beta_f_n)  +
                      - (I_pre_p  + I_asy_p + I_mld_p + I_sev_p) %*% (beta_f_p))
      dS_p = S_p * (-(I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_p_f) +
                      - (I_pre_n + I_asy_n + I_mld_n + I_sev_n) %*% (beta_p_n) +
                      - (I_pre_p + I_asy_p + I_mld_p + I_sev_p) %*% (beta_p_p))
      dS_n = S_n * (-(I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_n_f)  +
                      - (I_pre_n + I_asy_n + I_mld_n + I_sev_n) %*% (beta_n_n) +
                      -  (I_pre_p + I_asy_p + I_mld_p + I_sev_p) %*% (beta_n_p))
      
      dE_f = S_f * ((I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_f_f) +
                      (I_pre_n + I_asy_n + I_mld_n + I_sev_n) %*% (beta_f_n)  +
                      (I_pre_p + I_asy_p + I_mld_p + I_sev_p) %*% (beta_f_p)) - (gamma * E_f)
      dE_p = S_p * ((I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_p_f)  +
                      (I_pre_n + I_asy_n + I_mld_n + I_sev_n) %*% (beta_p_n)  +
                      (I_pre_p + I_asy_p + I_mld_p + I_sev_p) %*% (beta_p_p)) - (gamma * E_p)
      dE_n = S_n * ((I_pre_f + I_asy_f + I_mld_f + I_sev_f) %*% (beta_n_f) +
                      (I_pre_n + I_asy_n + I_mld_n + I_sev_n) %*% (beta_n_n) +
                      (I_pre_p + I_asy_p + I_mld_p + I_sev_p) %*% (beta_n_p)) - (gamma * E_n)
      
      dI_pre_f = (gamma * E_f) - (theta * I_pre_f)
      dI_pre_p = (gamma * E_p) - (theta * I_pre_p)
      dI_pre_n = (gamma * E_n) - (theta * I_pre_n)
      
      dI_asy_f = (theta * p * I_pre_f) - (delta_1 * I_asy_f)
      dI_asy_p = (theta * p * I_pre_p) - (delta_1 * I_asy_p)
      dI_asy_n = (theta * p * I_pre_n) - (delta_1 * I_asy_n)
      
      dI_mld_f = (theta * (1-p) * I_pre_f) - (psi + delta_2) * I_mld_f
      dI_mld_p = (theta * (1-p) * I_pre_p) - (psi + delta_2) * I_mld_p
      dI_mld_n = (theta * (1-p) * I_pre_n) - (psi + delta_2) * I_mld_n
      
      dI_sev_f = (psi * I_mld_f) - (omega * I_sev_f)
      dI_sev_p = (psi * I_mld_p) - (omega * I_sev_p)
      dI_sev_n = (psi * I_mld_n) - (omega * I_sev_n)
      
      dI_hsp_f = (phi_1 * omega * I_sev_f) - (delta_3 + tau_1) * I_hsp_f
      dI_hsp_p = (phi_1 * omega * I_sev_p) - (delta_3 + tau_1) * I_hsp_p
      dI_hsp_n = (phi_1 * omega * I_sev_n) - (delta_3 + tau_1) * I_hsp_n
      
      dI_icu_f = (1 - phi_1) * omega * I_sev_f - (delta_4 + tau_2) * I_icu_f
      dI_icu_p = (1 - phi_1) * omega * I_sev_p - (delta_4 + tau_2) * I_icu_p
      dI_icu_n = (1 - phi_1) * omega * I_sev_n - (delta_4 + tau_2) * I_icu_n
      
      dR_f = (delta_1 * I_asy_f) + (delta_2 * I_mld_f) + (delta_3 * I_hsp_f) + (delta_4 * I_icu_f)
      dR_p = (delta_1 * I_asy_p) + (delta_2 * I_mld_p) + (delta_3 * I_hsp_p) + (delta_4 * I_icu_p)
      dR_n = (delta_1 * I_asy_n) + (delta_2 * I_mld_n) + (delta_3 * I_hsp_n) + (delta_4 * I_icu_n)
      
      # combine results
      results = c (dS_f, dS_p, dS_n,
                   dE_f, dE_p, dE_n,
                   dI_pre_f, dI_pre_p, dI_pre_n,
                   dI_asy_f, dI_asy_p, dI_asy_n,
                   dI_mld_f, dI_mld_p, dI_mld_n,
                   dI_sev_f, dI_sev_p, dI_sev_n,
                   dI_hsp_f, dI_hsp_p, dI_hsp_n,
                   dI_icu_f, dI_icu_p, dI_icu_n,
                   dR_f, dR_p, dR_n)
      list (results)
    }
  )
}

seir_model_run = function (current_timepoint, state_values, parameters) {
  # create state variables (local variables)
  S     = state_values [1:6]        # susceptibles
  E     = state_values [7:12]        # exposed
  I_pre = state_values [13:18]        # infectious pre-symptomatic
  I_asy = state_values [19:24]        # infectious asymptomatic
  I_mld = state_values [25:30]        # infectious mild
  I_sev = state_values [31:36]        # infectious severe
  I_hsp = state_values [37:42]        # infectious hospitalized
  I_icu = state_values [43:48]        # infectious ICU
  R     = state_values [49:54]        # recovered
  
  q = parameters [49]
  
  beta = matrix(parameters[1:(length(S)^2)], length(S), length(S)) * q
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS     = (S * (I_pre + I_asy + I_mld + I_sev) %*% -beta)
      dE     = (S * (I_pre + I_asy + I_mld + I_sev) %*% beta) - (gamma * E)
      dI_pre = (gamma * E) - (theta * I_pre)
      dI_asy = (theta * p * I_pre) - (delta_1 * I_asy)
      dI_mld = (theta * (1-p) * I_pre) - (psi + delta_2) * I_mld
      dI_sev = (psi * I_mld) - (omega * I_sev)
      dI_hsp = (phi_1 * omega * I_sev) - (delta_3 + tau_1) * I_hsp
      dI_icu = (1 - phi_1) * omega * I_sev - (delta_4 + tau_2) * I_icu
      dR     = (delta_1 * I_asy) + (delta_2 * I_mld) + (delta_3 * I_hsp) + (delta_4 * I_icu)
      
      # combine results
      results = c (dS, dE, dI_pre, dI_asy, dI_mld, dI_sev, dI_hsp, dI_icu, dR)
      list (results)
    }
  )
}

R0_sub_full_agestr_mixed <- function(transmission_val = 0.051,
                                     gamma_val = gamma_value,
                                     theta_val = theta_value,
                                     betaff_val = matrix_frail_frail,
                                     betann_val = matrix_nonfrail_nonfrail,
                                     betapp_val = matrix_prefrail_prefrail,
                                     betanf_val = matrix_nonfrail_frail,
                                     betafn_val = matrix_frail_nonfrail,
                                     betafp_val = matrix_frail_prefrail,
                                     betapf_val = matrix_prefrail_frail,
                                     betapn_val = matrix_prefrail_nonfrail,
                                     betanp_val = matrix_nonfrail_prefrail,
                                     p_val = p_value,
                                     delta1_val = delta_1_value,
                                     delta2_val = delta_2_value,
                                     delta3_val = delta_3_value,
                                     delta4_val = delta_4_value,
                                     tau1_val = tau_1_value,
                                     tau2_val = tau_2_value,
                                     psi_val = psi_value,
                                     omega_val = omega_value,
                                     phi1_val = phi_1_value,
                                     mat_frail_val = mat_frail, 
                                     mat_prefrail_val = mat_prefrail,
                                     mat_nonfrail_val = mat_nonfrail,
                                     N_frail = pop_frail,
                                     N_prefrail = pop_prefrail,
                                     N_nonfrail = pop_nonfrail){
  
  betaff_vals = betaff_val * transmission_val * mat_frail_val
  betann_vals = betann_val * transmission_val * mat_nonfrail_val
  betapp_vals = betapp_val * transmission_val * mat_prefrail_val
  betanf_vals = betanf_val * transmission_val * mat_frail_val
  betafn_vals = betafn_val * transmission_val * mat_nonfrail_val
  betafp_vals = betafp_val * transmission_val * mat_prefrail_val
  betapf_vals = betapf_val * transmission_val * mat_frail_val
  betapn_vals = betapn_val * transmission_val * mat_nonfrail_val
  betanp_vals = betanp_val * transmission_val * mat_prefrail_val
  
  temp <- list()
  for(i in 1:length(betaff_vals)){
    F_mat = matrix(c(rep(0, 3), rep(c(betaff_vals[i], betafp_vals[i], betafn_vals[i]), 4), rep(0, 6),
                     rep(0, 3), rep(c(betapf_vals[i], betapp_vals[i], betapn_vals[i]), 4), rep(0, 6),
                     rep(0, 3), rep(c(betanf_vals[i], betanp_vals[i], betann_vals[i]), 4), rep(0, 6),
                     rep(rep(0, 21), 18)), 21, 21, byrow = T)
    
    V_mat = matrix(
      c(-gamma_val, rep(0, 20),
        0, -gamma_val, rep(0, 19),
        0, 0, -gamma_val, rep(0, 18),
        gamma_val, 0, 0, -theta_val, rep(0, 17),
        0, gamma_val, 0, 0, -theta_val, rep(0, 16),
        0, 0, gamma_val, 0, 0, -theta_val, rep(0, 15),
        rep(0, 3), theta_val*p_val, 0, 0, -delta1_val, rep(0, 14),
        rep(0, 4), theta_val*p_val, 0, 0, -delta1_val, rep(0, 13),
        rep(0, 5), theta_val*p_val, 0, 0, -delta1_val, rep(0, 12),
        rep(0, 3), theta_val*(1-p_val), rep(0, 5), -(psi_val + delta2_val), rep(0, 11),
        rep(0, 4), theta_val*(1-p_val), rep(0, 5), -(psi_val + delta2_val), rep(0, 10),
        rep(0, 5), theta_val*(1-p_val), rep(0, 5), -(psi_val + delta2_val), rep(0, 9),
        rep(0, 9), psi_val, 0, 0, -omega_val, rep(0, 8),
        rep(0, 10), psi_val, 0, 0, -omega_val, rep(0, 7),
        rep(0, 11), psi_val, 0, 0, -omega_val, rep(0, 6),
        rep(0, 12), (phi1_val * omega_val), 0, 0, -(delta3_val + tau1_val), rep(0, 5),
        rep(0, 13), (phi1_val * omega_val), 0, 0, -(delta3_val + tau1_val), rep(0, 4),
        rep(0, 14), (phi1_val * omega_val), 0, 0, -(delta3_val + tau1_val), rep(0, 3),
        rep(0, 12), (1-(phi1_val * omega_val)), rep(0, 5), -(delta4_val + tau2_val), rep(0, 2),
        rep(0, 13), (1-(phi1_val * omega_val)), rep(0, 5), -(delta4_val + tau2_val), rep(0, 1),
        rep(0, 14), (1-(phi1_val * omega_val)), rep(0, 5), -(delta4_val + tau2_val)
      ), 21, 21, byrow = T
    )
    
    temp[i] <- max(abs(eigen(F_mat %*% solve(V_mat))$values))
  }
  temp_unlist <- unlist(temp)
  proportion_temp <- population_vector/sum(population_vector)
  
  R0_weighted = proportion_temp[1] * sum(temp_unlist[1:6]) +
    proportion_temp[2] * sum(temp_unlist[7:12]) +
    proportion_temp[3] * sum(temp_unlist[13:18]) +
    proportion_temp[4] * sum(temp_unlist[19:24]) +
    proportion_temp[5] * sum(temp_unlist[25:30]) +
    proportion_temp[6] * sum(temp_unlist[31:36])
  
  return(list(R0_weighted = round(R0_weighted, 3)))
}

calculate_q_full_agestr_mixed <- function(R0_input = 1.3,
                                    betaff_vals = matrix_frail_frail,
                                    betann_vals = matrix_nonfrail_nonfrail,
                                    betapp_vals = matrix_prefrail_prefrail,
                                    betanf_vals = matrix_nonfrail_frail,
                                    betafn_vals = matrix_frail_nonfrail,
                                    betafp_vals = matrix_frail_prefrail,
                                    betapf_vals = matrix_prefrail_frail,
                                    betapn_vals = matrix_prefrail_nonfrail,
                                    betanp_vals = matrix_nonfrail_prefrail) {
  print(paste0("calculating q..."))
  initial_top = 1
  initial_bottom = 0
  qran <- (initial_top + initial_bottom)/2
  R0_temp <- R0_sub_full_agestr_mixed(transmission_val = qran,
                                betaff_val = betaff_vals,
                                betann_val = betann_vals,
                                betapp_val = betapp_vals,
                                betanf_val = betanf_vals,
                                betafn_val = betafn_vals,
                                betafp_val = betafp_vals,
                                betapf_val = betapf_vals,
                                betapn_val = betapn_vals,
                                betanp_val = betanp_vals)$R0_weighted
  
  iter = 0
  max_iter = 50
  while (abs((R0_input - R0_temp)) > 0.001 & iter < max_iter) {
    if (R0_temp > R0_input) {
      initial_top <- qran
    } else {
      initial_bottom <- qran
    }
    qran <- (initial_top + initial_bottom)/2
    R0_temp <- R0_sub_full_agestr_mixed(transmission_val = qran,
                                  betaff_val = betaff_vals,
                                  betann_val = betann_vals,
                                  betapp_val = betapp_vals,
                                  betanf_val = betanf_vals,
                                  betafn_val = betafn_vals,
                                  betafp_val = betafp_vals,
                                  betapf_val = betapf_vals,
                                  betapn_val = betapn_vals,
                                  betanp_val = betanp_vals)$R0_weighted
    
    iter = iter + 1
    print(R0_temp)
  }
  return(qran)
}

# ----
# Check R0...
# --- 
R0_full_ass <- function(beta_vals = beta_val,
                        n_vals = mat_nonfrail,
                        q_input = 0.051) {
  
  beta = max(Re(eigen(beta_vals * n_vals * q_input)$values))
  
  output = abs(((p_value * beta * delta_1_value * theta_value * psi_value) - (beta * delta_1_value * theta_value * psi_value) - 
                  (beta * delta_1_value * delta_2_value * omega_value) + (p_value * beta * delta_1_value * theta_value * omega_value) -
                  (beta * delta_1_value * theta_value * omega_value) - (p_value * beta * delta_2_value * theta_value * omega_value) - 
                  (beta * delta_1_value * psi_value * omega_value) - (p_value * beta * theta_value * psi_value * omega_value)) / 
                 ((delta_1_value * delta_2_value * theta_value * omega_value) + (delta_1_value * theta_value * psi_value * omega_value)))
  
  return(output)
}

calculate_q <- function(R0_input = 1.3,
                        beta_val = matrix_frail_frail,
                        n_val = mat_frail) {
  
  print(paste0("calculating q..."))
  initial_top = 1
  initial_bottom = 0
  qran <- (initial_top + initial_bottom)/2
  R0_temp <- R0_full_ass(beta_vals = beta_val,
                         n_vals = n_val,
                         q_input = qran)
  
  iter = 0
  max_iter = 50
  while (abs((R0_input - R0_temp)) > 0.001 & iter < max_iter) {
    if (R0_temp > R0_input) {
      initial_top <- qran
    } else {
      initial_bottom <- qran
    }
    qran <- (initial_top + initial_bottom)/2
    R0_temp <- R0_full_ass(beta_vals = beta_val,
                           n_vals = n_val,
                           q_input = qran)
    
    iter = iter + 1
    print(R0_temp)
  }
  return(qran)
}
