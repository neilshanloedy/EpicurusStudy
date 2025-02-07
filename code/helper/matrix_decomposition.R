##################################################################################--
## This script provides a function to decompose a contact matrix M into
## a more refine structure
## 
## e.g., M_y = sum_{y = 1}^{y'} My * Wy
## and even into a more deeper structure
## 
## e.g., My = sum_{y = 1}^{y'} My,y'.
##
## Author: Neilshan Loedy
## last update: 17/04/2023
## update (17/04/2023) --> update the way matrix are calculated. Previously it was Cji instead of Cij
##
##################################################################################--

#' @param participants : input dataset of the participants
#' @param contacts : input dataset of the contacts
#' @param age_input : the age groups
#' @param frailty_input : frailty level that aims to be decomposed
#' @param assortativity_frail : assortativity between frail input with frail individuals
#' @param assortativity_prefrail : assortativity between frail input with non-frail individuals
#' @param assortativity_nonfrail : 1 - assortativity_frail - assortativity_prefrail
#' @param population : population data from 2022 STATBEL. We assume that the frailty percentage is the same with what was obtained from the data

# matrix decomposition function
#-------------------------------

decompose_contact <- function(participants = participants,
                              contacts = contact_consent,
                              age_input = c(0, 50, 60, 70, 80, 90, 101),
                              frailty_i = "frail",
                              assortativity_frail = 1,
                              assortativity_prefrail = 0, 
                              population = population_data,
                              per_capita = TRUE){
  
  if(assortativity_frail + assortativity_prefrail > 1){
    stop("assortativity index is greater than 1")
  }
  
  assortativity_nonfrail = 1 - (assortativity_frail + assortativity_prefrail)
  
  survey_participants_frailty <- participants %>% 
    dplyr::select(part_id, age, gender, sday_date, frailty_score) %>% 
    mutate(country = "Belgium",
           year = year(sday_date)) %>% 
    dplyr::rename(part_age = age,
                  part_gender = gender)
  
  survey_participants_frailty$age_group <- cut(survey_participants_frailty$part_age, 
                                               breaks = age_input, 
                                               labels = paste0(age_input[-length(age_input)], "-", age_input[-1]-1), 
                                               include.lowest = T,
                                               right = F)
  
  population_data$age_group <- cut(population_data$age, 
                                   breaks = age_input, 
                                   labels = paste0(age_input[-length(age_input)], "-", age_input[-1]-1), 
                                   include.lowest = T,
                                   right = F)
  
  population <- population_data %>% group_by(age_group) %>% 
    summarise(sum = sum(sum))
  
  survey_participants_frailty$sday <- weekdays(survey_participants_frailty$sday_date)
  survey_participants_frailty$dayofweek <- match(survey_participants_frailty$sday, weekdays_vector) - 1
  survey.data.frailty <- survey(survey_participants_frailty, contacts)
  
  # the filtered matrix
  output_filter <- contact_matrix(survey.data.frailty,
                                  age.limits = age_input,
                                  n = N_boot_matrix,
                                  estimated.contact.age = "sample",
                                  weigh.dayofweek = T,
                                  filter = list(frailty_score = frailty_i))
  
  dum_output_filter <- output.matrix(output_filter)
  filter_mat <- matrix(dum_output_filter$mean, nrow = length(age_input)-1, length(age_input)-1, byrow = T)
  
  # calculate the proportion of frail individuals based on the survey's participant
  proportion_survey <- survey_participants_frailty %>% 
    filter(frailty_score != "missing" & is.na(age_group) == F) %>% 
    group_by(frailty_score, age_group) %>% 
    summarise(n = n()) %>%
    group_by(age_group) %>% 
    mutate(freq = n / sum(n))
  
  frail_proportion <- proportion_survey %>% filter(frailty_score == "frail")
  prefrail_proportion <- proportion_survey %>% filter(frailty_score == "pre-frail")
  nonfrail_proportion <- proportion_survey %>% filter(frailty_score == "non-frail")
  
  frail_proportion <- t(do.call(rbind, replicate(length(levels(survey_participants_frailty$age_group)),
                                                 t(matrix(frail_proportion$freq)),
                                                 simplify=FALSE)))
  prefrail_proportion <- t(do.call(rbind, replicate(length(levels(survey_participants_frailty$age_group)),
                                                    t(matrix(prefrail_proportion$freq)),
                                                    simplify=FALSE)))
  nonfrail_proportion <- t(do.call(rbind, replicate(length(levels(survey_participants_frailty$age_group)),
                                                    t(matrix(nonfrail_proportion$freq)),
                                                    simplify=FALSE)))
  
  w_frail = (assortativity_frail * frail_proportion)/(
    (frail_proportion * assortativity_frail) + (prefrail_proportion * assortativity_prefrail) + (nonfrail_proportion * assortativity_nonfrail)
  )
  w_prefrail = (assortativity_prefrail * prefrail_proportion)/(
    (frail_proportion * assortativity_frail) + (prefrail_proportion * assortativity_prefrail) + (nonfrail_proportion * assortativity_nonfrail)
  )
  w_nonfrail = (assortativity_nonfrail * nonfrail_proportion)/(
    (frail_proportion * assortativity_frail) + (prefrail_proportion * assortativity_prefrail) + (nonfrail_proportion * assortativity_nonfrail)
  )
  
  output_frail <- w_frail * filter_mat
  output_prefrail <- w_prefrail * filter_mat
  output_nonfrail <- w_nonfrail * filter_mat
  assortativity <- data.frame(frailty_index = c("frail", "pre-frail", "non-frail"),
                              assortativity_index = c(assortativity_frail, assortativity_prefrail, assortativity_nonfrail))
  
  if(per_capita == T){
    
    pop_frail <- frail_proportion[,1] * population$sum
    pop_prefrail <- prefrail_proportion[,1] * population$sum
    pop_nonfrail <- nonfrail_proportion[,1] * population$sum
    
    output_frail <- t(t(output_frail) / pop_frail)
    output_prefrail <- t(t(output_prefrail) / pop_prefrail)
    output_nonfrail <- t(t(output_nonfrail) / pop_nonfrail)
    
  }
  
  output <- setNames(
    list(
      levels(survey_participants_frailty$age_group),
      filter_mat,
      output_frail,
      output_prefrail,
      output_nonfrail,
      assortativity,
      w_frail,
      frail_proportion,
      w_prefrail,
      prefrail_proportion,
      w_nonfrail,
      nonfrail_proportion
    ),
    c(
      "age_group",
      paste0(str_replace_all(frailty_i, "-", ""), "_matrix"),
      paste0(str_replace_all(frailty_i, "-", ""), "_frail_matrix"),
      paste0(str_replace_all(frailty_i, "-", ""), "_prefrail_matrix"),
      paste0(str_replace_all(frailty_i, "-", ""), "_nonfrail_matrix"),
      "assortativity",
      "rho_frail",
      "frail_proportion",
      "rho_prefrail",
      "prefrail_proportion",
      "rho_nonfrail",
      "nonfrail_proportion"
    )
  )
  
  return(output)
}

