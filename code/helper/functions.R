set_data_structure <- function(dataset = data){
  
  dataset <- dataset %>% 
    mutate_if(str_detect(names(.), "_date"), ymd) %>% 
    mutate_if(is.character, as.factor)
  
  return(dataset)
}

set_participant_factor <- function(dataset = part_data){
  dataset <- dataset %>% 
    mutate(wave = fct_relevel(wave, c("Wave 1", "Wave 2", "Wave 3", "No answer")),
           part_age = fct_relevel(part_age,
                                   c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90-100", "No answer")),
           sample_source = fct_relevel(sample_source,
                                       c("NAT REP","WZC")),
           sample_method = fct_relevel(sample_method,
                                       c("PAPER", "CAWI", "APP")),
           help_required = fct_relevel(help_required,
                                       c("Self answered", "With the help of family, friend or others",
                                         "No answer")),
           part_gender = fct_relevel(part_gender,
                                c("Male", "Female", "Others", "No answer")),
           occupation = fct_relevel(occupation,
                                    c("Full time", "Part time", "Student job", "Volunteer", "Unemployed", "Not applicable", "No answer")),
           education = fct_relevel(education,
                                   c("Undergraduate degree", "Diploma of primary education", "Diploma of secondary education",
                                     "Certificate second degree of secondary education", "Postgraduate degree", "No official diploma", "No answer")),
           covid_vaccine = fct_relevel(covid_vaccine,
                                       c("1 dose", "2 doses", "3 or more doses", "Not yet", "Do not wish to receive",
                                         "Prefer not to say", "No answer")),
           flu_vaccine = fct_relevel(flu_vaccine,
                                     c("Yes", "No", "Prefer not to say", "No answer")),
           chronic_status = fct_relevel(chronic_status,
                                        c("Chronic condition", "No chronic condition", "No answer")),
           ili_status = fct_relevel(ili_status, 
                                    c("ILI symptoms", "No ILI symptoms", "No answer")),
           resident_status = fct_relevel(resident_status,
                                              c("Residential unit", "With my parents", "Family other than parents",
                                                "ROB", "RVT", "Others", "No answer")),
           lhf_info = fct_relevel(lhf_info,
                                           c("Less than 10 people", "Between 10 and 30 people", 
                                             "Between 30 and 50 people", "Between 50 and 100 people",
                                             "More than 100 people", "No answer")),
           hh_size = fct_relevel(hh_size,
                                        c("1", "2", "3", "4", "5+", "ROB/RVT", "No answer")),
           is_holiday = fct_relevel(is_holiday,
                                    c("Yes", "No", "No answer")),
           year = fct_relevel(year, c("2022", "2023", "No answer"))
    )
  
  return(dataset)
}

set_contact_factor <- function(dataset = part_data){
  dataset <- dataset %>% 
    mutate(cnt_age = fct_relevel(cnt_age,
                                  c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                                    "60-69", "70-79", "80-89", "90-100", "No answer")),
           cnt_day = fct_relevel(cnt_day,
                                 c("Monday", "Tuesday", "Wednesday", "Thursday",
                                   "Friday", "Saturday", "Sunday", "No answer")),
           cnt_weekend = fct_relevel(cnt_weekend,
                                     c("Weekday", "Weekend", "No answer")),
           cnt_gender = fct_relevel(cnt_gender,
                                    c("Male", "Female", "Others", "No answer")),
           cnt_role = fct_relevel(cnt_role,
                                  c("A household member", "A non-household family member", "A friend",
                                    "A caregiver", "Others", "No answer")),
           is_physical = fct_relevel(is_physical,
                                     c("Yes", "No", "No answer")),
           frequency_multi = fct_relevel(frequency_multi,
                                         c("(Almost) every day", "A few times per week",
                                           "A few times per month", "A few times per year or less",
                                           "First time", "No answer")),
           cnt_duration = fct_relevel(cnt_duration,
                                      c("< 5 minutes", "5-15 minutes",
                                        "15 min-1 hour", "1-4 hours",
                                        ">= 4 hours", "No answer")),
           is_facemask = fct_relevel(is_facemask,
                                     c("Yes, the whole time", "Yes, some of the time",
                                       "No", "No answer")),
           cnt_home = fct_relevel(cnt_home,
                                  c("Yes", "No")),
           cnt_work = fct_relevel(cnt_work,
                                  c("Yes", "No")),
           cnt_transport = fct_relevel(cnt_transport,
                                       c("Yes", "No")),
           cnt_school = fct_relevel(cnt_school,
                                    c("Yes", "No")),
           cnt_leisure = fct_relevel(cnt_leisure,
                                     c("Yes", "No")),
           cnt_others = fct_relevel(cnt_others,
                                    c("Yes", "No")))
  
  return(dataset)
}

data_with_frailty_index <- function(data = participant_data){
  data = data %>% 
    mutate(Q1_index = case_when(Q1 == "No answer" ~ NA,
                                Q1 == "Excellent" ~ 0,
                                Q1 == "Very good" ~ 0,
                                Q1 == "Good" ~ 0,
                                Q1 == "Fair" ~ 0.5,
                                Q1 == "Poor" ~ 1),
           Q11A_index = case_when(Q11A == "No answer" ~ NA,
                                  Q11A == "Definitely true" ~ 1,
                                  Q11A == "Mostly true" ~ 0.5,
                                  Q11A == "Don't know" ~ 0,
                                  Q11A == "Mostly false" ~ 0,
                                  Q11A == "Definitely false" ~ 0),
           Q11B_index = case_when(Q11B == "No answer" ~ NA,
                                  Q11B == "Definitely true" ~ 0,
                                  Q11B == "Mostly true" ~ 0,
                                  Q11B == "Don't know" ~ 0,
                                  Q11B == "Mostly false" ~ 0.5,
                                  Q11B == "Definitely false" ~ 1),
           Q11C_index = case_when(Q11C == "No answer" ~ NA,
                                  Q11C == "Definitely true" ~ 1,
                                  Q11C == "Mostly true" ~ 0.5,
                                  Q11C == "Don't know" ~ 0,
                                  Q11C == "Mostly false" ~ 0,
                                  Q11C == "Definitely false" ~ 0),
           Q11D_index = case_when(Q11D == "No answer" ~ NA,
                                  Q11D == "Definitely true" ~ 0,
                                  Q11D == "Mostly true" ~ 0,
                                  Q11D == "Don't know" ~ 0,
                                  Q11D == "Mostly false" ~ 0.5,
                                  Q11D == "Definitely false" ~ 1),
           Q3A_index = case_when(Q3A == "No answer" ~ NA,
                                 Q3A == "No, Not Limited at all" ~ 0,
                                 Q3A == "Yes, Limited a Little" ~ 0.5,
                                 Q3A == "Yes, Limited a lot" ~ 1),
           Q3B_index = case_when(Q3B == "No answer" ~ NA,
                                 Q3B == "No, Not Limited at all" ~ 0,
                                 Q3B == "Yes, Limited a Little" ~ 0.5,
                                 Q3B == "Yes, Limited a lot" ~ 1),
           Q3C_index = case_when(Q3C == "No answer" ~ NA,
                                 Q3C == "No, Not Limited at all" ~ 0,
                                 Q3C == "Yes, Limited a Little" ~ 0.5,
                                 Q3C == "Yes, Limited a lot" ~ 1),
           Q3D_index = case_when(Q3D == "No answer" ~ NA,
                                 Q3D == "No, Not Limited at all" ~ 0,
                                 Q3D == "Yes, Limited a Little" ~ 0.5,
                                 Q3D == "Yes, Limited a lot" ~ 1),
           Q3E_index = case_when(Q3E == "No answer" ~ NA,
                                 Q3E == "No, Not Limited at all" ~ 0,
                                 Q3E == "Yes, Limited a Little" ~ 0.5,
                                 Q3E == "Yes, Limited a lot" ~ 1),
           Q3F_index = case_when(Q3F == "No answer" ~ NA,
                                 Q3F == "No, Not Limited at all" ~ 0,
                                 Q3F == "Yes, Limited a Little" ~ 0.5,
                                 Q3F == "Yes, Limited a lot" ~ 1),
           Q3G_index = case_when(Q3G == "No answer" ~ NA,
                                 Q3G == "No, Not Limited at all" ~ 0,
                                 Q3G == "Yes, Limited a Little" ~ 0.5,
                                 Q3G == "Yes, Limited a lot" ~ 1),
           Q3H_index = case_when(Q3H == "No answer" ~ NA,
                                 Q3H == "No, Not Limited at all" ~ 0,
                                 Q3H == "Yes, Limited a Little" ~ 0.5,
                                 Q3H == "Yes, Limited a lot" ~ 1),
           Q3I_index = case_when(Q3I == "No answer" ~ NA,
                                 Q3I == "No, Not Limited at all" ~ 0,
                                 Q3I == "Yes, Limited a Little" ~ 0.5,
                                 Q3I == "Yes, Limited a lot" ~ 1),
           Q3J_index = case_when(Q3J == "No answer" ~ NA,
                                 Q3J == "No, Not Limited at all" ~ 0,
                                 Q3J == "Yes, Limited a Little" ~ 0.5,
                                 Q3J == "Yes, Limited a lot" ~ 1),
           Q9A_index = case_when(Q9A == "No answer" ~ NA,
                                 Q9A == "All of the time" ~ 0,
                                 Q9A == "Most of of the time" ~ 0,
                                 Q9A == "Some of the time" ~ 0,
                                 Q9A == "A little of the time" ~ 0.5,
                                 Q9A == "None of the time" ~ 1),
           Q9B_index = case_when(Q9B == "No answer" ~ NA,
                                 Q9B == "All of the time" ~ 1,
                                 Q9B == "Most of of the time" ~ 0.5,
                                 Q9B == "Some of the time" ~ 0,
                                 Q9B == "A little of the time" ~ 0,
                                 Q9B == "None of the time" ~ 0),
           Q9C_index = case_when(Q9C == "No answer" ~ NA,
                                 Q9C == "All of the time" ~ 1,
                                 Q9C == "Most of of the time" ~ 0.5,
                                 Q9C == "Some of the time" ~ 0,
                                 Q9C == "A little of the time" ~ 0,
                                 Q9C == "None of the time" ~ 0),
           Q9D_index = case_when(Q9D == "No answer" ~ NA,
                                 Q9D == "All of the time" ~ 0,
                                 Q9D == "Most of of the time" ~ 0,
                                 Q9D == "Some of the time" ~ 0,
                                 Q9D == "A little of the time" ~ 0.5,
                                 Q9D == "None of the time" ~ 1),
           Q9E_index = case_when(Q9E == "No answer" ~ NA,
                                 Q9E == "All of the time" ~ 0,
                                 Q9E == "Most of of the time" ~ 0,
                                 Q9E == "Some of the time" ~ 0,
                                 Q9E == "A little of the time" ~ 0.5,
                                 Q9E == "None of the time" ~ 1),
           Q9F_index = case_when(Q9F == "No answer" ~ NA,
                                 Q9F == "All of the time" ~ 1,
                                 Q9F == "Most of of the time" ~ 0.5,
                                 Q9F == "Some of the time" ~ 0,
                                 Q9F == "A little of the time" ~ 0,
                                 Q9F == "None of the time" ~ 0),
           Q9G_index = case_when(Q9G == "No answer" ~ NA,
                                 Q9G == "All of the time" ~ 1,
                                 Q9G == "Most of of the time" ~ 0.5,
                                 Q9G == "Some of the time" ~ 0,
                                 Q9G == "A little of the time" ~ 0,
                                 Q9G == "None of the time" ~ 0),
           Q9H_index = case_when(Q9H == "No answer" ~ NA,
                                 Q9H == "All of the time" ~ 0,
                                 Q9H == "Most of of the time" ~ 0,
                                 Q9H == "Some of the time" ~ 0,
                                 Q9H == "A little of the time" ~ 0.5,
                                 Q9H == "None of the time" ~ 1),
           Q9I_index = case_when(Q9I == "No answer" ~ NA,
                                 Q9I == "All of the time" ~ 1,
                                 Q9I == "Most of of the time" ~ 0.5,
                                 Q9I == "Some of the time" ~ 0,
                                 Q9I == "A little of the time" ~ 0,
                                 Q9I == "None of the time" ~ 0),
           Q2_index = case_when(Q2 == "No answer" ~ NA,
                                Q2 == "Much worse than one year ago" ~ 1,
                                Q2 == "Somewhat worse now than one year ago" ~ 0.5,
                                Q2 == "About the same" ~ 0,
                                Q2 == "Somewhat better now than one year ago" ~ 0,
                                Q2 == "Somewhat worse now than one year ago" ~ 0),
           EQ5D_MOB_index = case_when(EQ5D_MOB == "No answer" ~ NA,
                                      EQ5D_MOB == "No problems in strolling" ~ 0,
                                      EQ5D_MOB == "Slight problems in strolling" ~ 0.5,
                                      EQ5D_MOB == "Moderate problems in strolling" ~ 0.5,
                                      EQ5D_MOB == "Severe problems in strolling" ~ 1,
                                      EQ5D_MOB == "Unable to stroll" ~ 1),
           EQ5D_ANX_index = case_when(EQ5D_ANX == "No answer" ~ NA,
                                      EQ5D_ANX == "Not anxious or depressed" ~ 0,
                                      EQ5D_ANX == "Sligthly anxious or depressed" ~ 0.5,
                                      EQ5D_ANX == "Moderately anxious or depressed" ~ 0.5,
                                      EQ5D_ANX == "Severely anxious or depressed" ~ 1,
                                      EQ5D_ANX == "Extremely anxious or depressed" ~ 1),
           EQ5D_SELF_index = case_when(EQ5D_SELF == "No answer" ~ NA,
                                       EQ5D_SELF == "No problems washing or dressing myself" ~ 0,
                                       EQ5D_SELF == "Slight problems washing or dressing myself" ~ 0.5,
                                       EQ5D_SELF == "Moderate problems washing or dressing myself" ~ 0.5,
                                       EQ5D_SELF == "Severe problems washing or dressing myself" ~ 1,
                                       EQ5D_SELF == "Unable to wash or dress myself" ~ 1),
           EQ5D_ACT_index = case_when(EQ5D_ACT == "No answer" ~ NA,
                                      EQ5D_ACT == "No problems doing my usual activities" ~ 0,
                                      EQ5D_ACT == "Slight problems doing my usual activities" ~ 0.5,
                                      EQ5D_ACT == "Moderate problems doing my usual activities" ~ 0.5,
                                      EQ5D_ACT == "Severe problems doing my usual activities" ~ 1,
                                      EQ5D_ACT == "Unable to do my usual activities" ~ 1),
           EQ5D_DIS_index = case_when(EQ5D_DIS == "No answer" ~ NA,
                                      EQ5D_DIS == "No pain or discomfort" ~ 0,
                                      EQ5D_DIS == "Slight pain or discomfort" ~ 0.5,
                                      EQ5D_DIS == "Moderate pain or discomfort" ~ 0.5,
                                      EQ5D_DIS == "Severe pain or discomofort" ~ 1,
                                      EQ5D_DIS == "Extreme pain or discomfort" ~ 1),
           MH01_index = case_when(MH01 == NA ~ NA,
                                  MH01 == "Yes" ~ 1,
                                  MH01 == "No" ~ 0),
           MH02_index = case_when(MH02 == NA ~ NA,
                                  MH02 == "Yes" ~ 1,
                                  MH02 == "No" ~ 0),
           MH03_index = case_when(MH03 == NA ~ NA,
                                  MH03 == "Yes" ~ 1,
                                  MH03 == "No" ~ 0),
           MH04_index = case_when(MH04 == NA ~ NA,
                                  MH04 == "Yes" ~ 1,
                                  MH04 == "No" ~ 0),
           MH05_index = case_when(MH05 == NA ~ NA,
                                  MH05 == "Yes" ~ 1,
                                  MH05 == "No" ~ 0),
           MH06_index = case_when(MH06 == NA ~ NA,
                                  MH06 == "Yes" ~ 1,
                                  MH06 == "No" ~ 0),
           MH07_index = case_when(MH07 == NA ~ NA,
                                  MH07 == "Yes" ~ 1,
                                  MH07 == "No" ~ 0),
           MH08_index = case_when(MH08 == NA ~ NA,
                                  MH08 == "Yes" ~ 1,
                                  MH08 == "No" ~ 0),
           MH09_index = case_when(MH09 == NA ~ NA,
                                  MH09 == "Yes" ~ 1,
                                  MH09 == "No" ~ 0),
           MH10_index = case_when(MH10 == NA ~ NA,
                                  MH10 == "Yes" ~ 1,
                                  MH10 == "No" ~ 0),
           MH11_index = case_when(MH11 == NA ~ NA,
                                  MH11 == "Yes" ~ 1,
                                  MH11 == "No" ~ 0),
           MH12_index = case_when(MH12 == NA ~ NA,
                                  MH12 == "Yes" ~ 1,
                                  MH12 == "No" ~ 0))
  
  data <- as.data.frame(data)
  data$frailty_score <- ifelse(rowSums(is.na(data[grep("*_index$", names(data))])) == 41, # all answer is empty
                               "missing",
                               rowSums(data[grep("*_index$", names(data))], na.rm = T)/(41-rowSums(is.na(data[grep("*_index$", names(data))]))))
  data$frailty_score <- ifelse(data$frailty_score == "missing", "missing", ifelse(
    data$frailty_score > 0.25, "frail", ifelse(data$frailty_score > 0.08, "pre-frail", ifelse(data$frailty_score <= 0.08, "non-frail", data$frailty_score))))
  data$frailty_score <- ifelse(data$frailty_score != "frail" & rowSums(is.na(data[grep("*_index$", names(data))])) > 10, "missing", data$frailty_score)
  
  data$frailty_score <- ifelse(data$age <= 2 & is.na(data$age) == F, "non-frail", data$frailty_score)

  data <- data %>% 
    select(-matches("^EQ5D|^Q|^MH"))
  
  data$frailty_score <- factor(data$frailty_score,
                               levels = c("frail", "pre-frail", "non-frail", "missing"))
  
  return(data) 
}

fplot.scm <- function(dum_ci = dum_ci, color_scm_min = NULL, color_scm_max = NULL,
                      legendBrks = NULL,
                      legendLabels = NULL,
                      size = 20){
  
  plot_scm <- ggplot(dum_ci,aes(x=age.group,y=contact.age.group,fill=mean)) +
    geom_tile() + 
    scale_fill_viridis_c(limits = c(color_scm_min,
                                    color_scm_max),
                         oob = scales::squish,
                         breaks = legendBrks,
                         name ="Average number \n of contacts",
                         labels=legendLabels) +
    geom_text(aes(label=paste(formatC(round(mean, 2),2,format="f"), "\n",
                              "[", formatC(round(lower,2),2,format="f"), "; ",
                              formatC(round(upper,2),2,format="f"), "]", sep = ""),
                  colour=text_color),show.legend = FALSE, size = 6.5, fontface = "bold") + 
    scale_colour_manual(values=c("white","black")) +
    ylab("Contact age group") +
    xlab("Participant age group") +
    theme_update(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    theme(text = element_text(size = size),
          legend.position = "none") 
  
  return(plot_scm)
}

# ---- create matrices for the confidence interval of the mathematical models
create_dummy_cm <- function(input_part = participants_decompose,
                            input_cont = contact_consent,
                            nboot = 2) {
  
  dim_part = dim(input_part)
  # Create a list to store bootstrap samples
  bootstrap_samples <- list()
  
  # Perform stratified bootstrap
  strata_vars <- c("age_group", "frailty_score")
  # Group data by strata
  strata <- split(input_part, input_part[, strata_vars])
  
  colnames_part <- colnames(input_part)
  colnames_cnt <- colnames(input_cont)
  
  # Perform bootstrap resampling
  for (i in 1:nboot) {
    print(i)
    part_sample <- do.call(rbind, lapply(strata, function(group) {
      group[sample(1:nrow(group), size = nrow(group), replace = TRUE), ]
    }))
    rownames(part_sample) <- NULL
    part_sample$indicator <- ave(seq_along(part_sample$part_id), part_sample$part_id, FUN = seq_along)
    bootstrap_samples[[i]] <- part_sample
  }
  
  part_temp <- lapply(1:nboot, function(x) merge(bootstrap_samples[[x]], input_cont, by = c("part_id", "wave", "sday_date")))
  
  part_temp <- lapply(part_temp, function(df) {
    df$part_id <- paste0(df$part_id, "_", df$indicator)
    return(df) # Ensure the modified data frame is returned
  })
  
  bootstrap_samples <- lapply(bootstrap_samples, function(df) {
    df$part_id <- paste0(df$part_id, "_", df$indicator)
    return(df) # Ensure the modified data frame is returned
  })
  
  cont_sample <- lapply(part_temp, function(x) x[,colnames(x) %in% colnames_cnt])
  
  return(list(part_dummy = bootstrap_samples, 
              cnt_dummy = cont_sample))
}

