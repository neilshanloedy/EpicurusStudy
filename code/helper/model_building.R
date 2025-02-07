contact_home <- contact_data[contact_data$cnt_home == "Yes",]
contact_nothome <- contact_data[contact_data$cnt_home == "No",]

# divide participants into different residential status

# hh = 1
participants_alone <- participant_data[participant_data$hh_size == 1,] %>% 
  dplyr::select(-c(sday_date))
dim(participants_alone)

# hh = others
participants_hh <- participant_data[participant_data$hh_size %in% c("2","3","4","5+"),] %>% 
  dplyr::select(-c(sday_date))

# hh = rob/rvt
participants_rob <- participant_data[participant_data$hh_size %in% "ROB/RVT",] %>% 
  dplyr::select(-c(sday_date))

# ---------

dt_home_alone <- merge(participants_alone, 
                       merge(participants_alone, contact_home) %>% 
                         group_by(part_id) %>% 
                         summarise(n_contacts = n()),
                       all.x = T)
dt_home_alone$n_contacts <- ifelse(is.na(dt_home_alone$n_contacts) == T,
                                   0, dt_home_alone$n_contacts)

dt_home_alone <- dt_home_alone %>% 
  mutate(part_age = ifelse(part_age %in% c("0-9", "10-19", "20-29"), "< 30", as.character(part_age)))

dt_nothome_alone <- merge(participants_alone, 
                          merge(participants_alone, contact_nothome) %>% 
                            group_by(part_id) %>% 
                            summarise(n_contacts = n()),
                          all.x = T)
dt_nothome_alone$n_contacts <- ifelse(is.na(dt_nothome_alone$n_contacts) == T,
                                      0, dt_nothome_alone$n_contacts)

dt_nothome_alone <- dt_nothome_alone %>% 
  mutate(part_age = ifelse(part_age %in% c("0-9", "10-19", "20-29"), "< 30", as.character(part_age)))

dt_home <- merge(participants_hh, 
                 merge(participants_hh, contact_home) %>% 
                   group_by(part_id) %>% 
                   summarise(n_contacts = n()),
                 all.x = T)
dt_home$n_contacts <- ifelse(is.na(dt_home$n_contacts) == T,
                             0, dt_home$n_contacts)

dt_nothome <- merge(participants_hh, 
                    merge(participants_hh, contact_nothome) %>% 
                      group_by(part_id) %>% 
                      summarise(n_contacts = n()),
                    all.x = T)
dt_nothome$n_contacts <- ifelse(is.na(dt_nothome$n_contacts) == T,
                                0, dt_nothome$n_contacts)

dt_home$hh_size <- factor(dt_home$hh_size,
                          levels = c("2", "3", "4", "5+")) %>% droplevels()

dt_nothome$hh_size <- factor(dt_nothome$hh_size,
                             levels = c("2", "3", "4", "5+")) %>% droplevels()

dt_rob <- merge(participants_rob, 
                merge(participants_rob, contact_home) %>% 
                  group_by(part_id) %>% 
                  summarise(n_contacts = n()),
                all.x = T)
dt_rob$n_contacts <- ifelse(is.na(dt_rob$n_contacts) == T,
                            0, dt_rob$n_contacts)
dt_rob$frailty_score <- factor(dt_rob$frailty_score,
                               levels = c("frail", "pre-frail", "non-frail", "missing")) %>% droplevels()
dt_rob <- dt_rob %>% 
  mutate(part_age = ifelse(part_age %in% c("30-39", "50-59", "60-69"), "< 70",
                           ifelse(part_age %in% c("70-79", "80-89", "90-100"), "70+", "No answer")))

dt_notrob <- merge(participants_rob, 
                   merge(participants_rob, contact_nothome) %>% 
                     group_by(part_id) %>% 
                     summarise(n_contacts = n()),
                   all.x = T)
dt_notrob$n_contacts <- ifelse(is.na(dt_notrob$n_contacts) == T,
                               0, dt_notrob$n_contacts)
dt_notrob$frailty_score <- factor(dt_notrob$frailty_score,
                                  levels = c("frail", "pre-frail", "non-frail", "missing")) %>% droplevels()
dt_notrob <- dt_notrob %>% 
  mutate(part_age = ifelse(part_age %in% c("30-39", "50-59", "60-69"), "< 70",
                           ifelse(part_age %in% c("70-79", "80-89", "90-100"), "70+", "No answer")))

dt_rob <- dt_rob %>% droplevels()
dt_notrob <- dt_notrob %>% droplevels()

# Create models here
# ------------------

# ROB/RVT
rob_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
                 is_holiday + sample_method +
                  lhf_info + frailty_score + frailty_score:part_age,
               sigma.formula = ~ part_age + sample_method:frailty_score,
               data = na.omit(dt_rob[dt_rob$frailty_score != "missing",]),
               family = "NBIrc",
               method = RS(500))

saveRDS(rob_0, "../rds/model_rob_home.rds")

robo_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
                   is_holiday + sample_method +
                   lhf_info + frailty_score + frailty_score:part_age,
                 sigma.formula = ~ part_age + sample_method:frailty_score,
                 data = na.omit(dt_notrob[dt_notrob$frailty_score != "missing",]),
                 family = "NBIrc",
                 method = RS(1000))

saveRDS(robo_0, "../rds/model_rob_nothome.rds")

# HH 1
al_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age + frailty_score +
                 sample_method + is_holiday + education + occupation,
               sigma.formula = ~ part_age + occupation + is_holiday + frailty_score,
               data = na.omit(dt_home_alone),
                family = "NBIrc",
               method = RS(500))

saveRDS(al_0, "../rds/model_alone_home.rds")

alo_0 <-  gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age + frailty_score +
                   sample_method + is_holiday + education + occupation,
                 sigma.formula = ~ part_age + occupation + is_holiday + frailty_score,
                 data = na.omit(dt_nothome_alone),
                 family = "NBIrc",
                 method = mixed(10, 500))

saveRDS(alo_0, "../rds/model_alone_nothome.rds")

# HH others
oth_19_5 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
                     frailty_score + is_holiday + hh_size + sample_method + occupation + education +
                     part_age*hh_size + frailty_score*sample_method,
                   sigma.formula = ~ part_age + hh_size + sample_method:frailty_score,
                   data = na.omit(dt_home),
                   family = "NBIrc",
                   method = RS(500))

saveRDS(oth_19_5, "../rds/model_household_home.rds")

oth_not21_6 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
                        frailty_score + is_holiday + hh_size + sample_method + occupation +
                        education + part_age*hh_size + frailty_score*sample_method,
                      sigma.formula = ~ part_age + hh_size + sample_method:frailty_score,
                      data = na.omit(dt_nothome),
                      family = "NBIrc",
                      method = RS(500))

saveRDS(oth_not21_6, "../rds/model_household_nothome.rds")

# oth_not0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not0)
# colnames(oth_not0)
# 
# # age + frailty is significant
# oth_not1 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not1)
# colnames(dt_nothome)
# 
# LR.test(oth_not0, oth_not1)
# 
# # part_gender is significant
# oth_not2 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not2)
# LR.test(oth_not1, oth_not2)
# colnames(dt_nothome)
# 
# # is_holiday is significant
# oth_not3 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not3)
# LR.test(oth_not2, oth_not3)
# colnames(dt_nothome)
# 
# oth_not4 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not4)
# LR.test(oth_not3, oth_not4)
# colnames(dt_nothome)
# 
# oth_not5 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not5)
# LR.test(oth_not4, oth_not5)
# colnames(dt_nothome)
# 
# oth_not6 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                      occupation,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not6)
# LR.test(oth_not5, oth_not6)
# colnames(dt_nothome)
# 
# oth_not7 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                      occupation + education,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not7)
# LR.test(oth_not6, oth_not7)
# colnames(dt_nothome)
# 
# oth_not8 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                      occupation + education + cnt_weekend,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not8)
# LR.test(oth_not7, oth_not8)
# colnames(dt_nothome)
# 
# # with interactions
# oth_not9 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                      occupation + education + cnt_weekend + part_age*frailty_score,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not9)
# LR.test(oth_not8, oth_not9)
# colnames(dt_nothome)
# 
# oth_not10 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                      occupation + education + cnt_weekend + part_age*part_gender,
#                    data = na.omit(dt_nothome),
#                    family = "NBIrc")
# 
# summary(oth_not10)
# LR.test(oth_not8, oth_not10)
# colnames(dt_nothome)
# 
# oth_not11 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*is_holiday,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not11)
# LR.test(oth_not8, oth_not11)
# colnames(dt_nothome)
# 
# oth_not12 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*hh_size,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not12)
# LR.test(oth_not8, oth_not12)
# colnames(dt_nothome)
# 
# oth_not13 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*sample_method,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not13)
# LR.test(oth_not8, oth_not13)
# colnames(dt_nothome)
# 
# oth_not14 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*occupation,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not14)
# LR.test(oth_not8, oth_not14)
# colnames(dt_nothome)
# 
# oth_not15 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*education,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not15)
# LR.test(oth_not8, oth_not15)
# colnames(dt_nothome)
# 
# oth_not16 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + part_age*cnt_weekend,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not16)
# LR.test(oth_not8, oth_not16)
# colnames(dt_nothome)
# 
# oth_not17 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + help_required,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not17)
# LR.test(oth_not8, oth_not17)
# colnames(dt_nothome)
# 
# oth_not18 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*hh_size,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not18)
# LR.test(oth_not8, oth_not18)
# colnames(dt_nothome)
# 
# oth_not19 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*sample_method,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not19)
# LR.test(oth_not8, oth_not19)
# colnames(dt_nothome)
# 
# oth_not20 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not20)
# LR.test(oth_not8, oth_not20)
# colnames(dt_nothome)
# 
# oth_not21 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not21)
# LR.test(oth_not20, oth_not21)
# colnames(dt_nothome)
# 
# oth_not22 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend + frailty_score*is_holiday,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not22)
# LR.test(oth_not21, oth_not22)
# colnames(dt_nothome)
# 
# oth_not23 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend + frailty_score*cnt_weekend,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not23)
# LR.test(oth_not21, oth_not23)
# colnames(dt_nothome)
# 
# oth_not24 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend + frailty_score*cnt_weekend + hh_size*cnt_weekend,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not24)
# LR.test(oth_not21, oth_not24)
# colnames(dt_nothome)
# 
# oth_not25 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend + frailty_score*cnt_weekend + hh_size*is_holiday,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# summary(oth_not25)
# LR.test(oth_not21, oth_not25)
# colnames(dt_nothome)
# 
# # ----- contacts at home.
# # age + frailty is significant
# oth_1 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score,
#                    data = na.omit(dt_home),
#                    family = "NBIrc")
# 
# summary(oth_1)
# colnames(dt_home)
# 
# # part_gender is not significant
# oth_2 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                      frailty_score + part_gender,
#                    data = na.omit(dt_home),
#                    family = "NBIrc")
# 
# summary(oth_2)
# LR.test(oth_1, oth_2)
# 
# oth_3 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_3)
# LR.test(oth_1, oth_3)
# 
# oth_4 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_4)
# LR.test(oth_3, oth_4)
# 
# oth_5 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_5)
# LR.test(oth_4, oth_5)
# 
# oth_6 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method + occupation,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_6)
# LR.test(oth_5, oth_6)
# 
# oth_7 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method + occupation +
#                   education,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_7)
# LR.test(oth_6, oth_7)
# 
# oth_8 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method + occupation +
#                   cnt_weekend,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_8)
# LR.test(oth_6, oth_8)
# 
# oth_9 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method + occupation +
#                   cnt_weekend + part_age*frailty_score,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_8)
# LR.test(oth_8, oth_9)
# 
# oth_10 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                   frailty_score + is_holiday + hh_size + sample_method + occupation +
#                   cnt_weekend + part_age*part_gender,
#                 data = na.omit(dt_home),
#                 family = "NBIrc")
# 
# summary(oth_10)
# LR.test(oth_8, oth_10)
# 
# oth_11 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*is_holiday,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# summary(oth_11)
# LR.test(oth_8, oth_11)
# 
# oth_12 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# summary(oth_12)
# LR.test(oth_8, oth_12)
# 
# oth_13 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*sample_method,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# summary(oth_13)
# LR.test(oth_12, oth_13)
# 
# oth_14 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*occupation,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_14)
# LR.test(oth_12, oth_14)
# 
# oth_15 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_15)
# LR.test(oth_12, oth_15)
# 
# oth_16 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + part_age*cnt_weekend,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_16)
# LR.test(oth_15, oth_16)
# 
# oth_17 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + help_required,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_17)
# LR.test(oth_15, oth_17)
# 
# oth_18 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*hh_size,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_18)
# LR.test(oth_15, oth_18)
# 
# oth_19 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_19)
# LR.test(oth_15, oth_19)
# 
# oth_20 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_20)
# LR.test(oth_19, oth_20)
# 
# oth_21 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation + is_holiday*cnt_weekend,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_21)
# LR.test(oth_19, oth_21)
# 
# oth_22 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation + is_holiday*frailty_score,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_22)
# LR.test(oth_19, oth_22)
# 
# oth_23 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation + cnt_weekend*frailty_score,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_23)
# LR.test(oth_19, oth_23)
# 
# oth_24 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation + cnt_weekend*hh_size,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_24)
# LR.test(oth_19, oth_24)
# 
# oth_25 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method +
#                    frailty_score*occupation + is_holiday*hh_size,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# AIC(oth_25)
# LR.test(oth_19, oth_25)
# 
# # ----- final model
# ## ---- contacts not at home
# oth_not21 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                       frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                       occupation + education + cnt_weekend + frailty_score*occupation + 
#                       is_holiday*cnt_weekend,
#                     data = na.omit(dt_nothome),
#                     family = "NBIrc")
# 
# oth_not21_1 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         occupation + education + cnt_weekend + frailty_score*sample_method + frailty_score*occupation + 
#                         is_holiday*cnt_weekend,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# oth_not21_2 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         occupation + education + cnt_weekend + frailty_score*sample_method + 
#                         is_holiday*cnt_weekend,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# AIC(oth_not21_2)
# LR.test(oth_not21_2, oth_not21_1)
# 
# oth_not21_3 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         occupation + cnt_weekend + frailty_score*sample_method + 
#                         is_holiday*cnt_weekend,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# AIC(oth_not21_3)
# LR.test(oth_not21_3, oth_not21_2) # education is significant
# 
# oth_not21_4 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         education + cnt_weekend + frailty_score*sample_method + 
#                         is_holiday*cnt_weekend,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# AIC(oth_not21_3)
# LR.test(oth_not21_4, oth_not21_2) # occupation is significant
# 
# oth_not21_5 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         occupation + education + cnt_weekend + frailty_score*sample_method,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# LR.test(oth_not21_5, oth_not21_2) # holiday and weekend is significant
# 
# # FINAL MODEL COMPARABLE
# oth_not21_6 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + 
#                         frailty_score + part_gender + is_holiday + hh_size + sample_method +
#                         education + cnt_weekend + frailty_score*sample_method + 
#                         is_holiday*cnt_weekend + part_age*hh_size,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc")
# 
# LR.test(oth_not21_4, oth_not21_6)
# 
# 
# # ------
# oth_19_1 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + part_gender +
#                    frailty_score + is_holiday + hh_size + sample_method + occupation +
#                    cnt_weekend + part_age*hh_size + part_age*education + frailty_score*sample_method,
#                  data = na.omit(dt_home),
#                  family = "NBIrc")
# 
# LR.test(oth_19, oth_19_1)
# summary(oth_19_1)
# 
# oth_19_2 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + part_gender +
#                      frailty_score + is_holiday + hh_size + sample_method + occupation + education +
#                      cnt_weekend + part_age*hh_size + frailty_score*sample_method,
#                    data = na.omit(dt_home),
#                    family = "NBIrc")
# 
# oth_19_3 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + part_gender +
#                      frailty_score + is_holiday + hh_size + sample_method + occupation + education +
#                      cnt_weekend + part_age*hh_size + frailty_score*sample_method + part_age*education,
#                    data = na.omit(dt_home),
#                    family = "NBIrc")
# 
# LR.test(oth_19_2, oth_19_3)
# summary(oth_19_3)
# 
# oth_19_4 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ part_age + part_gender +
#                      frailty_score + is_holiday + hh_size + sample_method + occupation + education +
#                      cnt_weekend + part_age*hh_size + frailty_score*sample_method + part_age*education +
#                      is_holiday*cnt_weekend,
#                    data = na.omit(dt_home),
#                    family = "NBIrc")
# 
# # FINAL MODEL COMPARABLE
# oth_19_5 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
#                      frailty_score + is_holiday + hh_size + sample_method + occupation + education +
#                      part_age*hh_size + frailty_score*sample_method,
#                    sigma.formula = ~ part_age + hh_size + sample_method:frailty_score,
#                    data = na.omit(dt_home),
#                    family = "NBIrc",
#                    method = RS(500))
# 
# saveRDS(oth_19_5, "../rds/model_household_home.rds")
# 
# # FINAL MODEL COMPARABLE
# oth_not21_6 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
#                         frailty_score + is_holiday + hh_size + sample_method + occupation +
#                         education + part_age*hh_size + frailty_score*sample_method,
#                       sigma.formula = ~ part_age + hh_size + sample_method:frailty_score,
#                       data = na.omit(dt_nothome),
#                       family = "NBIrc",
#                       method = RS(500))
# 
# summary(oth_not21_6)
# 
# saveRDS(oth_not21_6, "../rds/model_household_nothome.rds")
# 
# # Model for hh_size == 1, contacts at home
# al_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age + frailty_score + 
#                  sample_method + is_holiday + education,
#                sigma.formula = ~ part_age + sample_method + occupation + is_holiday + frailty_score +
#                  sample_method:frailty_score,
#                data = na.omit(dt_home_alone),
#                 family = "NBIrc",
#                method = RS(500))
# 
# saveRDS(al_0, "../rds/model_alone_home.rds")
# 
# alo_0 <-  gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age + frailty_score + 
#                    sample_method + is_holiday + education,
#                  sigma.formula = ~ part_age + sample_method + occupation + is_holiday + frailty_score +
#                    sample_method:frailty_score,
#                  data = na.omit(dt_nothome_alone),
#                  family = "NBIrc",
#                  method = mixed(10, 500))
# 
# saveRDS(alo_0, "../rds/model_alone_nothome.rds")
# 
# # Model for ROB/RVT, contacts at home
# # rob_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 + part_age +
# #                  frailty_score + is_holiday + sample_method + occupation + education +
# #                   lhf_info + frailty_score*sample_method,
# #                sigma.formula = ~ part_age + sample_method:frailty_score,
# #                data = na.omit(dt_rob),
# #                family = "NBIrc",
# #                method = RS(500))
# 
# rob_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 +
#                  is_holiday + sample_method + education +
#                   lhf_info,
#                sigma.formula = ~ part_age + sample_method:frailty_score,
#                data = na.omit(dt_rob),
#                family = "NBIrc",
#                method = RS(500))
# 
# summary(rob_0)
# 
# saveRDS(rob_0, "../rds/model_rob_home.rds")
# 
# robo_0 <- gamlss(Surv(n_contacts, n_contacts <= 30, type = "right") ~ 1 +
#                    is_holiday + sample_method + education +
#                    lhf_info,
#                  sigma.formula = ~ part_age + sample_method:frailty_score,
#                  data = na.omit(dt_notrob),
#                  family = "NBIrc",
#                  method = RS(1000))
# 
# summary(robo_0)
# 
# saveRDS(robo_0, "../rds/model_rob_nothome.rds")
# 
