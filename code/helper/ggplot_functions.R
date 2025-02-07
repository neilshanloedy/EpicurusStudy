ggplot_average_cnt <- function(data_out = data_out) {
  
  nboot = 500
  if(!file.exists("../rds/prediction_intervals_bootstrap.rds")) {
    nrow_out <- nrow(data_out)
    pred <- list()
    for(b in 1:nboot) {
      sample_index <- sample(1:nrow_out, nrow_out, replace = T)
      data_temp <- data_out[sample_index, ]
      model_temp <- gamlss(formula = Surv(n, n <= 30, type = "right") ~      age_group_merge + frailty_score + occupation + sample_source +  
                             sample_method + help_required + household_size +          gender + education + year:wave_season + is_holiday:cnt_weekend +  
                             age_group_merge:is_holiday + help_required:frailty_score,      sigma.formula = ~frailty_score + help_required +  
                             education + household_size + age_group_merge, family = "NBIrc",      data = na.omit(data_temp)) 
      
      data_temp$pred <- predict(model_temp, type = "response") + model_temp$residuals
      
      pred[[b]] <- data_temp %>% 
        group_by(age_group_merge, frailty_score) %>% 
        summarise(q_low = quantile(pred, 0.025),
                  q_upp = quantile(pred, 0.975))
      
    }
    
    pred_df <- do.call("rbind", pred)
    pred_df <- pred_df %>% 
      group_by(age_group_merge, frailty_score) %>% 
      summarise(q_low = mean(q_low),
                q_upp = mean(q_upp)) %>% 
      filter(frailty_score != "Missing") %>% 
      mutate(q_low = ifelse(q_low < 0, 0, q_low))
    saveRDS(pred_df, "../rds/prediction_intervals_bootstrap.rds")
  } else {
    pred_df <- readRDS("../rds/prediction_intervals_bootstrap.rds")
  }
  
  # Filter data to exclude missing frailty scores
  filtered_data <- data_out[data_out$frailty_score != "Missing", ]
  # Plot
  output <- ggplot(data = filtered_data, aes(x = age_group_merge, y = number_of_cnt)) +
    
    # Boxplot with fill aesthetic
    geom_boxplot(aes(y = n, fill = age_group_merge), outliers = F) +
    
    # Mean points
    geom_point(data = filtered_data %>% 
                 group_by(age_group_merge, frailty_score) %>% 
                 summarise(mean = mean(n), .groups = 'drop'),
               aes(y = mean),
               size = 2) +
    geom_smooth(data = pred_df, aes(x = age_group_merge, y = q_low, group = 1),
                method = "gam",   # Change to loess
                formula = y ~ s(x, bs = "cs", k = 5),
                color = "red", 
                linetype = "dashed", 
                se = FALSE) +        
    geom_smooth(data = pred_df, aes(x = age_group_merge, y = q_upp, group = 1),
                method = "gam",   # Change to loess
                formula = y ~ s(x, bs = "cs", k = 5),
                color = "red", 
                linetype = "dashed", 
                se = FALSE) +        
    geom_smooth(method = "gam", 
                formula = y ~ s(x, bs = "cs", k = 5),
                se = T,  
                aes(group = 1),
                colour = "dodgerblue3", 
                size = 1) +
    
    # Axis and theme adjustments
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 18)) +
    ylab("Number of contacts") +
    xlab("Age group") +
    facet_wrap(~ frailty_score)
  
  
  return(output)
}

f_plot_gamlss <- function(data = list.model.short) {
  
  Covariates_plot <- c(
    "Age group (10-19)", "Age group (20-29)", "Age group (30-39)", "Age group (40-49)", "Age group (50-59)",
    "Age group (60-69)", "Age group (70-79)", "Age group (80-89)", "Age group (90-100)", "Age group (NA)",
    "Pre-frail", "Non-frail", "Missing",
    "Part time", "Student job", "Volunteer", "Unemployed", "Not applicable", "Occupation (NA)",
    "Health care sample",
    "CAWI", "APP", 
    "Help required", "Help required (NA)",
    "HH (2)", "HH (3)", "HH (4)", "HH (5+)", "ROB/RVT", "HH (NA)",
    "Female", "Others", "Gender (NA)",
    "Primary education", "Secondary education", "Secondary education (cert.)", "Post graduate", 
    "No diploma", "Education (NA)",
    "2022: Wave 1", "2023: Wave 1", "2022: Wave 2", "2023: Wave 2",
    "Holiday: Weekday", "No-holiday: Weekday", "Holiday: Weekend",
    "Age group (10-19): No-holiday", "Age group (20-29): No-holiday", "Age group (30-39): No-holiday", 
    "Age group (40-49): No-holiday", "Age group (50-59): No-holiday", "Age group (60-69): No-holiday",
    "Age group (70-79): No-holiday", "Age group (80-89): No-holiday", "Age group (90-100): No-holiday",
    "Age group (NA): No-holiday",
    "Pre-frail: Help required", "Non-frail: Help required", "Missing: Help required",
    "Pre-frail: Help required (NA)", "Non-frail: Help required (NA)", "Missing: Help required (NA)"
  )
  
  data_output <- NULL
  for(i in 1:length(data)) {  
    data_input <- cbind(Covariates_plot, data[[i]])
    data_input <- data_input[,-ncol(data_input)]
    
    new_data <- rbind(
      data.frame(
        Covariates_plot = "Age group (0-9)",
        xmin = 1, est = 1, xmax = 1),
      data_input[1:nrow(data_input),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:11),],
      data.frame(
        Covariates_plot = "Frail",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:11),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:15),],
      data.frame(
        Covariates_plot = "Full time",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:15),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:22),],
      data.frame(
        Covariates_plot = "General population sample",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:22),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:24),],
      data.frame(
        Covariates_plot = "Paper",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:24),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:27),],
      data.frame(
        Covariates_plot = "Self answered",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:27),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:30),],
      data.frame(
        Covariates_plot = "HH (1)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:30),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:37),],
      data.frame(
        Covariates_plot = "Male",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:37),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:41),],
      data.frame(
        Covariates_plot = "Undergraduate",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:41),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:48),],
      data.frame(
        Covariates_plot = "2023: Wave 3",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:48),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:53),],
      data.frame(
        Covariates_plot = "No-holiday: Weekend",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:53),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:57),],
      data.frame(
        Covariates_plot = "Age group (0-9): No-holiday",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:57),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:68),],
      data.frame(
        Covariates_plot = "Frail: Help required",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:68),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:72),],
      data.frame(
        Covariates_plot = "Frail: Help required (NA)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:72),])
    rownames(new_data) <- NULL
    
    new_data$order_level <- factor(c(rep(1, 11), rep(2, 4), rep(3, 7), rep(4, 2), rep(5, 3), rep(6, 3),
                                     rep(7, 7), rep(8, 4), rep(9, 7), rep(10, 5), rep(11, 4),
                                     rep(12, 11), rep(13, 4), rep(14, 4)),
                                   levels = seq(1, 18))
    
    new_data$order_in <- c(seq(11, 1), seq(4, 1), seq(7, 1), seq(2, 1), seq(3, 1), seq(3, 1),
                           seq(7, 1), seq(4, 1), seq(7, 1), seq(5, 1), seq(4, 1), seq(11, 1),
                           seq(4, 1), seq(4, 1))
    
    if(i == 1) {
      new_data$type <- "Contacts not at home"
    } else {
      new_data$type <- "Contacts at home"
    }
    
    if(is.null(data_output)) {
      data_output <- new_data 
    } else {
      data_output <- rbind(data_output, new_data)
    }
    
  }
  
  data_output <- data_output %>%
    arrange(order_level, order_in) %>%
    mutate(Covariates_plot = factor(Covariates_plot, levels = unique(Covariates_plot)))
  
  # Create the plot
  output <- ggplot(data_output, aes(x = est, y = Covariates_plot, colour = type)) +
    geom_point(size = 3) +  # Plot points for the estimates
    geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2) +  # Add horizontal error bars
    geom_vline(xintercept = 1, color = "red", lwd = 1) +
    facet_wrap(~ order_level, ncol = 2, scales = "free_y") +  # Facet by order_level, 2 columns
    theme_minimal() +
    scale_colour_manual(values = c("#38b000", "#03045e")) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),  # Remove y-axis title since Covariates_plot are labeled
      axis.text.y = element_text(size = 14),  # Adjust y-axis text size
      text = element_text(size = 18),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.title=element_blank()
    ) +
    labs(x = "Relative number of contacts")
  
  return(output)
  
}

f_plot_frailty_home <- function(data = scm_frailty_home) {
  
  figure <- ggarrange(fplot.scm(data$frail_home, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Contacts at home") +
                        theme(plot.title.position = 'plot', 
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank()), 
                      fplot.scm(data$frail_nohome, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Contacts not at home") +
                        scale_y_discrete(position = "right") +
                        theme(plot.title.position = 'plot', 
                              plot.title = element_text(hjust = 0.5),
                              axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank()),
                      fplot.scm(data$prefrail_home, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + 
                        theme(axis.text.x = element_blank()),
                      fplot.scm(data$prefrail_nohome, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                        scale_y_discrete(position = "right") +
                        theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank()),
                      fplot.scm(data$nonfrail_home, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") +
                        theme(),
                      fplot.scm(data$non_frail_nohome, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + ylab("Non-frail participant") + rremove("xlab") +
                        scale_y_discrete(position = "right") +
                        theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                              axis.text.y = element_blank()),
                      ncol = 2,
                      nrow = 3)
  
  figure_output <- annotate_figure(figure, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                   bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  
  return(figure_output)
}

f_plot_gamlss_homes <- function(data = list.model.short) {
  
  Covariates_plot <- c(
    "Age group (10-19)", "Age group (20-29)", "Age group (30-39)", "Age group (40-49)", "Age group (50-59)",
    "Age group (60-69)", "Age group (70-79)", "Age group (80-89)", "Age group (90-100)", "Age group (NA)",
    "Pre-frail", "Non-frail", "Missing",
    "No-holiday", "Holiday (NA)",
    "HH (3)", "HH (4)", "HH (5+)",
    "CAWI", "APP", 
    "Part time", "Student job", "Volunteer", "Unemployed", "Not applicable", "Occupation (NA)",
    "Primary education", "Secondary education", "Secondary education (cert.)", "Post graduate", 
    "No diploma", "Education (NA)",
    "Age group (10-19): HH (3)", "Age group (20-29): HH (3)", "Age group (30-39): HH (3)", "Age group (40-49): HH (3)", "Age group (50-59): HH (3)",
    "Age group (60-69): HH (3)", "Age group (70-79): HH (3)", "Age group (80-89): HH (3)", "Age group (90-100): HH (3)", "Age group (NA): HH (3)",
    "Age group (10-19): HH (4)", "Age group (20-29): HH (4)", "Age group (30-39): HH (4)", "Age group (40-49): HH (4)", "Age group (50-59): HH (4)",
    "Age group (60-69): HH (4)", "Age group (70-79): HH (4)", "Age group (80-89): HH (4)", "Age group (90-100): HH (4)", "Age group (NA): HH (4)",
    "Age group (10-19): HH (5+)", "Age group (20-29): HH (5+)", "Age group (30-39): HH (5+)", "Age group (40-49): HH (5+)", "Age group (50-59): HH (5+)",
    "Age group (60-69): HH (5+)", "Age group (70-79): HH (5+)", "Age group (80-89): HH (5+)", "Age group (90-100): HH (5+)", "Age group (NA): HH (5+)",
    
    "Pre-frail: CAWI", "Non-frail: CAWI",
    "Pre-frail: APP", "Non-frail: APP"
  )
  
  data_output <- NULL
  for(i in 1:length(data)) {
    data_input <- cbind(Covariates_plot, data[[i]])
    data_input <- data_input[,-ncol(data_input)]
    
    new_data <- rbind(
      data.frame(
        Covariates_plot = "Age group (0-9)",
        xmin = 1, est = 1, xmax = 1),
      data_input[1:nrow(data_input),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:11),],
      data.frame(
        Covariates_plot = "Frail",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:11),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:15),],
      data.frame(
        Covariates_plot = "Holiday",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:15),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:18),],
      data.frame(
        Covariates_plot = "HH (2)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:18),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:22),],
      data.frame(
        Covariates_plot = "Paper",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:22),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:25),],
      data.frame(
        Covariates_plot = "Full time",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:25),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:32),],
      data.frame(
        Covariates_plot = "Undergraduate",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:32),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:39),],
      data.frame(
        Covariates_plot = "Age group (0-9): HH (3)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:39),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:50),],
      data.frame(
        Covariates_plot = "Age group (0-9): HH (4)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:50),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:61),],
      data.frame(
        Covariates_plot = "Age group (0-9): HH (5+)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:61),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:72),],
      data.frame(
        Covariates_plot = "Frail: CAWI",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:72),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:75),],
      data.frame(
        Covariates_plot = "Frail: APP",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:75),])
    rownames(new_data) <- NULL
    
    new_data$order_level <- factor(c(rep(1, 11), rep(2, 4), rep(3, 3), rep(4, 4), rep(5, 3), rep(6, 7),
                                     rep(7, 7), rep(8, 11), rep(9, 11), rep(10, 11), rep(11, 3),
                                     rep(12, 3)),
                                   levels = seq(1, 12))
    
    new_data$order_in <- c(seq(11, 1), seq(4, 1), seq(3, 1), seq(4, 1), seq(3, 1), seq(7, 1),
                           seq(7, 1), seq(11, 1), seq(11, 1), seq(11, 1), seq(3, 1),
                           seq(3, 1))
    
    if(i == 1) {
      new_data$type <- "Contacts not at home"
    } else {
      new_data$type <- "Contacts at home"
    }
    
    if(is.null(data_output)) {
      data_output <- new_data 
    } else {
      data_output <- rbind(data_output, new_data)
    }
    
  }
  
  data_output <- data_output %>%
    arrange(order_level, order_in) %>%
    mutate(Covariates_plot = factor(Covariates_plot, levels = unique(Covariates_plot)))
  
  # Create the plot
  output <- ggplot(data_output, aes(x = est, y = Covariates_plot, colour = type)) +
    geom_point(size = 3) +  # Plot points for the estimates
    geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2) +  # Add horizontal error bars
    geom_vline(xintercept = 1, color = "red", lwd = 1) +
    facet_wrap(~ order_level, ncol = 2, scales = "free_y") +  # Facet by order_level, 2 columns
    theme_minimal() +
    scale_colour_manual(values = c("#38b000", "#03045e")) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),  # Remove y-axis title since Covariates_plot are labeled
      axis.text.y = element_text(size = 14),  # Adjust y-axis text size
      text = element_text(size = 18),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.title=element_blank()
    ) +
    labs(x = "Relative number of contacts")
  
  return(output)
  
}

f_plot_gamlss_alone <- function(data = list.model.alone.short) {
  
  Covariates_plot <- c(
    "Age group (30-39)", "Age group (40-49)", "Age group (50-59)",
    "Age group (60-69)", "Age group (70-79)", "Age group (80-89)", "Age group (90-100)", "Age group (NA)",
    "Pre-frail", "Non-frail", "Missing",
    "CAWI", "APP", 
    "No-holiday", "Holiday (NA)",
    "Primary education", "Secondary education", "Secondary education (cert.)", "Post graduate", 
    "No diploma", "Education (NA)",
    "Part time", "Student job", "Volunteer", "Unemployed", "Not applicable", "Occupation (NA)"
  )
  
  data_output <- NULL
  for(i in 1:length(data)) {
    data_input <- cbind(Covariates_plot, data[[i]])
    data_input <- data_input[,-ncol(data_input)]
    
    new_data <- rbind(
      data.frame(
        Covariates_plot = "Age group (< 30)",
        xmin = 1, est = 1, xmax = 1),
      data_input[1:nrow(data_input),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:9),],
      data.frame(
        Covariates_plot = "Frail",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:9),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:13),],
      data.frame(
        Covariates_plot = "Paper",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:13),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:16),],
      data.frame(
        Covariates_plot = "Holiday",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:16),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:19),],
      data.frame(
        Covariates_plot = "Undergraduate",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:19),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:26),],
      data.frame(
        Covariates_plot = "Full time",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:26),])
    rownames(new_data) <- NULL
    
    new_data$order_level <- factor(c(rep(1, 9), rep(2, 4), rep(3, 3), rep(4, 3), rep(5, 7), rep(6, 7)),
                                   levels = seq(1, 6))
    
    new_data$order_in <- c(seq(9, 1), seq(4, 1), seq(3, 1), seq(3, 1), seq(7, 1), seq(7, 1))
    
    if(i == 1) {
      new_data$type <- "Contacts not at home"
    } else {
      new_data$type <- "Contacts at home"
    }
    
    if(is.null(data_output)) {
      data_output <- new_data 
    } else {
      data_output <- rbind(data_output, new_data)
    }
    
  }
  
  data_output <- data_output %>%
    arrange(order_level, order_in) %>%
    mutate(Covariates_plot = factor(Covariates_plot, levels = unique(Covariates_plot)))
  
  # Create the plot
  output <- ggplot(data_output, aes(x = est, y = Covariates_plot, colour = type)) +
    geom_point(size = 3) +  # Plot points for the estimates
    geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2) +  # Add horizontal error bars
    geom_vline(xintercept = 1, color = "red", lwd = 1) +
    facet_wrap(~ order_level, ncol = 2, scales = "free_y") +  # Facet by order_level, 2 columns
    theme_minimal() +
    scale_colour_manual(values = c("#38b000", "#03045e")) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),  # Remove y-axis title since Covariates_plot are labeled
      axis.text.y = element_text(size = 14),  # Adjust y-axis text size
      text = element_text(size = 18),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.title=element_blank()
    ) +
    labs(x = "Relative number of contacts")
  
  return(output)
}

f_plot_gamlss_rob <- function(data = list.model.rob.short) {
  
  Covariates_plot <- c(
    "Age group (70+)", "Age group (NA)",
    "No-holiday", "Holiday (NA)",
    "CAWI",
    "HCF (30 - 50ppl)", "HCF (50 - 100ppl)", "HCF (100ppl+)", "HCF (NA)",
    "Pre-frail", "Non-frail",
    "Age group (70+): Pre-frail"
  )
  
  data_output <- NULL
  for(i in 1:length(data)) {
    data_input <- cbind(Covariates_plot, data[[i]])
    data_input <- data_input[,-ncol(data_input)]
    
    new_data <- rbind(
      data.frame(
        Covariates_plot = "Age group (< 70)",
        xmin = 1, est = 1, xmax = 1),
      data_input[1:nrow(data_input),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:3),],
      data.frame(
        Covariates_plot = "Holiday",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:3),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:6),],
      data.frame(
        Covariates_plot = "Paper",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:6),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:8),],
      data.frame(
        Covariates_plot = "HCF (10 - 30ppl)",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:8),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:13),],
      data.frame(
        Covariates_plot = "Frail",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:13),])
    rownames(new_data) <- NULL
    
    new_data <- rbind(
      new_data[c(1:16),],
      data.frame(
        Covariates_plot = "Age group (70+): Frail",
        xmin = 1, est = 1, xmax = 1),
      new_data[-c(1:16),])
    rownames(new_data) <- NULL
    
    new_data$order_level <- factor(c(rep(1, 3), rep(2, 3), rep(3, 2), 
                                     rep(4, 5), rep(5, 3), rep(6, 2)),
                                   levels = seq(1, 6))
    
    new_data$order_in <- c(seq(3, 1), seq(3, 1), seq(2, 1), seq(5, 1), seq(3, 1), seq(2, 1))
    
    if(i == 1) {
      new_data$type <- "Contacts not at home"
    } else {
      new_data$type <- "Contacts at home"
    }
    
    if(is.null(data_output)) {
      data_output <- new_data 
    } else {
      data_output <- rbind(data_output, new_data)
    }
    
  }
  
  data_output <- data_output %>%
    arrange(order_level, order_in) %>%
    mutate(Covariates_plot = factor(Covariates_plot, levels = unique(Covariates_plot)))
  
  # Create the plot
  output <- ggplot(data_output, aes(x = est, y = Covariates_plot, colour = type)) +
    geom_point(size = 3) +  # Plot points for the estimates
    geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2) +  # Add horizontal error bars
    geom_vline(xintercept = 1, color = "red", lwd = 1) +
    facet_wrap(~ order_level, ncol = 2, scales = "free_y") +  # Facet by order_level, 2 columns
    theme_minimal() +
    scale_colour_manual(values = c("#38b000", "#03045e")) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),  # Remove y-axis title since Covariates_plot are labeled
      axis.text.y = element_text(size = 14),  # Adjust y-axis text size
      text = element_text(size = 18),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.title=element_blank()
    ) +
    labs(x = "Relative number of contacts")
  
  return(output)
}

f_plot_nonhh_frailty_home <- function(data = scm_nonhh_frailty_home) {
  
  figure2 <- ggarrange(fplot.scm(data$frail_home, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Contacts at home") +
                         theme(plot.title.position = 'plot', 
                               plot.title = element_text(hjust = 0.5),
                               axis.text.x = element_blank()), 
                       fplot.scm(data$frail_nohome, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Contacts not at home") +
                         scale_y_discrete(position = "right") +
                         theme(plot.title.position = 'plot', 
                               plot.title = element_text(hjust = 0.5),
                               axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank()),
                       fplot.scm(data$prefrail_home, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + 
                         theme(axis.text.x = element_blank()),
                       fplot.scm(data$prefrail_nohome, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                         scale_y_discrete(position = "right") +
                         theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank()),
                       fplot.scm(data$nonfrail_home, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") +
                         theme(),
                       fplot.scm(data$non_frail_nohome, size = 24,
                                 color_scm_min =  color_scm_min,
                                 color_scm_max = color_scm_max,
                                 legendBrks = legendBrks,
                                 legendLabels = legendLabels) + ylab("Non-frail participant") + rremove("xlab") +
                         scale_y_discrete(position = "right") +
                         theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                               axis.text.y = element_blank()),
                       ncol = 2,
                       nrow = 3)
  
  figure_output2 <- annotate_figure(figure2, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                    bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  
  return(figure_output2)
}

f_plot_nonhh_frailty_rob <- function(data = scm_nonhh_frailty_rob) {
  
  figure_rob <- ggarrange(fplot.scm(data$frail_hhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Household member contacts") +
                            theme(plot.title.position = 'plot', 
                                  plot.title = element_text(hjust = 0.5),
                                  axis.text.x = element_blank()), 
                          fplot.scm(data$frail_nohhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Non-household member contacts") +
                            scale_y_discrete(position = "right") +
                            theme(plot.title.position = 'plot', 
                                  plot.title = element_text(hjust = 0.5),
                                  axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_blank()),
                          fplot.scm(data$prefrail_hhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + rremove("ylab") + rremove("xlab"),
                          fplot.scm(data$prefrail_nonhhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                            scale_y_discrete(position = "right") +
                            theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                  axis.text.y = element_blank()),
                          ncol = 2,
                          nrow = 2)
  
  figure_rob_output <- annotate_figure(figure_rob, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                       bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  return(figure_rob_output)
}

f_plot_frailty_onehh <- function(data = scm_nonhh_frailty_onehh){
  
  figure_one <- ggarrange(fplot.scm(data$frail_hhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Contacts at home") +
                            theme(plot.title.position = 'plot', 
                                  plot.title = element_text(hjust = 0.5),
                                  axis.text.x = element_blank()), 
                          fplot.scm(data$frail_nohhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Contacts not at home") +
                            scale_y_discrete(position = "right") +
                            theme(plot.title.position = 'plot', 
                                  plot.title = element_text(hjust = 0.5),
                                  axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_blank()),
                          fplot.scm(data$prefrail_hhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + 
                            theme(axis.text.x = element_blank()),
                          fplot.scm(data$prefrail_nonhhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                            scale_y_discrete(position = "right") +
                            theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_blank()),
                          fplot.scm(data$nonfrail_hhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") +
                            theme(),
                          fplot.scm(data$non_frail_nonhhmember, size = 24,
                                    color_scm_min =  color_scm_min,
                                    color_scm_max = color_scm_max,
                                    legendBrks = legendBrks,
                                    legendLabels = legendLabels) + ylab("Non-frail participant") + rremove("xlab") +
                            scale_y_discrete(position = "right") +
                            theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                  axis.text.y = element_blank()),
                          ncol = 2,
                          nrow = 3)
  
  figure_output_one <- annotate_figure(figure_one, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                       bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  return(figure_output_one)
}

f_plot_frailty_othershh <- function(data = scm_nonhh_frailty_othershh) {
  
  figure_others <- ggarrange(fplot.scm(data$frail_hhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Contacts at home") +
                               theme(plot.title.position = 'plot', 
                                     plot.title = element_text(hjust = 0.5),
                                     axis.text.x = element_blank()), 
                             fplot.scm(data$frail_nohhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Contacts not at home") +
                               scale_y_discrete(position = "right") +
                               theme(plot.title.position = 'plot', 
                                     plot.title = element_text(hjust = 0.5),
                                     axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                     axis.text.x = element_blank(),
                                     axis.text.y = element_blank()),
                             fplot.scm(data$prefrail_hhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + 
                               theme(axis.text.x = element_blank()),
                             fplot.scm(data$prefrail_nonhhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                               scale_y_discrete(position = "right") +
                               theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                     axis.text.x = element_blank(),
                                     axis.text.y = element_blank()),
                             fplot.scm(data$nonfrail_hhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") +
                               theme(),
                             fplot.scm(data$non_frail_nonhhmember, size = 24,
                                       color_scm_min =  color_scm_min,
                                       color_scm_max = color_scm_max,
                                       legendBrks = legendBrks,
                                       legendLabels = legendLabels) + ylab("Non-frail participant") + rremove("xlab") +
                               scale_y_discrete(position = "right") +
                               theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                                     axis.text.y = element_blank()),
                             ncol = 2,
                             nrow = 3)
  
  figure_output_others <- annotate_figure(figure_others, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                          bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  
  return(figure_output_others)
}

f_plot_frailty_othershh <- function(data = scm_frailty_rob) {
  
  figure <- ggarrange(fplot.scm(data$frail_hhmember, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + rremove("ylab") + rremove("xlab") + ggtitle("Contacts at home") +
                        theme(plot.title.position = 'plot', 
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank()), 
                      fplot.scm(data$frail_nohhmember, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + ylab("Frail participant") + rremove("xlab") + ggtitle("Contacts not at home") +
                        scale_y_discrete(position = "right") +
                        theme(plot.title.position = 'plot', 
                              plot.title = element_text(hjust = 0.5),
                              axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank()),
                      fplot.scm(data$prefrail_hhmember, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + rremove("ylab") + rremove("xlab"),
                      fplot.scm(data$prefrail_nonhhmember, size = 24,
                                color_scm_min =  color_scm_min,
                                color_scm_max = color_scm_max,
                                legendBrks = legendBrks,
                                legendLabels = legendLabels) + ylab("Pre-frail participant")  + rremove("xlab") +
                        scale_y_discrete(position = "right") +
                        theme(axis.title.y.right = element_text(angle=270, vjust = 1, size = 26),
                              axis.text.y = element_blank()),
                      ncol = 2,
                      nrow = 2)
  
  figure_output <- annotate_figure(figure, left = textGrob("Contact age group", rot = 90, gp = gpar(cex = 1.85)),
                                   bottom = textGrob("Participant age group", gp = gpar(cex = 1.85), vjust = 0.25))
  
  return(figure_output)
}

decompose_matrix_output <- function(data = temp$output_decomposition) {
  
  data_frail_frail = data$frail_output$frail_frail_matrix
  data_frail_prefrail = data$frail_output$frail_prefrail_matrix
  data_frail_nonfrail = data$frail_output$frail_nonfrail_matrix
  
  data_prefrail_frail = data$prefrail_output$pre_frail_frail_matrix
  data_prefrail_prefrail = data$prefrail_output$pre_frail_prefrail_matrix
  data_prefrail_nonfrail = data$prefrail_output$pre_frail_nonfrail_matrix
  
  data_nonfrail_frail = data$nonfrail_output$non_frail_frail_matrix
  data_nonfrail_prefrail = data$nonfrail_output$non_frail_prefrail_matrix
  data_nonfrail_nonfrail = data$nonfrail_output$non_frail_nonfrail_matrix
  
  colnames(data_frail_frail) <- colnames(data_frail_prefrail) <- colnames(data_frail_nonfrail) <- age_range
  rownames(data_frail_frail) <- rownames(data_frail_prefrail) <- rownames(data_frail_nonfrail) <- age_range
  
  colnames(data_prefrail_frail) <- colnames(data_prefrail_prefrail) <- colnames(data_prefrail_nonfrail) <- age_range
  rownames(data_prefrail_frail) <- rownames(data_prefrail_prefrail) <- rownames(data_prefrail_nonfrail) <- age_range
  
  colnames(data_nonfrail_frail) <- colnames(data_nonfrail_prefrail) <- colnames(data_nonfrail_nonfrail) <- age_range
  rownames(data_nonfrail_frail) <- rownames(data_nonfrail_prefrail) <- rownames(data_nonfrail_nonfrail) <- age_range
  
  data_df <- rbind(
    as.data.frame(data_frail_frail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Frail",
                    cnt_id = "Frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_frail_prefrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Frail",
                    cnt_id = "Pre-frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_frail_nonfrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Frail",
                    cnt_id = "Non-frail") %>% 
      dplyr::arrange(part_age, cnt_age), #--- end here for part frail
    as.data.frame(data_prefrail_frail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Pre-frail",
                    cnt_id = "Frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_prefrail_prefrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Pre-frail",
                    cnt_id = "Pre-frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_prefrail_nonfrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Pre-frail",
                    cnt_id = "Non-frail") %>% 
      dplyr::arrange(part_age, cnt_age),# --- ends here for pre-frail
    as.data.frame(data_nonfrail_frail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Non-frail",
                    cnt_id = "Frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_nonfrail_prefrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Non-frail",
                    cnt_id = "Pre-frail") %>% 
      dplyr::arrange(part_age, cnt_age),
    as.data.frame(data_nonfrail_nonfrail) %>% 
      dplyr::mutate(part_age = rownames(.)) %>%
      gather(cnt_age, cnt_rates, -part_age) %>%
      dplyr::mutate(part_id = "Non-frail",
                    cnt_id = "Non-frail") %>% 
      dplyr::arrange(part_age, cnt_age)
  )
  
  data_df$part_id <- as.factor(data_df$part_id)
  data_df$cnt_id <- as.factor(data_df$cnt_id)
  data_df$text_color<-ifelse(data_df$cnt_rates>1,"up" ,"low")
  
  data_df$part_id <- factor(data_df$part_id, levels = c("Frail", "Pre-frail", "Non-frail"))
  data_df$cnt_id <- factor(data_df$cnt_id, levels = c("Frail", "Pre-frail", "Non-frail"))
  
  output <- ggplot(data_df,
                   aes(x = part_age, y = cnt_age,
                       fill = cnt_rates)) +
    geom_tile() +
    facet_grid(cnt_id ~ part_id) +
    labs(x = "Participant age group",
         y = "Contact age group") +
    scale_fill_viridis_c(limits = c(color_scm_min,
                                    color_scm_max+0.5),
                         oob = scales::squish,
                         breaks = legendBrks,
                         name ="Average number \n of contacts",
                         labels=legendLabels) +
    geom_text(aes(label=paste(formatC(round(cnt_rates, 2),2,format="f")),
                  colour=text_color),show.legend = FALSE, size = 5, fontface = "bold") + 
    scale_colour_manual(values=c("white","black")) +
    theme(legend.position = "top") +
    theme(text = element_text(size = 18))
  
  return(output)
}