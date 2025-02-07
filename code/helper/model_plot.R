library(ggpubr)

if(!file.exists("../rds/plot_model_household.rds")) {
  model_household_home <- readRDS("../rds/model_household_home.rds")
  model_household_nothome <- readRDS("../rds/model_household_nothome.rds")
  model.list <- c("model_household_nothome", "model_household_home")
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
  
  list.model.short <- lapply(list.model, function(x) x[1:(which(rownames(x) == "X.Intercept..1") - 1), c("xmin", "est", "xmax")])
  list.model.short <- lapply(list.model.short, function(x) x[rownames(x) != "X.Intercept.",])
  list.model.short <- lapply(list.model.short, function(x) {
    x$Covariates <- rownames(x)  # Create a new column 'Covariates' with rownames as values
    rownames(x) <- NULL  # Remove the rownames from the data frame
    return(x)
  })
  
  saveRDS(list.model.short, "../rds/plot_model_household.rds")
} else {
  list.model.short <- readRDS("../rds/plot_model_household.rds")
} 

output_homes <- f_plot_gamlss_homes(data = list.model.short)

file_name = paste(dir, "/../output/gamlss_result_homes.png", sep="")
png(file_name, width = 30, height = 35, units = "cm", res = 250)
print(output_homes)
dev.off()

# --- HH 1
if(!file.exists("../rds/plot_model_alone_household.rds")) {
  model_alone_home <- readRDS("../rds/model_alone_home.rds")
  model_alone_nothome <- readRDS("../rds/model_alone_nothome.rds")
  
  model.list <- c("model_alone_nothome", "model_alone_home")
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
  
  list.model.short <- lapply(list.model, function(x) x[1:(which(rownames(x) == "X.Intercept..1") - 1), c("xmin", "est", "xmax")])
  list.model.short <- lapply(list.model.short, function(x) x[rownames(x) != "X.Intercept.",])
  list.model.short <- lapply(list.model.short, function(x) {
    x$Covariates <- rownames(x)  # Create a new column 'Covariates' with rownames as values
    rownames(x) <- NULL  # Remove the rownames from the data frame
    return(x)
  })
  
  saveRDS(list.model.short, "../rds/plot_model_alone_household.rds")
} else {
  list.model.alone.short <- readRDS("../rds/plot_model_alone_household.rds")
}

output_alone <- f_plot_gamlss_alone(data = list.model.alone.short)

file_name = paste(dir, "/../output/gamlss_result_alone.png", sep="")
png(file_name, width = 30, height = 35, units = "cm", res = 250)
print(output_alone)
dev.off()

# --- ROB/RVT
if(!file.exists("../rds/plot_model_alone_household.rds")) {
  model_rob_home <- readRDS("../rds/model_rob_home.rds")
  model_rob_nothome <- readRDS("../rds/model_rob_nothome.rds")
  
  model.list <- c("model_rob_nothome", "model_rob_home")
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
  
  list.model.short <- lapply(list.model, function(x) x[1:(which(rownames(x) == "X.Intercept..1") - 1), c("xmin", "est", "xmax")])
  list.model.short <- lapply(list.model.short, function(x) x[rownames(x) != "X.Intercept.",])
  list.model.short <- lapply(list.model.short, function(x) {
    x$Covariates <- rownames(x)  # Create a new column 'Covariates' with rownames as values
    rownames(x) <- NULL  # Remove the rownames from the data frame
    return(x)
  })
  
  saveRDS(list.model.short, "../rds/plot_model_rob.rds")
} else {
  list.model.rob.short <- readRDS("../rds/plot_model_rob.rds")  
}

output_rob <- f_plot_gamlss_rob(data = list.model.rob.short)

file_name = paste(dir, "/../output/gamlss_result_rob.png", sep="")
png(file_name, width = 20, height = 15, units = "cm", res = 250)
print(output_rob)
dev.off()

file_name = paste(dir, "/../output/gamlss_result_arrange.png", sep="")
png(file_name, width = 40, height = 35, units = "cm", res = 250)
ggarrange(output_alone, output_homes,
          common.legend = T, labels = c("(A)", "(B)"))
dev.off()

