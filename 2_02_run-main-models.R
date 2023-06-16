# Run Main Models
# F31 Google Traffic COVID ITS Analysis
# Jenni A. Shearston 
# Updated 06/16/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Split, Filter & Nest Dataset
# 2: Create Function for Running & Saving Models
# 3: Run Models
# 4: Model Evaluation
# 5: Sensitivity Analysis Removing Traffic Vars from EJI
# 6: Sensitivity Analyses Changing Tensor Term Knots
# 7: Sensitivity Analysis Adjusting Each EJI Module by the Others

####**************
#### N: Notes ####
####**************

# STILL TO FOLLOW UP ON AS OF JUNE 16:
# Follow up with Markus about weird traffic decrease in June-August 2018

# Na Description
# In this script we specify and run all models, including the main analysis,
# secondary analyses, and sensitivity analyses. We also conduct some model
# diagnostics and evaluation.

# Nb Resources
# Resource for adding autocorrelation structure to mixed model with nlme::lme
# http://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
# short summary from stack exchange, which linked to B Bolker's github:
# https://stackoverflow.com/questions/49796175/how-to-check-and-control-for-autocorrelation-in-a-mixed-effect-model-of-longitud

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
data_path <- paste0(project.folder, 'data/processed_data/')
model_path <- paste0(project.folder, 'outputs/models/')

# 0d Load data
fullData <- read_rds(paste0(data_path, 'full_dataset_wcovars_daily.rds'))
fullData_rh <- read_rds(paste0(data_path, 'full_dataset_wcovars+rushhour_daily.rds'))


####*************************************
#### 1: Split, Filter & Nest Dataset #### 
####*************************************

# 1a Nest by strata
# 1a.i ICE HH Income and BW Race, 5 quintiles
fullDataS <- fullData %>% group_by(ice_hhincome_bw_5) %>% nest() %>% 
  rename(strata = ice_hhincome_bw_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'iceHhincomeBwQ1', strata == 'Q2' ~ 'iceHhincomeBwQ2',
    strata == 'Q3' ~ 'iceHhincomeBwQ3', strata == 'Q4' ~ 'iceHhincomeBwQ4',
    strata == 'Q5' ~ 'iceHhincomeBwQ5'))
fullDataS_rh <- fullData_rh %>% group_by(ice_hhincome_bw_5, rush_hour) %>% nest() %>% 
  mutate(strata = paste0('iceHhincomeBw', ice_hhincome_bw_5, 'rh', rush_hour)) %>% 
  ungroup() %>% dplyr::select(strata, data)
# 1a.ii EJI, 5 quintiles
fullDataS.2 <- fullData %>% group_by(eji_5) %>% nest() %>% 
  rename(strata = eji_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiQ1', strata == 'Q2' ~ 'ejiQ2',
    strata == 'Q3' ~ 'ejiQ3', strata == 'Q4' ~ 'ejiQ4',
    strata == 'Q5' ~ 'ejiQ5'))
fullDataS.2_rh <- fullData_rh %>% group_by(eji_5, rush_hour) %>% nest() %>% 
  mutate(strata = paste0('eji', eji_5, 'rh', rush_hour)) %>% 
  ungroup() %>% dplyr::select(strata, data)
# 1a.iii EJI EBM Module, 3 tertiles
fullDataS.3 <- fullData %>% group_by(eji_ebm_3) %>% nest() %>% 
  rename(strata = eji_ebm_3) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiEbmQ1', strata == 'Q2' ~ 'ejiEbmQ2', 
    strata == 'Q3' ~ 'ejiEbmQ3'))
# 1a.iv EJI SVM Module, 3 tertiles
fullDataS.4 <- fullData %>% group_by(eji_svm_3) %>% nest() %>% 
  rename(strata = eji_svm_3) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiSvmQ1', strata == 'Q2' ~ 'ejiSvmQ2', 
    strata == 'Q3' ~ 'ejiSvmQ3'))
# 1a.v EJI HVM Module, 3 tertiles
fullDataS.5 <- fullData %>% group_by(eji_hvm_3) %>% nest() %>% 
  rename(strata = eji_hvm_3) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiHvmQ1', strata == 'Q2' ~ 'ejiHvmQ2', 
    strata == 'Q3' ~ 'ejiHvmQ3'))

# 1b Bind together
fullDataS <- fullDataS %>% 
  bind_rows(fullDataS.2, fullDataS.3, fullDataS.4, fullDataS.5, fullDataS_rh, fullDataS.2_rh) %>% 
  filter(!is.na(strata)) %>% filter(!str_detect(strata, 'NA'))

# 1c Create filtered dataset to exclude Phase 1 reopening and beyond for models
#    that follow ITS impact model A
#    Note: NYC entered Phase 1 reopening on June 8, 2020
fullDataFS <- fullDataS %>% 
  mutate(data = map(data, ~ filter(., date < '2020-06-08')))

# 1d Clean environment
rm(fullDataS.2, fullDataS.3, fullDataS.4, fullDataS.5, fullData_rh,
   fullDataS_rh, fullDataS.2_rh)

####****************************************************
#### 2: Create Function for Running & Saving Models #### 
####****************************************************

# 2a Initialize function
analyze_trafPause <- function(strata, dataForMod, outcome, analysis, outputPath){
                              # strata <- 'fullDataS$strata[[i]]'; 
                              # dataForMod <- fullDataS$data[[i]];
                              # outcome <- 'propGreen';
                              # analysis <- 'main';
                              # outputPath <- model_path
  
  # 2b Create model identifier
  modelIdentifier <- paste0(strata, '_', outcome, '_', analysis)
  
  # 2c Create intervention model(s)
  #    Notes: Time terms are used to account for temporal autocorrelation
  #           Tensor term is used to account for spatial autocorrelation
  #             We estimate 8 knots for lat b/c study area is ~ 8 mi
  #             We estimate 4 knots for lon b/c study area is ~ 4 mi
  
      # 2c.i Outcome == propGreen & Analysis == main
      if (str_detect(outcome, 'propGreen') & str_detect(analysis, 'main')){
        
        mod <- gamm4::gamm4(prop_green ~ pause + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.ii Outcome == propMaroonRed & Analysis == main
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'main')){
    
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.iii Outcome == propMaroonRed & Analysis == includeRecovery
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'includeRecovery')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.iv Sensitivity analysis with 4 and 2 knots in the tensor term (one knot per 2 miles)
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'sensKnots4.2')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(4, 2), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.v Sensitivity analysis with 11 and 5 knots in the tensor term
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'sensKnots11.5')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(11, 5), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.vi Sensitivity analysis with 16 and 8 knots in the tensor term (one knot per 0.5 miles)
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'sensKnots16.8')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + t2(lat, lon, k = c(16, 8), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.vii Sensitivity analysis with EJI Modules adjusted for each other: EBM module  
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'ejiModsAdjustedEBM')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + rpl_svm + rpl_hvm
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.viii Sensitivity analysis with EJI Modules adjusted for each other: SVM module 
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'ejiModsAdjustedSVM')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + rpl_ebm + rpl_hvm
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
      # 2c.ix Sensitivity analysis with EJI Modules adjusted for each other: HVM module 
      if (str_detect(outcome, 'propMaroonRed') & str_detect(analysis, 'ejiModsAdjustedHVM')){
        
        mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end
                            + time_elapsed 
                            + as.factor(year) + as.factor(month) + as.factor(dow) 
                            + rpl_ebm + rpl_svm
                            + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                            random = ~(1|poly_id), family = gaussian(), 
                            data = dataForMod)}
  
  # 2d Save model
  mod %>% saveRDS(paste0(outputPath, modelIdentifier, '.rds'))
  
  # 2e Extract desired model coefficients and n
  tidy_mod <- tidy(mod$gam, parametric = TRUE, conf.int = TRUE)
  model_n <- dataForMod %>% na.omit() %>% summarise(n = as.integer(n()))
  
  # 2f Read in model results table
  modTable <- read_csv(paste0(outputPath, 'model_results_table.csv'), 
                       col_types = c('c', 'i', 'n', 'n', 'n', 'n', 'n', 'n', 'T'))
  
  # 2g Add this iteration's model results to the set of model results
  
      # 2g.i Analysis == main
      if (str_detect(analysis, 'main')){
        
        modTable[1 + nrow(modTable),] <- list(modelIdentifier, model_n$n,
                                        tidy_mod$estimate[2], tidy_mod$conf.low[2], 
                                        tidy_mod$conf.high[2], NA, NA, NA,
                                        Sys.time())}
      
      # 2g.ii Analysis == includeRecovery or ajiModsAdjusted or sens
      if (str_detect(analysis, 'includeRecovery') | str_detect(analysis, 'ejiModsAdjusted')
          | str_detect(analysis, 'sens')){
        
        modTable[1 + nrow(modTable),] <- list(modelIdentifier, model_n$n,
                                              tidy_mod$estimate[2], tidy_mod$conf.low[2], 
                                              tidy_mod$conf.high[2],
                                              tidy_mod$estimate[3], tidy_mod$conf.low[3],
                                              tidy_mod$conf.high[3],
                                              Sys.time())}
  
  # 2h Remove old models and save
  #    Note: The slice step keeps only the earliest model results for each 
  #          strata-outcome-analysis combo
  modTable %>% 
    group_by(model_identifier) %>% 
    arrange(desc(run_date)) %>% 
    slice(0:1) %>% 
    filter(!is.na(model_identifier)) %>% 
    write_csv(paste0(outputPath, 'model_results_table.csv'))
  
}

####*******************
#### 3: Run Models #### 
####*******************

# 3a Set up parallelization (if using)
# 3a.i Get number of cores
#      Note: We subtract 2 cores to reserve for other tasks
n.cores <- parallel::detectCores() - 2
# 3a.ii Create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = 'FORK')
# 3a.iii Register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# 3b Run models for prop_maroon_red outcome & include Pause recovery
#    Note: Run in parallel, 19 strata take ~ 4.3 hrs
#          When running in parallel the 'slice' step in 2h where old models are 
#          removed from the modTable does not work correctly
#          b/c each core does the step independently (although the table is saved)

# 3b.i Parallel option 
tictoc::tic('Run 20 strata in parallel')
mod_results <- 
  foreach(
    i = 1:length(fullDataS$strata),
    .combine = 'rbind'
  ) %dopar% {
    analyze_trafPause(strata = fullDataS$strata[[i]], dataForMod = fullDataS$data[[i]], 
                      outcome = 'propMaroonRed', analysis = 'includeRecovery', outputPath = model_path)}
stopCluster(my.cluster)
tictoc::toc()
# 3b.i.1 Remove old models and re-save
#        When running in parallel the 'slice' step in 2h where old models are 
#        removed from the modTable does not work correctly
#        b/c each core does the step independently (although the table is saved)
mod_results <- mod_results %>% 
  group_by(model_identifier) %>% 
  arrange(desc(run_date)) %>% 
  slice(0:1) %>% 
  filter(!is.na(model_identifier)) %>% 
  write_csv(paste0(model_path, 'model_results_table.csv'))

# 3b.ii For loop option 
#    Note: One model takes ~ 15 min, two models take ~ 32 min
tictoc::tic('Run X strata in for loop')
for(i in 1:length(fullDataS$strata)){
  analyze_trafPause(strata = fullDataS$strata[[i]], dataForMod = fullDataS$data[[i]], 
                    outcome = 'propMaroonRed', analysis = 'includeRecovery', outputPath = model_path)
  print(fullDataS$strata[[i]])
}
tictoc::toc()


# Solo model for experimenting
# ~15 minutes
tictoc::tic('one model')
mod <- gamm4::gamm4(prop_maroon_red ~ pause + pause_end + time_elapsed 
                    + as.factor(year) + as.factor(month) + as.factor(dow) 
                    + t2(lat, lon, k = c(8, 4), bs = 'cr'),
                    random = ~(1|poly_id), family = gaussian(), 
                    data = fullDataS$data[[1]])
tictoc::toc()

####*************************
#### 4: Model Evaluation #### 
####*************************

# Notes: Great QQ Plots resource: https://towardsdatascience.com/q-q-plots-explained-5aa8495426c0

# 4a Initiate function to evaluate model
model_eval <- function(dataForMod, model, outputPath){
                       # dataForMod <- fullDataS$data[[i]];
                       # model <- 'iceHhincomeBwQ2_propGreen_main.rds'
                       # outputPath <- model_path

  # 4b Load model(s) to evaluate
  mod <- readr::read_rds(paste0(outputPath, model))

  # 4c Create dataframe for model diagnostics
  mod_diag <- dataForMod %>% 
    mutate(fitted = mod$gam$fitted.values,
           resids = mod$gam$residuals,
           resids_scaled = scale(resids))
  
  # 4d Check heteroscedasticity using resids vs fitted values plot
  hetero_plot <- mod_diag %>% ggplot(aes(x = fitted, y = resids_scaled)) +
    geom_point() + geom_hline(yintercept = 0) + xlab("Fitted values") + 
    ylab("Standardized residuals") + theme_bw()
  
  # 4e Check for non-linearity using resids vs explanatory vars plots
  # 4e.i Year
  nonLinearity_plot_year <- mod_diag %>% ggplot(aes(x = year, y = resids_scaled)) +
    geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Explanatory Var") + 
    ylab("Standardized residuals") + theme_bw()
  # 4e.i Month
  nonLinearity_plot_month <- mod_diag %>% ggplot(aes(x = month, y = resids_scaled)) +
    geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Explanatory Var") + 
    ylab("Standardized residuals") + theme_bw()
  # 4e.i Day of week
  nonLinearity_plot_dow <- mod_diag %>% ggplot(aes(x = dow, y = resids_scaled)) +
    geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Explanatory Var") + 
    ylab("Standardized residuals") + theme_bw()
  
  # 4f Check that residuals are normally distributed (QQ Plot)
  qqnorm(mod_diag$resids, pch = 20, col = "black")
  qqline(mod_diag$resids)
  norm_plot <- recordPlot()
  
  # 4g Check for influential datapoints
  infl_plot <- mod_diag %>% 
    ggplot(aes(y = resids_scaled)) +
    geom_boxplot() + ylab('Standardized Resids')
  mean_traf <- mean(mod_diag$prop_maroon_red)
  sd_traf <- sd(mod_diag$prop_maroon_red)
  infl_df <- mod_diag %>% filter(resids > (mean_traf+(sd_traf*3)))
  
  # 4h Check for autocorrelation
  acf_plot <- acf(mod_diag$resids)
  pacf_plot <- pacf(mod_diag$resids)
  
  # 4i Check concurvity
  concurv_df <- concurvity(mod$gam)
  
  # 4j Return needed plots
  return(list(hetero_plot, nonLinearity_plot_year, nonLinearity_plot_month,
         nonLinearity_plot_dow, norm_plot, infl_plot, infl_df,
         acf_plot, pacf_plot, concurv_df))
}

# 4k Evaluate one strata from ICE 
#    Notes: error variances a bit unequal (fanned to right for fits vs st.resids plot),
#             but not outrageous, just ok
#           not quite normal; distribution peaked in middle (from QQ plot)
#             considered logging outcome, but the decrease in interpretibility not
#             worth it
#           acf of about .5 and lower, pacf ok; good enough
#           9 outliers greater than 3 SD from mean, no clear pattern although most in Nov,
#             and within expected range. doubt these are influential 
#           concurvity present (non-parametric version of multicollinearity), prob
#             from time vars correlating with our intervention (e.g., pause only
#             occurred in 2020 in a couple months), this can inflate std. errors 
#             but our std. errors are super small so not concerning here
dataForMod = fullDataS$data[[1]] 
model = 'iceHhincomeBwQ2_propMaroonRed_includeRecovery.rds'
outputPath = model_path
eval1 <- model_eval(dataForMod, model, outputPath)
mod_ice <- readr::read_rds(paste0(outputPath, model))
gam.check(mod_ice$gam)
mod_ice$mer

# 4l Evaluate one strata from EJI
#    Notes: error variances a bit unequal (fanned to right for fits vs st.resids plot),
#             but not outrageous, just ok
#           not quite normal; distribution peaked in middle (from QQ plot)
#             considered logging outcome, but the decrease in interpretibility not
#             worth it
#           1 outlier greater than 3 SD from mean, within expected range, prob.
#             not influential
#           pacf ok, acf of .64 at lag 1, .49 at lag 2, and then lower --> ok with this
#           concurvity present (non-parametric version of multicollinearity), prob
#             from time vars correlating with our intervention (e.g., pause only
#             occurred in 2020 in a couple months), this can inflate std. errors 
#             but our std. errors are super small so not concerning here
dataForMod = fullDataS$data[[6]]
model = 'ejiQ1_propMaroonRed_includeRecovery.rds' 
outputPath = model_path
eval2 <- model_eval(dataForMod, model, outputPath)
mod_eji <- readr::read_rds(paste0(outputPath, model))
gam.check(mod_eji$gam)
mod_eji$mer

####************************************************************
#### 5: Sensitivity Analysis Removing Traffic Vars from EJI #### 
####************************************************************

# 5a Nest by EJI_sens quintiles
fullDataS_EJInoTraf <- fullData %>% group_by(eji_sens_5) %>% nest() %>% 
  rename(strata = eji_sens_5) %>% 
  mutate(strata = case_when(
    strata == 'Q1' ~ 'ejiSensQ1', strata == 'Q2' ~ 'ejiSensQ2',
    strata == 'Q3' ~ 'ejiSensQ3', strata == 'Q4' ~ 'ejiSensQ4',
    strata == 'Q5' ~ 'ejiSensQ5')) %>% filter(!is.na(strata))

# 5b Run models using for loop option 
#    Note: One model takes ~ 15 min
tictoc::tic('Run 5 strata in for loop')
for(i in 1:length(fullDataS_EJInoTraf$strata)){
  analyze_trafPause(strata = fullDataS_EJInoTraf$strata[[i]], dataForMod = fullDataS_EJInoTraf$data[[i]], 
                    outcome = 'propMaroonRed', analysis = 'includeRecovery', outputPath = model_path)
  print(fullDataS_EJInoTraf$strata[[i]])
}
tictoc::toc()

####********************************************************
#### 6: Sensitivity Analyses Changing Tensor Term Knots #### 
####********************************************************

# 6a Filter to ICE and EJI quintiles used for main analysis
fullDataS_forKnots <- fullDataS %>% 
  filter(!str_detect(strata, 'rh0|rh1|Ebm|Svm|Hvm'))

# 6b Set up parallelization (if using)
# 6b.i Get number of cores
#      Note: We subtract 2 cores to reserve for other tasks
n.cores <- parallel::detectCores() - 2
# 6b.ii Create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = 'FORK')
# 6b.iii Register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# 6c Run models with 4 and 2 knots (one knot per 2 miles)
#    Note: Run in parallel, 10 strata take ~ 2.5 hrs
# 6c.i Run models
tictoc::tic('Run 10 strata in parallel with 4 and 2 knots')
mod_results <- 
  foreach(
    i = 1:length(fullDataS_forKnots$strata),
    .combine = 'rbind'
  ) %dopar% {
    analyze_trafPause(strata = fullDataS_forKnots$strata[[3]], dataForMod = fullDataS_forKnots$data[[3]], 
                      outcome = 'propMaroonRed', analysis = 'sensKnots4.2', outputPath = model_path)}
stopCluster(my.cluster)
tictoc::toc()
# 6c.ii Remove old models and re-save
mod_results <- mod_results %>% 
  group_by(model_identifier) %>% 
  arrange(desc(run_date)) %>% 
  slice(0:1) %>% 
  filter(!is.na(model_identifier)) %>% 
  write_csv(paste0(model_path, 'model_results_table.csv'))

# 6d Run models with 11 and 5 knots
# 6d.i Run models
tictoc::tic('Run 10 strata in parallel w 11 and 5 knots')
mod_results <- 
  foreach(
    i = 1:length(fullDataS_forKnots$strata),
    .combine = 'rbind'
  ) %dopar% {
    analyze_trafPause(strata = fullDataS_forKnots$strata[[i]], dataForMod = fullDataS_forKnots$data[[i]], 
                      outcome = 'propMaroonRed', analysis = 'sensKnots11.5', outputPath = model_path)}
stopCluster(my.cluster)
tictoc::toc()
# 6d.ii Remove old models and re-save
mod_results <- mod_results %>% 
  group_by(model_identifier) %>% 
  arrange(desc(run_date)) %>% 
  slice(0:1) %>% 
  filter(!is.na(model_identifier)) %>% 
  write_csv(paste0(model_path, 'model_results_table.csv'))

# 6e Run one strata for 16 and 8 knots (one knot per .5 miles)
#    Note: Only did one strata because of computational time
tictoc::tic('Run 1 strata with 16 and 8 knots')
analyze_trafPause(strata = fullDataS_forKnots$strata[[1]], dataForMod = fullDataS_forKnots$data[[1]], 
                    outcome = 'propMaroonRed', analysis = 'sensKnots16.8', outputPath = model_path)
tictoc::toc()

####*********************************************************************
#### 7: Sensitivity Analysis Adjusting Each EJI Module by the Others #### 
####*********************************************************************

# 7a Filter to EJI modules tertiles used for main analysis
fullDataS_forEBMSens <- fullDataS %>% filter(str_detect(strata, 'Ebm'))
fullDataS_forSVMSens <- fullDataS %>% filter(str_detect(strata, 'Svm'))
fullDataS_forHVMSens <- fullDataS %>% filter(str_detect(strata, 'Hvm'))

# 7b Run EBM module adjusted for SVM and HVM
tictoc::tic('Run 3 strata in for loop')
for(i in 1:length(fullDataS_forEBMSens$strata)){
  analyze_trafPause(strata = fullDataS_forEBMSens$strata[[i]], dataForMod = fullDataS_forEBMSens$data[[i]], 
                    outcome = 'propMaroonRed', analysis = 'ejiModsAdjustedEBM', outputPath = model_path)
  print(fullDataS_forEBMSens$strata[[i]])
}
tictoc::toc()

# 7c Run SVM module adjusted for EBM and HVM
tictoc::tic('Run 3 strata in for loop')
for(i in 1:length(fullDataS_forSVMSens$strata)){
  analyze_trafPause(strata = fullDataS_forSVMSens$strata[[i]], dataForMod = fullDataS_forSVMSens$data[[i]], 
                    outcome = 'propMaroonRed', analysis = 'ejiModsAdjustedSVM', outputPath = model_path)
  print(fullDataS_forSVMSens$strata[[i]])
}
tictoc::toc()

# 7d Run HVM module adjusted for SVM and EBM
tictoc::tic('Run 3 strata in for loop')
for(i in 2:length(fullDataS_forHVMSens$strata)){
  fullDataS_forHVMSens$data[[i]] <- fullDataS_forHVMSens$data[[i]] %>% na.omit()
  analyze_trafPause(strata = fullDataS_forHVMSens$strata[[i]], dataForMod = fullDataS_forHVMSens$data[[i]], 
                    outcome = 'propMaroonRed', analysis = 'ejiModsAdjustedHVM', outputPath = model_path)
  print(fullDataS_forHVMSens$strata[[i]])
}
tictoc::toc()









