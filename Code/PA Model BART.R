library(dbarts)
library(fastDummies)
library(rsample)
library(tidyverse)

#### BART MODEL ####
weighting1_function_pa <- function(weight_2021_1){
  weight_2022_1 <- 1 - weight_2021_1
  print(paste('Weight 2021 1: ', round(weight_2021_1,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1),
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*0.2  + lineup_pos_avg_2021*0.8, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*0.8 + lineup_pos_avg_2021*0.2,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*0.2 + pa_second_half_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*0.8 + pa_second_half_2021*0.2,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*0.2 + pa_diff_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*0.8 + pa_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*3,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*0.2  + PA_per_game_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*0.8 + PA_per_game_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*0.2  + onbase_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*0.8 + onbase_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*0.2  + total_bases2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*0.8 + total_bases2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*0.2 + hardhits_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*0.8 + hardhits_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*0.2+ barrels_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*0.8 + barrels_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*3,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      cluster_2023 = as.factor(cluster_2023),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*0.2 + pitches_faced_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*0.8 + pitches_faced_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*0.2 + hr_2021*0.8, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*0.8 + hr_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*0.2 + fielding_RV_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*0.8 + fielding_RV_2021*0.2,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*0.2 + bat_RV_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*0.8 + bat_RV_2021*0.2,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*3,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*0.2  + wOBA_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*0.8 + wOBA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*0.2  + hardhit_pct_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*0.8 + hardhit_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*0.2  + barrel_pct_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*0.8 + barrel_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*0.2  + hr_pct_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*0.8 + hr_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*0.2  + avg_ev_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*0.2  + percentile_90_ev_2021*0.8, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
}

weighting1_function_pa_df <- tibble(
  weight_2021_1 = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), weighting1_function_pa)
)

weighting1_function_pa_df %>% ggplot(aes(weight_2021_1, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = '2021 Weight 1',
       title = 'RMSE by Each 2021 Weight 1 Value')#0

weight_2021_1 <- weighting1_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_1) #0
weight_2022_1 <- 1 - weight_2021_1 #1

weighting2_function_pa <- function(weight_2021_2){
  weight_2022_2 <- 1 - weight_2021_2
  print(paste('Weight 2021 2: ', round(weight_2021_2,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1),
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*0.8 + lineup_pos_avg_2021*0.2,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*0.8 + pa_second_half_2021*0.2,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*0.8 + pa_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*3,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*0.8 + PA_per_game_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*0.8 + onbase_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*0.8 + total_bases2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*0.8 + hardhits_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*0.8 + barrels_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*3,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*0.8 + pitches_faced_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*0.8 + hr_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      cluster_2023 = as.factor(cluster_2023),
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*0.8 + fielding_RV_2021*0.2,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*0.8 + bat_RV_2021*0.2,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*3,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*0.8 + wOBA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*0.8 + hardhit_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*0.8 + barrel_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*0.8 + hr_pct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
}

weighting2_function_pa_df <- tibble(
  weight_2021_2 = seq(0.5, 1, by = 0.1),
  rmse = map_dbl(seq(0.5,1,by = 0.1), weighting2_function_pa)
)

weighting2_function_pa_df %>% ggplot(aes(weight_2021_2, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = '2021 Weight 2',
       title = 'RMSE by Each 2021 Weight 2 Value')#0.6

weight_2021_2 <- weighting2_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_2) #0.6
weight_2022_2 <- 1 - weight_2021_2 #0.4

weighting3_function_pa <- function(weight_2021_3){
  weight_2022_3 <- 1 - weight_2021_3
  print(paste('Weight 2021 3: ', round(weight_2021_3,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1),
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*weight_2022_3 + lineup_pos_avg_2021*weight_2021_3,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*weight_2022_3 + pa_second_half_2021*weight_2021_3,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*weight_2022_3 + pa_diff_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*3,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*weight_2022_3 + PA_per_game_2021*weight_2021_3,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*weight_2022_3 + onbase_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*3,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      cluster_2023 = as.factor(cluster_2023),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*weight_2022_3 + pitches_faced_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*weight_2022_3 + fielding_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*3,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*weight_2022_3 + wOBA_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*weight_2022_3 + hardhit_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*weight_2022_3 + barrel_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*weight_2022_3 + hr_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3 + avg_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3 + percentile_90_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val))
  return(mean(rmse_val))
}

weighting3_function_pa_df <- tibble(
  weight_2021_3 = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), weighting3_function_pa)
)

weighting3_function_pa_df %>% ggplot(aes(weight_2021_3, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = '2021 Weight 3',
       title = 'RMSE by Each 2021 Weight 3 Value')#0.1

weight_2021_3 <- weighting3_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_3) #0.1
weight_2022_3 <- 1 - weight_2021_3 #0.9

weighting4_function_pa <- function(weight_2022_4){
  print(paste('Weight 2022 4: ', round(weight_2022_4,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0,  !(playing_time_2021 == 0 & playing_time_2022 == 0))%>% # removing 2023 rookies
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1),
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23,
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*weight_2022_3 + lineup_pos_avg_2021*weight_2021_3,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*weight_2022_3 + pa_second_half_2021*weight_2021_3,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*weight_2022_3 + pa_diff_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*3,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*weight_2022_3 + PA_per_game_2021*weight_2021_3,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*weight_2022_3 + onbase_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*3,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      cluster_2023 = as.factor(cluster_2023),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*weight_2022_3 + pitches_faced_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*weight_2022_3 + fielding_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*3,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*weight_2022_3 + wOBA_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*weight_2022_3 + hardhit_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*weight_2022_3 + barrel_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*weight_2022_3 + hr_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3 + avg_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3 + percentile_90_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
}

weighting4_function_pa_df <- tibble(
  weight_2022_4 = seq(1, 3, by = 0.2),
  rmse = map_dbl(seq(1,3,by = 0.2), weighting4_function_pa)
)

weighting4_function_pa_df %>% ggplot(aes(weight_2022_4, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = '2022 Weight 4',
       title = 'RMSE by Each 2022 Weight 4 Value')#1

weight_2022_4 <- weighting4_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_4) #1


weighting5_function_pa <- function(weight_2022_5){
  print(paste('Weight 2022 5: ', round(weight_2022_5,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1),
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*weight_2022_3 + lineup_pos_avg_2021*weight_2021_3,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*weight_2022_3 + pa_second_half_2021*weight_2021_3,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*weight_2022_3 + pa_diff_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*3,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*weight_2022_3 + PA_per_game_2021*weight_2021_3,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*weight_2022_3 + onbase_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*3,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      cluster_2023 = as.factor(cluster_2023),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*weight_2022_3 + pitches_faced_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*weight_2022_3 + fielding_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*weight_2022_5,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*weight_2022_5,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*3,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*weight_2022_3 + wOBA_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*weight_2022_3 + hardhit_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*weight_2022_3 + barrel_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*weight_2022_3 + hr_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3 + avg_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3 + percentile_90_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
}

weighting5_function_pa_df <- tibble(
  weight_2022_5 = seq(1, 3, by = 0.2),
  rmse = map_dbl(seq(1,3,by = 0.2), weighting5_function_pa)
)

weighting5_function_pa_df %>% ggplot(aes(weight_2022_5, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1,3, by = 0.2)) +
  labs(y = 'RMSE',
       x = '2022 Weight 5',
       title = 'RMSE by Each 2022 Weight 5 Value')#2.4

weight_2022_5 <- weighting5_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_5) #2.4


weighting6_function_pa <- function(weight_2022_6){
  print(paste('Weight 2022 6: ', round(weight_2022_6,2)))
  
  plate_app_xg <- plate_appearances %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
    reframe(
      playing_time_2021,
      playing_time_2022,
      playing_time_2023,
      prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1), #only applicable to rookies
      prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
      bat_RV_2022 = RV_2022,
      bat_RV_2021 = RV_2022,
      pos,
      age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
      years_since_debut_23,
      lineup_pos_avg = 
        case_when(
          playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
          playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*weight_2022_3 + lineup_pos_avg_2021*weight_2021_3,
          years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
          .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
      debut_month,
      fielding_RV_2022 =  fielding_run_value_2022,
      fielding_RV_2021 =  fielding_run_value_2021,
      pa_second_half = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*weight_2022_3 + pa_second_half_2021*weight_2021_3,
        years_since_debut_23 == 1 ~ pa_second_half_2022,
        .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
      pa_diff = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*weight_2022_3 + pa_diff_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*weight_2022_6,
        .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
      PA_per_game = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*weight_2022_3 + PA_per_game_2021*weight_2021_3,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
        .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
      onbase = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*weight_2022_3 + onbase_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*weight_2022_6,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*weight_2022_6,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*weight_2022_6,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*weight_2022_6,
        .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
      ),
      barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
      barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
      cluster_2023 = as.factor(cluster_2023),
      
      pitches_faced = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*weight_2022_3 + pitches_faced_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*weight_2022_6,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*weight_2022_5,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*weight_2022_6,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    ) %>% 
    rowwise() %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           fielding_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*weight_2022_3 + fielding_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*weight_2022_5,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*weight_2022_6,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*weight_2022_4,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*weight_2022_5,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*weight_2022_6,
             .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
           wOBA = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*weight_2022_3 + wOBA_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
             .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
           hardhit_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*weight_2022_3 + hardhit_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
             .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
           barrel_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*weight_2022_3 + barrel_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
             .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
           hr_pct = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*weight_2022_3 + hr_pct_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
             .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
           avg_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3 + avg_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
           percentile_90_ev = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3 + percentile_90_ev_2021*weight_2021_3,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
    ) %>% 
    select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
           -starts_with('percentile_90_'))
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

weighting6_function_pa_df <- tibble(
  weight_2022_6 = seq(2, 4, by = 0.2),
  rmse = map_dbl(seq(2,4,by = 0.2), weighting6_function_pa)
)

weighting6_function_pa_df %>% ggplot(aes(weight_2022_6, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = '2022 Weight 6',
       title = 'RMSE by Each Weight 6 Value')

weight_2022_6 <- weighting6_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_6) #4

plate_app_xg <- plate_appearances %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
  reframe(
    playing_time_2021,
    playing_time_2022,
    playing_time_2023,
    prop_games_started_2021 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1), #only applicable to rookies
    prop_games_started_2022 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1),
    bat_RV_2022 = RV_2022,
    bat_RV_2021 = RV_2022,
    pos,
    age_23 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23),
    years_since_debut_23,
    lineup_pos_avg = 
      case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ lineup_pos_avg_2022*weight_2022_2  + lineup_pos_avg_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ lineup_pos_avg_2022*weight_2022_3 + lineup_pos_avg_2021*weight_2021_3,
        years_since_debut_23 == 1 ~ lineup_pos_avg_2022,
        .default = lineup_pos_avg_2022*weight_2022_1 + lineup_pos_avg_2021*weight_2021_1),
    debut_month,
    fielding_RV_2022 =  fielding_run_value_2022,
    fielding_RV_2021 =  fielding_run_value_2021,
    pa_second_half = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  pa_second_half_2022*weight_2022_2 + pa_second_half_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_second_half_2022*weight_2022_3 + pa_second_half_2021*weight_2021_3,
      years_since_debut_23 == 1 ~ pa_second_half_2022,
      .default = pa_second_half_2022*weight_2022_1 + pa_second_half_2021*weight_2021_1), 
    pa_diff = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ pa_diff_2022*weight_2022_2 + pa_diff_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pa_diff_2022*weight_2022_3 + pa_diff_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pa_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pa_diff_2022*weight_2022_6,
      .default = pa_diff_2022*weight_2022_1 + pa_diff_2021*weight_2021_1), # PA vs R - PA vs L (absolute value)
    PA_per_game = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ PA_per_game_2022*weight_2022_2  + PA_per_game_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ PA_per_game_2022*weight_2022_3 + PA_per_game_2021*weight_2021_3,
      playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ PA_per_game_2022,
      .default = PA_per_game_2022*weight_2022_1 + PA_per_game_2021*weight_2021_1),
    onbase = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ onbase_2022*weight_2022_2  + onbase_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ onbase_2022*weight_2022_3 + onbase_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ onbase_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ onbase_2022*weight_2022_6,
      .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
    total_bases = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_bases2022*weight_2022_6,
      .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
    ),
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
    hardhits = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhits_2022*weight_2022_6,
      .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
    ),
    hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
    hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
    
    barrels = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
      years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2022*weight_2022_6,
      .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
    ),
    barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
    barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
    cluster_2023 = as.factor(cluster_2023),
    
    pitches_faced = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*weight_2022_2 + pitches_faced_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*weight_2022_3 + pitches_faced_2021*weight_2021_3,
      years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*weight_2022_6,
      .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
    ),
    hr = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
      years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*weight_2022_5,
      years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*weight_2022_6,
      .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
    ),
    hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
    hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022)
  ) %>% 
  rowwise() %>% 
  mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
         rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1,0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
         fielding_RV = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ fielding_RV_2022*weight_2022_2 + fielding_RV_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ fielding_RV_2022*weight_2022_3 + fielding_RV_2021*weight_2021_3,
           years_since_debut_23 == 1 & debut_month <= 4 ~ fielding_RV_2022,
           years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*weight_2022_4,
           years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*weight_2022_5,
           years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*weight_2022_6,
           .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
         bat_RV = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
           years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
           years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*weight_2022_4,
           years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*weight_2022_5,
           years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2022*weight_2022_6,
           .default = bat_RV_2022*weight_2022_1 + bat_RV_2021*weight_2021_1),
         wOBA = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ wOBA_2022*weight_2022_2  + wOBA_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ wOBA_2022*weight_2022_3 + wOBA_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBA_2022,
           .default = wOBA_2022*weight_2022_1 + wOBA_2021*weight_2021_1),
         hardhit_pct = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhit_pct_2022*weight_2022_2  + hardhit_pct_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhit_pct_2022*weight_2022_3 + hardhit_pct_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pct_2022,
           .default = hardhit_pct_2022*weight_2022_1 + hardhit_pct_2021*weight_2021_1),
         barrel_pct = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ barrel_pct_2022*weight_2022_2  + barrel_pct_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrel_pct_2022*weight_2022_3 + barrel_pct_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pct_2022,
           .default = barrel_pct_2022*weight_2022_1 + barrel_pct_2021*weight_2021_1),
         
         
         hr_pct = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ hr_pct_2022*weight_2022_2  + hr_pct_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_pct_2022*weight_2022_3 + hr_pct_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pct_2022,
           .default = hr_pct_2022*weight_2022_1 + hr_pct_2021*weight_2021_1),
         
         avg_ev = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ avg_ev_2022*weight_2022_2  + avg_ev_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3 + avg_ev_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1 + avg_ev_2021*weight_2021_1),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ percentile_90_ev_2022*weight_2022_2  + percentile_90_ev_2021*weight_2021_2, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3 + percentile_90_ev_2021*weight_2021_3,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1 + percentile_90_ev_2021*weight_2021_1)
  ) %>% 
  ungroup() %>% 
  select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
         -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'), -starts_with('wOBA_'),
         -starts_with('percentile_90_'))

plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)


hyperparam_bart_pa <- function(sigdf, sigquant, k, power, row_num){
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf))
  print(paste('Sigquant: ', sigquant))
  print(paste('K: ', k))
  print(paste('Power: ', power))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   sigdf = sigdf,
                   sigquant = sigquant,
                   k = k,
                   power = power,
                   n.trees = 200,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_pa_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_pa_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_pa_0.75_df$sigdf,
                                                 hyperparam_bart_pa_0.75_df$sigquant, 
                                                 hyperparam_bart_pa_0.75_df$k, 
                                                 hyperparam_bart_pa_0.75_df$power,
                                                 hyperparam_bart_pa_0.75_df$row_num), hyperparam_bart_pa)

hyperparam_bart_pa_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_pa_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_pa_0.9_df$sigdf,
                                                hyperparam_bart_pa_0.9_df$sigquant, 
                                                hyperparam_bart_pa_0.9_df$k, 
                                                hyperparam_bart_pa_0.9_df$power,
                                                hyperparam_bart_pa_0.9_df$row_num), hyperparam_bart_pa)

hyperparam_bart_pa_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_pa_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_pa_0.99_df$sigdf,
                                                 hyperparam_bart_pa_0.99_df$sigquant, 
                                                 hyperparam_bart_pa_0.99_df$k, 
                                                 hyperparam_bart_pa_0.99_df$power,
                                                 hyperparam_bart_pa_0.99_df$row_num), hyperparam_bart_pa)

hyperparam_bart_pa_df <- bind_rows(hyperparam_bart_pa_0.75_df, hyperparam_bart_pa_0.9_df, hyperparam_bart_pa_0.99_df)

best_hyperparam_bart_pa <- hyperparam_bart_pa_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

sigdf <- best_hyperparam_bart_pa$sigdf #10
sigquant <- best_hyperparam_bart_pa$sigquant #0.75
k <- best_hyperparam_bart_pa$k # 2
power <- best_hyperparam_bart_pa$power #3

### RMSE = 146.96

hyperparam_bart_trees <- function(trees){
  set.seed(101);split <- initial_split(plate_app_xg, 0.7, strata = playing_time_2023)
  
  train_data <- training(split)
  test_data <- testing(split)
  
  print(paste('Trees: ', trees))

    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) 

    test_x <- test_data %>% select(-playing_time_2023)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
   set.seed(101);model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   sigdf = sigdf,
                   sigquant = sigquant,
                   k = k,
                   power = power,
                   n.trees = trees,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
  rmse_val <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
  print(mean(rmse_val))
  return(mean(rmse_val))
}


find_trees_pa <- tibble(
  trees = seq(200, 1200, by = 100),
  rmse = map_dbl(seq(200, 1200, by = 100), hyperparam_bart_trees)
)

find_trees_pa %>% ggplot(aes(trees, rmse)) +
  geom_line() +
  geom_smooth() #1000

trees <- 1000

# finding final rmse with cross validation
final_rmse_bart_pa <- function(){
  set.seed(101);plate_app_xg$fold <- sample(1:35, nrow(plate_app_xg), replace = TRUE)
  
  rmse_val <- numeric();set.seed(101);for (i in 1:35){
    print(paste('Iteration: ', i))
    train_data <- plate_app_xg %>% 
      filter(fold != i)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    train_data_dummies <- fastDummies::dummy_cols(
      train_data, 
      select_columns = c('pos', 'cluster_2023'), # selects columns to create dummies for
      remove_selected_columns = TRUE) %>% # drops columns that were
      #   # used to create dummies from the final dataframe (i.e. columns 
      #   # level and venue_id are removed)
      select(-fold)
    # train_data_dummies <- train_data %>% select(-fold)
    
    
    test_data <- plate_app_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    # creating dummy variables for factors so I can create a numeric matrix for prediction 
    #(required for BART)
    test_x_dummies <- fastDummies::dummy_cols(
      test_x, 
      select_columns = c("pos", 'cluster_2023'), 
      remove_selected_columns = TRUE)
    
    test_matrix <- as.matrix(test_x_dummies)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data_dummies, 
                   test = test_matrix,
                   sigdf = sigdf,
                   sigquant = sigquant,
                   k = k,
                   power = power,
                   n.trees = trees,
                   n.samples = 3000,
                   n.burn = 1000,
                   n.threads = 2, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$playing_time_2023, ifelse(predictions <= 0.5, 0, predictions))
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

(final_rmse <- final_rmse_bart_pa()) #147.06
