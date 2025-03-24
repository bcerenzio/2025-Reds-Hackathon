library(xgboost)
library(fastDummies)
library(tidyverse)
library(rsample)

slice <- dplyr::slice
#### NEW VERSION ####

#histogram of debut months
plate_appearances %>% ggplot(aes(debut_month)) + 
  geom_histogram(bins = 8, fill = 'blue', color = 'gray', breaks = seq(3,11, by = 1)) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(3,11, by = 1))

#debut month of players who debuted in 2022 as a function of 2022 PA vs 2023 PA
plate_appearances %>% 
  filter(years_since_debut_23 == 1)%>% 
  ggplot(aes(playing_time_2022, playing_time_2023, color = debut_month)) + 
  geom_point() + 
  scale_color_continuous(low = 'red',high = 'green') +
  theme_bw() +
  labs(y = 'PA 2023',
       x = 'PA 2022',
       color = 'Debut Month')

plate_appearances %>% 
  filter(years_since_debut_23 == 1)%>% 
  ggplot(aes(playing_time_2022, playing_time_2023, color = wOBA_2022)) + 
  geom_point() + 
  scale_color_continuous(low = 'red',high = 'green') +
  theme_bw() +
  labs(y = 'PA 2023',
       x = 'PA 2022',
       color = '2022 wOBA')

plate_appearances %>% 
  filter(years_since_debut_23 == 1)%>% 
  ggplot(aes(playing_time_2022, playing_time_2023, color = barrel_pct_2022)) + 
  geom_point() + 
  scale_color_continuous(low = 'red',high = 'green', limits = c(0,0.2), oob = scales::squish) +
  theme_bw() +
  labs(y = 'PA 2023',
       x = 'PA 2022',
       color = '2022 Barrel%')

plate_appearances %>% 
  filter(years_since_debut_23 == 1)%>% 
  ggplot(aes(playing_time_2022, playing_time_2023, color = hardhit_pct_2022)) + 
  geom_point() + 
  scale_color_continuous(low = 'red',high = 'green',
                         limits = c(0.25, 0.5), oob = scales::squish,
                         breaks = seq(0.25, 0.5, by = 0.05)) +
  theme_bw() +
  labs(y = 'PA 2023',
       x = 'PA 2022',
       color = '2022 HardHit%')

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
    bat_RV_2021 = RV_2021,
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
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
      years_since_debut_23 == 1  & debut_month > 8  ~ pa_diff_2022*3,
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
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*2.2,
      years_since_debut_23 == 1  & debut_month > 8  ~ onbase_2022*3,
      .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
    total_bases = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*0.2  + total_bases2021*0.8, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*0.8 + total_bases2021*0.2,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*2.2,
      years_since_debut_23 == 1  & debut_month > 8  ~ total_bases2022*3,
      .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
    ),
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
    hardhits = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*0.2 + hardhits_2021*0.8, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*0.8 + hardhits_2021*0.2,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*2.2,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*3,
      .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
    ),
    hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
    hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
    
    barrels = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*0.2+ barrels_2021*0.8, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*0.8 + barrels_2021*0.2,
      years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2022*2.2,
      years_since_debut_23 == 1  & debut_month > 8  ~ barrels_2022*3,
      .default = barrels_2022*weight_2022_1 + barrels_2021*weight_2021_1
    ),
    barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
    barrel_pct_2021 = ifelse(is.na(barrel_pct_2021), quantile(barrel_pct_2021, probs = 0.1, na.rm = TRUE), barrel_pct_2021),
    cluster_2023 = as.factor(cluster_2023),
    
    pitches_faced = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  pitches_faced_2022*0.2 + pitches_faced_2021*0.8, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ pitches_faced_2022*0.8 + pitches_faced_2021*0.2,
      years_since_debut_23 == 1 & debut_month <= 4 ~ pitches_faced_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_faced_2022*3,
      .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
    ),
    
    hr = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*0.2 + hr_2021*0.8, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*0.8 + hr_2021*0.2,
      years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
      years_since_debut_23 == 1  & debut_month > 8  ~ hr_2022*3,
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
           years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1,
           years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
           years_since_debut_23 == 1  & debut_month > 8 ~ fielding_RV_2022*3,
           .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
         bat_RV = case_when(
           playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*0.2 + bat_RV_2021*0.8, 
           playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*0.8 + bat_RV_2021*0.2,
           years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
           years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1,
           years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2022*2.2,
           years_since_debut_23 == 1  & debut_month > 8  ~ bat_RV_2022*3,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)

plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)



dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)


set.seed(101);mod <- xgb.cv(
  params = list(
    eta = 0.001,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = 1,
    lambda = 1,
    alpha = 0,
    max_depth = 2,
    min_child_weight = 12,
    subsample = 0.5
  ),
  data = dtrain,
  nrounds = 200000,
  nfold = 35,
  print_every_n = 500,
  early_stopping_rounds = 1000,
  nthread = 7,
  seed = 101
) 

rmse <- mod$evaluation_log %>% 
  slice_min(test_rmse_mean, n = 1) %>% 
  pull(test_rmse_mean)

return(rmse)

}

weighting1_function_pa_df <- tibble(
  weight_2021_1 = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), weighting1_function_pa)
)

weighting1_function_pa_df %>% ggplot(aes(weight_2021_1, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +#rmse = 147.4
  labs(y = 'RMSE',
       x = 'Weight 2021 1 (PA)')
  
ggsave('Batter Weight 1 2021.png', width = 4, height = 4.76)

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
      bat_RV_2021 = RV_2021,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*0.8 + total_bases2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*0.8 + hardhits_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*0.8 + barrels_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*0.8 + hr_2021*0.2,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2022*3,
        .default = hr_2022*weight_2022_1 + hr_2021*weight_2021_1
      ),
      
      hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
      hr_pct_2021 = ifelse(is.na(hr_pct_2021), quantile(hr_pct_2021, probs = 0.1, na.rm = TRUE), hr_pct_2021),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      cluster_2023 = as.factor(cluster_2023)
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
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*0.8 + bat_RV_2021*0.2,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  
  
  dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 12,
      subsample = 0.5
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting2_function_pa_df <- tibble(
  weight_2021_2 = seq(0.6, 1, by = 0.05),
  rmse = map_dbl(seq(0.6,1,by = 0.05), weighting2_function_pa)
)

weighting2_function_pa_df %>% ggplot(aes(weight_2021_2, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  labs(y = 'RMSE',
       x = 'Weight 2021 2 (PA)')

ggsave('Batter Weight 2 2021.png', width = 4, height = 4.76)

weight_2021_2 <- weighting2_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_2) #1
weight_2022_2 <- 1 - weight_2021_2 #0

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
      bat_RV_2021 = RV_2021,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*3,
        .default = hardhits_2022*weight_2022_1 + hardhits_2021*weight_2021_1
      ),
      hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
      hardhit_pct_2021 = ifelse(is.na(hardhit_pct_2021), quantile(hardhit_pct_2021, probs = 0.1, na.rm = TRUE), hardhit_pct_2021),
      
      barrels = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  barrels_2022*weight_2022_2 + barrels_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ barrels_2022*weight_2022_3 + barrels_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ barrels_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2022*1,
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
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2022*1,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2022*2.2,
        years_since_debut_23 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2022*3,
        .default = pitches_faced_2022*weight_2022_1 + pitches_faced_2021*weight_2021_1
      ),
      
      hr = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~  hr_2022*weight_2022_2 + hr_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hr_2022*weight_2022_3 + hr_2021*weight_2021_3,
        years_since_debut_23 == 1 & debut_month <= 4 ~ hr_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2022*1,
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
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2022*1,
             years_since_debut_23 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2022*3,
             .default = fielding_RV_2022*weight_2022_1 + fielding_RV_2021*weight_2021_1),
           bat_RV = case_when(
             playing_time_2021 > 100 & playing_time_2022 < 30 ~ bat_RV_2022*weight_2022_2 + bat_RV_2021*weight_2021_2, 
             playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ bat_RV_2022*weight_2022_3 + bat_RV_2021*weight_2021_3,
             years_since_debut_23 == 1 & debut_month <= 4 ~ bat_RV_2022,
             years_since_debut_23 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2022*1,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  
  
  dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 12,
      subsample = 0.5
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
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
       x = 'Weight 2021 3 (PA)')

ggsave('Batter Weight 3 2021.png', width = 4, height = 4.76)

weight_2021_3 <- weighting3_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_3) #0.05
weight_2022_3 <- 1 - weight_2021_3 #0.95

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
      bat_RV_2021 = RV_2021,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*3,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  
  dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 12,
      subsample = 0.5
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting4_function_pa_df <- tibble(
  weight_2022_4 = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), weighting4_function_pa)
)

weighting4_function_pa_df %>% ggplot(aes(weight_2022_4, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw()+
  labs(y = 'RMSE',
       x = 'Weight 2022 4 (PA)')

ggsave('Batter Weight 4 2022.png', width = 4, height = 4.76)



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
      bat_RV_2021 = RV_2021,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*3,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*3,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*3,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 12,
      subsample = 0.5
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting5_function_pa_df <- tibble(
  weight_2022_5 = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), weighting5_function_pa)
)

weighting5_function_pa_df %>% ggplot(aes(weight_2022_5, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1,3, by = 0.2)) +
  labs(y = 'RMSE',
       x = 'Weight 2022 5 (PA)')

ggsave('Batter Weight 5 2022.png', width = 4, height = 4.76)


weight_2022_5 <- weighting5_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_5) #1.7


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
      bat_RV_2021 = RV_2021,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*weight_2022_6,
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
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*weight_2022_6,
        .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
      total_bases = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*weight_2022_6,
        .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
      ),
      wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
      wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
      hardhits = case_when(
        playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
        playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*weight_2022_6,
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
           -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
           -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)
  
  plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
    dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)
  
  dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 12,
      subsample = 0.5
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting6_function_pa_df <- tibble(
  weight_2022_6 = seq(1.5, 3.5, by = 0.1),
  rmse = map_dbl(seq(1.5,3.5,by = 0.1), weighting6_function_pa)
)

weighting6_function_pa_df %>% ggplot(aes(weight_2022_6, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw()+
  labs(y = 'RMSE',
       x = 'Weight 2022 6 (PA)')

ggsave('Batter Weight 6 2022.png', width = 4, height = 4.76)


weight_2022_6 <- weighting6_function_pa_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_6) #2.9

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
    bat_RV_2021 = RV_2021,
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*weight_2022_6,
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*weight_2022_6,
      .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
    total_bases = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*weight_2022_6,
      .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
    ),
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
    hardhits = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*weight_2022_6,
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
  select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
         -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
         -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)

plate_app_xg_x <- plate_app_xg %>% select(-playing_time_2023) %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)

dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2023)


hyperparam_pa_tuning_reg <- function(max_depth, weight, lambda,
                                     gam,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Lambda: ', lambda))
  print(paste('Gamma: ', gam))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse', 
      gamma = gam,
      lambda = lambda,
      alpha = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_df <- expand_grid(
  max_depth = c(1,2,3),
  weight = c(8,12,16,20),
  lambda = c(1,10),
  gam = c(1,3),
  subsample = c(0.3,0.4, 0.5)
) %>% 
  mutate(row_num = row_number())

reg_tuning_df <- reg_tuning_df %>% 
  rowwise() %>% 
  mutate(
    rmse = pmap_dbl(list(max_depth, weight, lambda, gam, subsample, row_num), hyperparam_pa_tuning_reg)
    ) %>% 
  ungroup()

reg_tuning_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_best <- reg_tuning_df %>% 
    slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

gam <- reg_tuning_best$gam #1
lambda <- reg_tuning_best$lambda #1
max_depth <- reg_tuning_best$max_depth #1
weight <- reg_tuning_best$weight #16
subsample<- reg_tuning_best$subsample #0.4


hyperparam_pa_tuning_col <- function(by_tree, by_level, 
                                     by_node, row_num){
  
  print(paste('By Tree: ', by_tree))
  print(paste('By Level: ', by_level))
  print(paste('By Node: ', by_node))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam,
      lambda = lambda,
      alpha = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      colsample_bytree = by_tree,
      colsample_bylevel = by_level,
      colsample_bynode = by_node
      ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

pa_tuning_col <- expand_grid(
  by_tree = c(0.33,0.67,1),
  by_level = c(0.33,0.67,1),
  by_node = c(0.33, 0.67, 1)
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(by_tree, by_level, by_node, row_num),hyperparam_pa_tuning_col)) %>% 
  ungroup()

pa_tuning_col %>% 
  arrange(rmse) %>% 
  head(5)

pa_tuning_col_best <- pa_tuning_col %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

by_tree <- pa_tuning_col_best$by_tree #0.67
by_level <- pa_tuning_col_best$by_level #0.67
by_node <- pa_tuning_col_best$by_node #0.67

hyperparam_pa_tuning_methods <- function(tree_method, grow_policy, row_num){
  
  print(paste('Tree Method: ', tree_method))
  print(paste('Grow Policy : ', grow_policy))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam,
      lambda = lambda,
      alpha = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      colsample_bytree = by_tree,
      colsample_bylevel = by_level,
      colsample_bynode = by_node,
      tree_method = tree_method,
      grow_policy = grow_policy
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 1000,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}


pa_tuning_methods <- expand_grid(
  tree_method = c('hist','approx','exact'),
  grow_policy = c('depthwise','lossguide')
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(tree_method, grow_policy, row_num), hyperparam_pa_tuning_methods)) %>% 
  ungroup()

pa_tuning_methods %>% 
  arrange(rmse) %>% 
  head(5)

pa_tuning_methods_best <- pa_tuning_methods %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

tree_method <- pa_tuning_methods_best$tree_method #approx
grow_policy <- pa_tuning_methods_best$grow_policy #depthwise

hyperparam_pa_tuning_eta <- function(eta, early_stopping_rounds, row_num){
  
  print(paste('eta: ', eta))
  print(paste('Early Stopping Rounds : ', early_stopping_rounds))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = eta,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam,
      lambda = lambda,
      alpha = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      colsample_bytree = by_tree,
      colsample_bylevel = by_level,
      colsample_bynode = by_node,
      tree_method = tree_method,
      grow_policy = grow_policy
    ),
    data = dtrain,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 1000,
    early_stopping_rounds = early_stopping_rounds,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  nrounds <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(iter)
  
  df <- tibble(
    rmse, 
    nrounds,
    eta,
    early_stopping_rounds
  )
  
  return(df)
  
}


hyperparam_pa_tuning_eta_df <- tibble(
  eta = 0.001/1:5,
  early_stopping_rounds = 1000*1:5,
  row_num = 1:5
)

hyperparam_pa_tuning_eta_results <- pmap_df(list(
  hyperparam_pa_tuning_eta_df$eta, hyperparam_pa_tuning_eta_df$early_stopping_rounds,
  hyperparam_pa_tuning_eta_df$row_num
), hyperparam_pa_tuning_eta)

hyperparam_pa_tuning_eta_results %>% 
  arrange(rmse) %>% 
  head(5)

best_eta_tuning <- hyperparam_pa_tuning_eta_results %>% 
  slice_min(rmse)

eta <- best_eta_tuning$eta #0.001
nrounds <- best_eta_tuning$nrounds #13924
### RMSE = 144.32 ###


#### Train Model ####
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
    bat_RV_2021 = RV_2021,
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*weight_2022_6,
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*weight_2022_6,
      .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
    total_bases = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*weight_2022_6,
      .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
    ),
    
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
    hardhits = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*weight_2022_6,
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
         -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
         -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)

set.seed(101);split <- initial_split(plate_app_xg, prop = 0.7, strata = playing_time_2023)

train <- training(split)
test <- testing(split)

train_x <- train %>% select(-playing_time_2023) %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)

dtrain <- xgb.DMatrix(as.matrix(train_x), label = train$playing_time_2023)

test_x <- test %>% select(-playing_time_2023) %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2023'), remove_selected_columns = TRUE)

dtest <- xgb.DMatrix(as.matrix(test_x), label = test$playing_time_2023)

set.seed(101);mod <- xgb.train(
  params = list(
    eta = eta,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = gam,
    lambda = lambda,
    alpha = 0,
    max_depth = max_depth,
    min_child_weight = weight,
    subsample = subsample,
    colsample_bytree = by_tree,
    colsample_bylevel = by_level,
    colsample_bynode = by_node,
    tree_method = tree_method,
    grow_policy = grow_policy
  ),
  data = dtrain,
  nrounds = nrounds,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 500
)

xgb.ggplot.importance(xgb.importance(model =mod),top_n = 20) +
  ggthemes::theme_few()

test$preds <- predict(mod, newdata = as.matrix(test %>% 
                                                         dummy_cols(select_columns = c('pos','cluster_2023'), remove_selected_columns = TRUE) %>% 
                                                         select(-playing_time_2023)))

test %>% ggplot(aes(playing_time_2023)) +
  geom_density(fill = 'blue', alpha = 0.5, adjust = 1) +
  geom_density(data = test, aes(preds), fill = 'red', alpha = 0.5, adjust = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,800, by = 100)) +
  geom_vline(xintercept = 397, linetype = 'dashed') + 
  geom_vline(xintercept = 56, linetype = 'dashed') +
  labs(title = 'Distribution of PA in 2023 (Blue) Compared to the\nDistribution of Predicted PA (Red)',
       x = 'PA (2023)',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 2') +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))

test <- test %>% 
  mutate(diff = playing_time_2023 - preds,
         pos = case_when(
           pos == 2 ~ 'C',
           pos == 3 ~ '1B',
           pos == 4 ~ '2B',
           pos == 5 ~ '3B',
           pos == 6 ~ 'SS',
           pos == 7 ~ 'LF',
           pos == 8 ~ 'CF',
           pos == 9 ~ 'RF',
           pos == 10 ~ 'DH',
           pos == 11 ~ 'Unknown'
         ))

test$pos <- factor(test$pos, levels = c('C','1B','2B','3B','SS','LF','CF','RF','DH','Unknown'))
  
test %>% ggplot(aes(pos, diff, group = pos, fill = pos)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = 'Prediction Error\n(PA 2023 - Predicted PA 2023)',
       x = 'Position',
       title = 'Prediction Accuracy By Batter Position',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 3')+
  guides(fill = 'none') +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))

test %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (Batters)",
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)',
       subtitle = 'Predicting on Untrained Data')+
  scale_y_continuous(breaks = seq(-500, 500, by = 100)) +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No'))) # p = 0.51

test %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (Batters)",
       y = 'Prediction Error (PA - Predicted PA)',
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 4')+
  scale_y_continuous(breaks = seq(-400, 600, by = 100),
                     limits = c(-400, 600)) +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No')), annotations = 'p = 0.51') +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))
  
test %>% 
  filter(rookie_year_2022 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2021 = ifelse(rookie_year_2021 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2021), diff, group = rookie_year_2021, fill = as.factor(rookie_year_2021))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2021",
       x = "Rookie Year 2021",
       title = 'Prediction Error on 2021 Rookies vs Rest of Data\n(Excluding 2022 Rookies)',
       subtitle = 'Predicting on Untrained Data') +
  scale_y_continuous(breaks = seq(-500, 500, by = 100))
  
  


  
test$diffs <- abs(test$playing_time_2023 - test$preds)

test %>% ggplot(aes(x = playing_time_2021, y = playing_time_2022, color = diffs)) +
  geom_jitter(position = position_jitter(width = 10, height = 10, seed = 101)) +
  scale_color_continuous(low = 'green',high = 'red') + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0,800, by = 100)) +
  labs(color = 'Absolute\nError',
       x = 'PA 2021',
       y = 'PA 2022',
       title = 'Evaluating The Error of Our Predictions on\n2021 & 2022 PA')

#### Final XGBoost Model ####

plate_app_xg <- plate_appearances %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies
  reframe(
    playing_time_2021,
    playing_time_2022,
    playing_time_2024 = playing_time_2023, #renaming for predictions
    prop_games_started_2022 = ifelse(years_since_debut_23 == 2, prop_games_started_2021, 1), #renaming for predictions
    prop_games_started_2023 = ifelse(years_since_debut_23 == 1, prop_games_started_2022, 1), #renaming for predictions
    bat_RV_2022 = RV_2022,
    bat_RV_2021 = RV_2021,
    pos,
    age_24 = ifelse(is.na(age_23), mean(age_23, na.rm = TRUE), age_23), #renaming for predictions
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pa_diff_2022*weight_2022_6,
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
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ onbase_2022*weight_2022_6,
      .default = onbase_2022*weight_2022_1 + onbase_2021*weight_2021_1),
    total_bases = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ total_bases2022*weight_2022_2  + total_bases2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ total_bases2022*weight_2022_3 + total_bases2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_bases2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_bases2022*weight_2022_6,
      .default = total_bases2022*weight_2022_1 + total_bases2021*weight_2021_1
    ),
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    wOBA_2021 = ifelse(is.na(wOBA_2021), quantile(wOBA_2021, probs = 0.1, na.rm = TRUE), wOBA_2021),
    hardhits = case_when(
      playing_time_2021 > 100 & playing_time_2022 < 30 ~ hardhits_2022*weight_2022_2 + hardhits_2021*weight_2021_2, 
      playing_time_2021 < 30 & playing_time_2022 > 100 & years_since_debut_23 != 1 ~ hardhits_2022*weight_2022_3 + hardhits_2021*weight_2021_3,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhits_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2022*weight_2022_4,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2022*weight_2022_5,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhits_2022*weight_2022_6,
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
    
    cluster_2024 = as.factor(cluster_2023), #renaming for predictions
    
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
  mutate(rookie_year_2023 = ifelse(years_since_debut_23 == 1, 1, 0), #renaming for predictions
         rookie_year_2022 = ifelse(years_since_debut_23 == 2, 1,0), # renaming for predictions
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0), # renaming for predictions
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0), # renaming for predictions
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
         -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
         -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month) %>% 
  rename('playing_time_2023' = 'playing_time_2022',
         'playing_time_2022' = 'playing_time_2021',
         'years_since_debut_24' = 'years_since_debut_23')

plate_app_xg_x <- plate_app_xg %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2024'), remove_selected_columns = TRUE) %>% 
  select(-playing_time_2024)
  
glimpse(plate_app_xg_x) #checking structure

dtrain <- xgb.DMatrix(as.matrix(plate_app_xg_x), label = plate_app_xg$playing_time_2024)


set.seed(101);pa_mod <- xgboost(
  params = list(
    eta = eta,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = gam,
    lambda = lambda,
    alpha = 0,
    max_depth = max_depth,
    min_child_weight = weight,
    subsample = subsample,
    colsample_bytree = by_tree,
    colsample_bylevel = by_level,
    colsample_bynode = by_node,
    tree_method = tree_method,
    grow_policy = grow_policy
  ),
  data = dtrain,
  nrounds = nrounds,
 # watchlist = list(train = dtrain, test = dtest),
  print_every_n = 500
)

#saving model to R directory
save(pa_mod, file = 'Plate Appearance Model_edited.RData')



