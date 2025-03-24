library(tidyverse)
library(fastDummies)
library(xgboost)

slice <- dplyr::slice

submission <- read_csv('sample_submission.csv')
#load in models
load(file = 'Plate Appearance Model_edited.RData')
load(file = 'BF SP Model_edited.RData')
load(file = 'BF RP Model_edited_updated.RData')


#### PA predictions ####
plate_app_xg_2024 <- plate_appearances %>% 
  filter(batter %in% unique(submission$PLAYER_ID)) %>% 
  reframe(
    PLAYER_ID = batter,
    playing_time_2022,
    playing_time_2023,
    prop_games_started_2022 = ifelse(years_since_debut_24 == 2, prop_games_started_2022, 1), 
    prop_games_started_2023 = ifelse(years_since_debut_24 == 1, prop_games_started_2023, 1), 
    bat_RV_2023 = RV_2023,
    bat_RV_2022 = RV_2022,
    pos,
    age_24 = ifelse(is.na(age_24), mean(age_24, na.rm = TRUE), age_24), 
    years_since_debut_24,
    lineup_pos_avg = 
      case_when(
        playing_time_2022 > 100 & playing_time_2023 < 30 ~ lineup_pos_avg_2023*weight_2022_2  + lineup_pos_avg_2022*weight_2021_2, 
        playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ lineup_pos_avg_2023*weight_2022_3 + lineup_pos_avg_2022*weight_2021_3,
        years_since_debut_24 == 1 ~ lineup_pos_avg_2023,
        .default = lineup_pos_avg_2023*weight_2022_1 + lineup_pos_avg_2022*weight_2021_1),
    debut_month,
    fielding_RV_2023 =  fielding_run_value_2023,
    fielding_RV_2022 =  fielding_run_value_2022,
    pa_second_half = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~  pa_second_half_2023*weight_2022_2 + pa_second_half_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ pa_second_half_2023*weight_2022_3 + pa_second_half_2022*weight_2021_3,
      years_since_debut_24 == 1 ~ pa_second_half_2023,
      .default = pa_second_half_2023*weight_2022_1 + pa_second_half_2022*weight_2021_1), 
    pa_diff = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~ pa_diff_2023*weight_2022_2 + pa_diff_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ pa_diff_2023*weight_2022_3 + pa_diff_2022*weight_2021_3,
      years_since_debut_24 == 1  & debut_month <= 4 ~ pa_diff_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ pa_diff_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ pa_diff_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month > 8  ~ pa_diff_2023*weight_2022_6,
      .default = pa_diff_2023*weight_2022_1 + pa_diff_2022*weight_2021_1), # PA vs R - PA vs L (absolute value)
    PA_per_game = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~ PA_per_game_2023*weight_2022_2  + PA_per_game_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ PA_per_game_2023*weight_2022_3 + PA_per_game_2022*weight_2021_3,
      playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ PA_per_game_2023,
      .default = PA_per_game_2023*weight_2022_1 + PA_per_game_2022*weight_2021_1),
    onbase = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~ onbase_2023*weight_2022_2  + onbase_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ onbase_2023*weight_2022_3 + onbase_2022*weight_2021_3,
      years_since_debut_24 == 1  & debut_month <= 4 ~ onbase_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ onbase_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ onbase_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month > 8  ~ onbase_2023*weight_2022_6,
      .default = onbase_2023*weight_2022_1 + onbase_2022*weight_2021_1),
    total_bases = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~ total_bases2023*weight_2022_2  + total_bases2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ total_bases2023*weight_2022_3 + total_bases2022*weight_2021_3,
      years_since_debut_24 == 1  & debut_month <= 4 ~ total_bases2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ total_bases2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ total_bases2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month > 8  ~ total_bases2023*weight_2022_6,
      .default = total_bases2023*weight_2022_1 + total_bases2022*weight_2021_1
    ),
    wOBA_2023 = ifelse(is.na(wOBA_2023), quantile(wOBA_2023, probs = 0.1, na.rm = TRUE), wOBA_2023),
    wOBA_2022 = ifelse(is.na(wOBA_2022), quantile(wOBA_2022, probs = 0.1, na.rm = TRUE), wOBA_2022),
    hardhits = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~ hardhits_2023*weight_2022_2 + hardhits_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ hardhits_2023*weight_2022_3 + hardhits_2022*weight_2021_3,
      years_since_debut_24 == 1  & debut_month <= 4 ~ hardhits_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4  ~ hardhits_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhits_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month > 8  ~ hardhits_2023*weight_2022_6,
      .default = hardhits_2023*weight_2022_1 + hardhits_2022*weight_2021_1
    ),
    hardhit_pct_2023 = ifelse(is.na(hardhit_pct_2023), quantile(hardhit_pct_2023, probs = 0.1, na.rm = TRUE), hardhit_pct_2023),
    hardhit_pct_2022 = ifelse(is.na(hardhit_pct_2022), quantile(hardhit_pct_2022, probs = 0.1, na.rm = TRUE), hardhit_pct_2022),
    
    barrels = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~  barrels_2023*weight_2022_2 + barrels_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ barrels_2023*weight_2022_3 + barrels_2022*weight_2021_3,
      years_since_debut_24 == 1 & debut_month <= 4 ~ barrels_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ barrels_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ barrels_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month <= 10 & debut_month > 8  ~ barrels_2023*weight_2022_6,
      .default = barrels_2023*weight_2022_1 + barrels_2022*weight_2021_1
    ),
    barrel_pct_2023 = ifelse(is.na(barrel_pct_2023), quantile(barrel_pct_2023, probs = 0.1, na.rm = TRUE), barrel_pct_2023),
    barrel_pct_2022 = ifelse(is.na(barrel_pct_2022), quantile(barrel_pct_2022, probs = 0.1, na.rm = TRUE), barrel_pct_2022),
    
    cluster_2024 = as.factor(cluster_2024),
    
    pitches_faced = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~  pitches_faced_2023*weight_2022_2 + pitches_faced_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ pitches_faced_2023*weight_2022_3 + pitches_faced_2022*weight_2021_3,
      years_since_debut_24 == 1 & debut_month <= 4 ~ pitches_faced_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_faced_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_faced_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month <= 10 & debut_month > 8  ~ pitches_faced_2023*weight_2022_6,
      .default = pitches_faced_2023*weight_2022_1 + pitches_faced_2022*weight_2021_1
    ),
    hr = case_when(
      playing_time_2022 > 100 & playing_time_2023 < 30 ~  hr_2023*weight_2022_2 + hr_2022*weight_2021_2, 
      playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ hr_2023*weight_2022_3 + hr_2022*weight_2021_3,
      years_since_debut_24 == 1 & debut_month <= 4 ~ hr_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ hr_2023*weight_2022_4,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ hr_2023*weight_2022_5,
      years_since_debut_24 == 1  & debut_month <= 10 & debut_month > 8  ~ hr_2023*weight_2022_6,
      .default = hr_2023*weight_2022_1 + hr_2022*weight_2021_1
    ),
    hr_pct_2023 = ifelse(is.na(hr_pct_2023), quantile(hr_pct_2023, probs = 0.1, na.rm = TRUE), hr_pct_2023),
    hr_pct_2022 = ifelse(is.na(hr_pct_2022), quantile(hr_pct_2022, probs = 0.1, na.rm = TRUE), hr_pct_2022),
    
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    avg_ev_2023 = ifelse(is.na(avg_ev_2023), quantile(avg_ev_2023, probs = 0.1, na.rm = TRUE), avg_ev_2023),
    
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    percentile_90_ev_2023 = ifelse(is.na(percentile_90_ev_2023), quantile(percentile_90_ev_2023, probs = 0.1, na.rm = TRUE), percentile_90_ev_2023),
  ) %>% 
  rowwise() %>% 
  mutate(rookie_year_2023 = ifelse(years_since_debut_24 == 1, 1, 0),
         rookie_year_2022 = ifelse(years_since_debut_24 == 2, 1,0),
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0), 
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0), 
         fielding_RV = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ fielding_RV_2023*weight_2022_2 + fielding_RV_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ fielding_RV_2023*weight_2022_3 + fielding_RV_2022*weight_2021_3,
           years_since_debut_24 == 1 & debut_month <= 4 ~ fielding_RV_2023,
           years_since_debut_24 == 1 & debut_month <= 6  & debut_month > 4 ~ fielding_RV_2023*weight_2022_4,
           years_since_debut_24 == 1  & debut_month > 6 & debut_month <= 8  ~ fielding_RV_2023*weight_2022_5,
           years_since_debut_24 == 1  & debut_month > 8 & debut_month <= 10  ~ fielding_RV_2023*weight_2022_6,
           .default = fielding_RV_2023*weight_2022_1 + fielding_RV_2022*weight_2021_1),
         bat_RV = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ bat_RV_2023*weight_2022_2 + bat_RV_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ bat_RV_2023*weight_2022_3 + bat_RV_2022*weight_2021_3,
           years_since_debut_24 == 1 & debut_month <= 4 ~ bat_RV_2023,
           years_since_debut_24 == 1 & debut_month <= 6  & debut_month > 4 ~ bat_RV_2023*weight_2022_4,
           years_since_debut_24 == 1  & debut_month > 6 & debut_month <= 8  ~ bat_RV_2023*weight_2022_5,
           years_since_debut_24 == 1  & debut_month > 8 & debut_month <= 10  ~ bat_RV_2023*weight_2022_6,
           .default = bat_RV_2023*weight_2022_1 + bat_RV_2022*weight_2021_1),
         wOBA = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ wOBA_2023*weight_2022_2  + wOBA_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ wOBA_2023*weight_2022_3 + wOBA_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ wOBA_2023,
           .default = wOBA_2023*weight_2022_1 + wOBA_2022*weight_2021_1),
         hardhit_pct = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ hardhit_pct_2023*weight_2022_2  + hardhit_pct_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ hardhit_pct_2023*weight_2022_3 + hardhit_pct_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ hardhit_pct_2023,
           .default = hardhit_pct_2023*weight_2022_1 + hardhit_pct_2022*weight_2021_1),
         barrel_pct = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ barrel_pct_2023*weight_2022_2  + barrel_pct_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ barrel_pct_2023*weight_2022_3 + barrel_pct_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ barrel_pct_2023,
           .default = barrel_pct_2023*weight_2022_1 + barrel_pct_2022*weight_2021_1),
         hr_pct = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ hr_pct_2023*weight_2022_2  + hr_pct_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ hr_pct_2023*weight_2022_3 + hr_pct_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ hr_pct_2023,
           .default = hr_pct_2023*weight_2022_1 + hr_pct_2022*weight_2021_1),
         
         avg_ev = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ avg_ev_2023*weight_2022_2  + avg_ev_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ avg_ev_2023*weight_2022_3 + avg_ev_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ avg_ev_2023,
           .default = avg_ev_2023*weight_2022_1 + avg_ev_2022*weight_2021_1),
         
         percentile_90_ev = case_when(
           playing_time_2022 > 100 & playing_time_2023 < 30 ~ percentile_90_ev_2023*weight_2022_2  + percentile_90_ev_2022*weight_2021_2, 
           playing_time_2022 < 30 & playing_time_2023 > 100 & years_since_debut_24 != 1 ~ percentile_90_ev_2023*weight_2022_3 + percentile_90_ev_2022*weight_2021_3,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ percentile_90_ev_2023,
           .default = percentile_90_ev_2023*weight_2022_1 + percentile_90_ev_2022*weight_2021_1)
  ) %>% 
  select(-starts_with('fielding_RV_'), -starts_with('bat_RV_'), -starts_with('barrel_pct_'),
         -starts_with('hardhit_pct_'),-starts_with('hr_pct_'), -starts_with('avg_ev_'),
         -starts_with('percentile_90_'), -starts_with('wOBA_'),-debut_month)

plate_app_xg_2024 <- plate_app_xg_2024 %>% 
  dummy_cols(select_columns = c('pos', 'cluster_2024'), remove_selected_columns = TRUE) 

plate_app_xg_2024$PLAYING_TIME <- predict(pa_mod, 
                                          as.matrix(plate_app_xg_2024 %>% select(-PLAYER_ID)))

#### SP Predictions ####
batters_faced_xg_2024 <- batters_faced %>% 
  filter(pitcher %in% unique(submission$PLAYER_ID)) %>% 
  filter(role_key_2024 == 'SP') %>% 
  reframe(
    PLAYER_ID = pitcher,
    playing_time_2023,
    playing_time_2022,
    RV_2023 = ifelse(is.na(RV_2023), 0, RV_2023),
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV100_2023 = ifelse(is.na(RV100_2023), quantile(RV100_2023, probs = 0.1, na.rm = TRUE), RV100_2023),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    debut_month, 
    age_24,
    years_since_debut_24,
    bf_second_half = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ bf_second_half_2023*weight_2022_2sp  + bf_second_half_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ bf_second_half_2023*weight_2022_3sp + bf_second_half_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ bf_second_half_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ bf_second_half_2023*weight_2022_6sp,
      .default = bf_second_half_2023*weight_2022_1sp + bf_second_half_2022*weight_2021_1sp
    ),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    wOBAA_2023 = ifelse(is.na(wOBAA_2023), quantile(wOBAA_2023, probs = 0.9, na.rm = TRUE), wOBAA_2023),
    bf_diff = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ bf_diff_2023*weight_2022_2sp  + bf_diff_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ bf_diff_2023*weight_2022_3sp + bf_diff_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ bf_diff_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ bf_diff_2023*weight_2022_6sp,
      .default = bf_diff_2023*weight_2022_1sp + bf_diff_2022*weight_2021_1sp
    ),
    times_faced_2023,
    times_faced_2022,
    avg_pitches_per_appearance_2023,
    avg_pitches_per_appearance_2022,
    avg_bf_per_appearance_2022,
    avg_bf_per_appearance_2023,
    earned_runs_2023 = ifelse(is.na(earned_runs_2023), 0,earned_runs_2023),
    earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
    ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
    ERA_2023 = ifelse(is.na(ERA_2023),quantile(ERA_2023, probs = 0.9, na.rm = TRUE),ERA_2023),
    pitches_thrown = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ pitches_thrown_2023*weight_2022_2sp  + pitches_thrown_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ pitches_thrown_2023*weight_2022_3sp + pitches_thrown_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ pitches_thrown_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ pitches_thrown_2023*weight_2022_6sp,
      .default = pitches_thrown_2023*weight_2022_1sp + pitches_thrown_2022*weight_2021_1sp
    ),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    ip_2023 = ifelse(is.na(ip_2023), 0, ip_2023),
    num_in_rotation_2023,
    num_in_rotation_2022,
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    fip_2023 = ifelse(is.na(fip_2023),quantile(fip_2023, probs = 0.9, na.rm = TRUE),fip_2023),
    so = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ so_2023*weight_2022_2sp  + so_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ so_2023*weight_2022_3sp + so_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ so_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ so_2023*weight_2022_6sp,
      .default = so_2023*weight_2022_1sp + so_2022*weight_2021_1sp
    ),
    kpct_2023 = so_2023/playing_time_2023,
    kpct_2022 = so_2022/playing_time_2022,
    HRA = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ HRA_2023*weight_2022_2sp  + HRA_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ HRA_2023*weight_2022_3sp + HRA_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ HRA_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ HRA_2023*weight_2022_6sp,
      .default = HRA_2023*weight_2022_1sp + HRA_2022*weight_2021_1sp
    ),
    walks_hbp = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ walks_hbp_2023*weight_2022_2sp  + walks_hbp_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ walks_hbp_2023*weight_2022_3sp + walks_hbp_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ walks_hbp_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ walks_hbp_2023*weight_2022_6sp,
      .default = walks_hbp_2023*weight_2022_1sp + walks_hbp_2022*weight_2021_1sp
    ),
    total_basesa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ total_basesa_2023*weight_2022_2sp  + total_basesa_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ total_basesa_2023*weight_2022_3sp + total_basesa_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ total_basesa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ total_basesa_2023*weight_2022_6sp,
      .default = total_basesa_2023*weight_2022_1sp + total_basesa_2022*weight_2021_1sp
    ),
    barrelsa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ barrelsa_2023*weight_2022_2sp  + barrelsa_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ barrelsa_2023*weight_2022_3sp + barrelsa_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ barrelsa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ barrelsa_2023*weight_2022_6sp,
      .default = barrelsa_2023*weight_2022_1sp + barrelsa_2022*weight_2021_1sp
    ),
    hardhitsa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ hardhitsa_2023*weight_2022_2sp  + hardhitsa_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hardhitsa_2023*weight_2022_3sp + hardhitsa_2022*weight_2021_3sp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ hardhitsa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2023*weight_2022_4sp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2023*weight_2022_5sp,
      years_since_debut_24 == 1  & debut_month > 8  ~ hardhitsa_2023*weight_2022_6sp,
      .default = hardhitsa_2023*weight_2022_1sp + hardhitsa_2022*weight_2021_1sp
    ),
    
    hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
    hardhit_pcta_2023 = ifelse(is.na(hardhit_pcta_2023),quantile(hardhit_pcta_2023, probs = 0.9, na.rm = TRUE),hardhit_pcta_2023),
    
    barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE), barrel_pcta_2022),
    barrel_pcta_2023 = ifelse(is.na(barrel_pcta_2023),quantile(barrel_pcta_2023, probs = 0.9, na.rm = TRUE),barrel_pcta_2023),

    ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
    ChaseRateA_2023 = ifelse(is.na(ChaseRateA_2023),quantile(ChaseRateA_2023, probs = 0.1, na.rm = TRUE),ChaseRateA_2023),
    
    WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
    WhiffRateA_2023 = ifelse(is.na(WhiffRateA_2023),quantile(WhiffRateA_2023, probs = 0.1, na.rm = TRUE),WhiffRateA_2023),
    
    SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
    SLGA_2023 = ifelse(is.na(SLGA_2023),quantile(SLGA_2023, probs = 0.9, na.rm = TRUE),SLGA_2023),
    
    BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
    BAA_2023 = ifelse(is.na(BAA_2023),quantile(BAA_2023, probs = 0.9, na.rm = TRUE),BAA_2023),
    
    OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
    OBPA_2023 = ifelse(is.na(OBPA_2023),quantile(OBPA_2023, probs = 0.9, na.rm = TRUE),OBPA_2023),
    
    hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022), 
    hr_pcta_2023 = ifelse(is.na(hr_pcta_2023),quantile(hr_pcta_2023, probs = 0.9, na.rm = TRUE),hr_pcta_2023),
    
    gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
    gball_rate_2023 = ifelse(is.na(gball_rate_2023), quantile(gball_rate_2023, probs = 0.9, na.rm = TRUE), gball_rate_2023),
    
    cluster_2024_sp = as.factor(cluster_2024_sp),
    cluster_2024_sp_prop = as.factor(cluster_2024_sp_prop),
    
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    avg_ev_2023 = ifelse(is.na(avg_ev_2023), quantile(avg_ev_2023, probs = 0.1, na.rm = TRUE), avg_ev_2023),
    
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    percentile_90_ev_2023 = ifelse(is.na(percentile_90_ev_2023), quantile(percentile_90_ev_2023, probs = 0.1, na.rm = TRUE), percentile_90_ev_2023),
    
    percentile_90_velo = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ percentile_90_velo_2023*weight_2022_2sp  + percentile_90_velo_2022*weight_2021_2sp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ percentile_90_velo_2023*weight_2022_3sp + percentile_90_velo_2022*weight_2021_3sp,
      playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ percentile_90_velo_2023,
      .default = percentile_90_velo_2023*weight_2022_1sp + percentile_90_velo_2022*weight_2021_1sp)
  ) %>% 
  mutate(rookie_year_2023 = ifelse(years_since_debut_24 == 1, 1, 0), #renaming for 2024 predictions
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0),#renaming for 2024 predictions
         rookie_year_2022 = ifelse(years_since_debut_24 == 2, 1, 0),#renaming for 2024 predictions
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),#renaming for 2024 predictions
         kpct_2023 = ifelse(is.na(kpct_2023), quantile(kpct_2023, probs = 0.1, na.rm =TRUE), kpct_2023),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ kpct_2023*weight_2022_2sp  + kpct_2022*weight_2021_2sp,
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ kpct_2023*weight_2022_3sp + kpct_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ kpct_2023,
           .default = kpct_2023*weight_2022_1sp + kpct_2022*weight_2021_1sp),
         earned_runs = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ earned_runs_2023*weight_2022_2sp  + earned_runs_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ earned_runs_2023*weight_2022_3sp + earned_runs_2022*weight_2021_3sp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ earned_runs_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2023*weight_2022_4sp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2023*weight_2022_5sp,
           years_since_debut_24 == 1  & debut_month > 8  ~ earned_runs_2023*weight_2022_6sp,
           .default = earned_runs_2023*weight_2022_1sp + earned_runs_2022*weight_2021_1sp
         ),
         RV = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ RV_2023*weight_2022_2sp  + RV_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ RV_2023*weight_2022_3sp + RV_2022*weight_2021_3sp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ RV_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2023*weight_2022_4sp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2023*weight_2022_5sp,
           years_since_debut_24 == 1  & debut_month > 8  ~ RV_2023*weight_2022_6sp,
           .default = RV_2023*weight_2022_1sp + RV_2022*weight_2021_1sp
         ),
         RV100 = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ RV100_2023*weight_2022_2sp  + RV100_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ RV100_2023*weight_2022_3sp + RV100_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ RV100_2023,
           .default = RV100_2023*weight_2022_1sp + RV100_2022*weight_2021_1sp),
         ip = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ip_2023*weight_2022_2sp  + ip_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ip_2023*weight_2022_3sp + ip_2022*weight_2021_3sp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ ip_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2023*weight_2022_4sp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2023*weight_2022_5sp,
           years_since_debut_24 == 1  & debut_month > 8  ~ ip_2023*weight_2022_6sp,
           .default = ip_2023*weight_2022_1sp + ip_2022*weight_2021_1sp
         ),
         wOBAA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ wOBAA_2023*weight_2022_2sp  + wOBAA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ wOBAA_2023*weight_2022_3sp + wOBAA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ wOBAA_2023,
           .default = wOBAA_2023*weight_2022_1sp + wOBAA_2022*weight_2021_1sp),
         
         times_faced = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ times_faced_2023*weight_2022_2sp  + times_faced_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ times_faced_2023*weight_2022_3sp + times_faced_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ times_faced_2023,
           .default = times_faced_2023*weight_2022_1sp + times_faced_2022*weight_2021_1sp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_pitches_per_appearance_2023*weight_2022_2sp  + avg_pitches_per_appearance_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_pitches_per_appearance_2023*weight_2022_3sp + avg_pitches_per_appearance_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ avg_pitches_per_appearance_2023,
           .default = avg_pitches_per_appearance_2023*weight_2022_1sp + avg_pitches_per_appearance_2022*weight_2021_1sp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_bf_per_appearance_2023*weight_2022_2sp  + avg_bf_per_appearance_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_bf_per_appearance_2023*weight_2022_3sp + avg_bf_per_appearance_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ avg_bf_per_appearance_2023,
           .default = avg_bf_per_appearance_2023*weight_2022_1sp + avg_bf_per_appearance_2022*weight_2021_1sp),
         
         ERA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ERA_2023*weight_2022_2sp  + ERA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ERA_2023*weight_2022_3sp + ERA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ ERA_2023,
           .default = ERA_2023*weight_2022_1sp + ERA_2022*weight_2021_1sp),
         
         num_in_rotation = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ num_in_rotation_2023*weight_2022_2sp  + num_in_rotation_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ num_in_rotation_2023*weight_2022_3sp + num_in_rotation_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ num_in_rotation_2023,
           .default = num_in_rotation_2023*weight_2022_1sp + num_in_rotation_2022*weight_2021_1sp),
         
         fip = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ fip_2023*weight_2022_2sp  + fip_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ fip_2023*weight_2022_3sp + fip_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ fip_2023,
           .default = fip_2023*weight_2022_1sp + fip_2022*weight_2021_1sp),
         
         hardhit_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ hardhit_pcta_2023*weight_2022_2sp  + hardhit_pcta_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hardhit_pcta_2023*weight_2022_3sp + hardhit_pcta_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ hardhit_pcta_2023,
           .default = hardhit_pcta_2023*weight_2022_1sp + hardhit_pcta_2022*weight_2021_1sp),
         
         barrel_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ barrel_pcta_2023*weight_2022_2sp  + barrel_pcta_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ barrel_pcta_2023*weight_2022_3sp + barrel_pcta_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ barrel_pcta_2023,
           .default = barrel_pcta_2023*weight_2022_1sp + barrel_pcta_2022*weight_2021_1sp),
         
         ChaseRateA  = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ChaseRateA_2023*weight_2022_2sp  + ChaseRateA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ChaseRateA_2023*weight_2022_3sp + ChaseRateA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ ChaseRateA_2023,
           .default = ChaseRateA_2023*weight_2022_1sp + ChaseRateA_2022*weight_2021_1sp),
         
         WhiffRateA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ WhiffRateA_2023*weight_2022_2sp  + WhiffRateA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ WhiffRateA_2023*weight_2022_3sp + WhiffRateA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ WhiffRateA_2023,
           .default = WhiffRateA_2023*weight_2022_1sp + WhiffRateA_2022*weight_2021_1sp),
         
         SLGA =  case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ SLGA_2023*weight_2022_2sp  + SLGA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ SLGA_2023*weight_2022_3sp + SLGA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ SLGA_2023,
           .default = SLGA_2023*weight_2022_1sp + SLGA_2022*weight_2021_1sp),
         
         BAA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ BAA_2023*weight_2022_2sp  + BAA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ BAA_2023*weight_2022_3sp + BAA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ BAA_2023,
           .default = BAA_2023*weight_2022_1sp + BAA_2022*weight_2021_1sp),
         
         OBPA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ OBPA_2023*weight_2022_2sp  + OBPA_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ OBPA_2023*weight_2022_3sp + OBPA_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ OBPA_2023,
           .default = OBPA_2023*weight_2022_1sp + OBPA_2022*weight_2021_1sp),
         
         hr_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ hr_pcta_2023*weight_2022_2sp  + hr_pcta_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hr_pcta_2023*weight_2022_3sp + hr_pcta_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ hr_pcta_2023,
           .default = hr_pcta_2023*weight_2022_1sp + hr_pcta_2022*weight_2021_1sp),
         
         gball_rate = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ gball_rate_2023*weight_2022_2sp  + gball_rate_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ gball_rate_2023*weight_2022_3sp + gball_rate_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ gball_rate_2023,
           .default = gball_rate_2023*weight_2022_1sp + gball_rate_2022*weight_2021_1sp),
         
         avg_ev = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_ev_2023*weight_2022_2sp  + avg_ev_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_ev_2023*weight_2022_3sp + avg_ev_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ avg_ev_2023,
           .default = avg_ev_2023*weight_2022_1sp + avg_ev_2022*weight_2021_1sp),
         
         percentile_90_ev = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ percentile_90_ev_2023*weight_2022_2sp  + percentile_90_ev_2022*weight_2021_2sp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ percentile_90_ev_2023*weight_2022_3sp + percentile_90_ev_2022*weight_2021_3sp,
           playing_time_2022 == 0 & playing_time_2022 > 0 & years_since_debut_24 == 1 ~ percentile_90_ev_2023,
           .default = percentile_90_ev_2023*weight_2022_1sp + percentile_90_ev_2022*weight_2021_1sp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))


batters_faced_xg_2024 <- batters_faced_xg_2024 %>% 
  dummy_cols(select_columns = c('cluster_2024_sp', 'cluster_2024_sp_prop'), remove_selected_columns = TRUE)

batters_faced_xg_2024$PLAYING_TIME <- predict(sp_mod, 
                                              as.matrix(batters_faced_xg_2024 %>% select(-PLAYER_ID)))

#### RP Predictions ####
batters_faced_xg_rp_2024 <- batters_faced %>% 
  filter(pitcher %in% unique(submission$PLAYER_ID),role_key_2024 == 'RP') %>% 
  reframe(
    PLAYER_ID = pitcher,
    playing_time_2023,
    playing_time_2022,
    RV_2023 = ifelse(is.na(RV_2023), 0, RV_2023),
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV100_2023 = ifelse(is.na(RV100_2023), quantile(RV100_2023, probs = 0.1, na.rm = TRUE), RV100_2023),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    debut_month, 
    age_24, #renaming for predictions
    years_since_debut_24,
    bf_second_half = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ bf_second_half_2023*weight_2022_2rp  + bf_second_half_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ bf_second_half_2023*weight_2022_3rp + bf_second_half_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ bf_second_half_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2023*weight_2022_5rp,
      years_since_debut_24 == 1  & debut_month > 8  ~ bf_second_half_2023*weight_2022_6rp,
      .default = bf_second_half_2023*weight_2022_1rp + bf_second_half_2022*weight_2021_1rp
    ),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    wOBAA_2023 = ifelse(is.na(wOBAA_2023), quantile(wOBAA_2023, probs = 0.9, na.rm = TRUE), wOBAA_2023),
    bf_diff = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ bf_diff_2023*weight_2022_2rp  + bf_diff_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ bf_diff_2023*weight_2022_3rp + bf_diff_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ bf_diff_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ bf_diff_2023*weight_2022_6rp,
      .default = bf_diff_2023*weight_2022_1rp + bf_diff_2022*weight_2021_1rp
    ),
    times_faced_2022,
    times_faced_2023,
    avg_pitches_per_appearance_2022,
    avg_pitches_per_appearance_2023,
    avg_bf_per_appearance_2022,
    avg_bf_per_appearance_2023,
    earned_runs_2023 = ifelse(is.na(earned_runs_2023), 0,earned_runs_2023),
    earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
    ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
    ERA_2023 = ifelse(is.na(ERA_2023),quantile(ERA_2023, probs = 0.9, na.rm = TRUE),ERA_2023),
    pitches_thrown = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ pitches_thrown_2023*weight_2022_2rp  + pitches_thrown_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ pitches_thrown_2023*weight_2022_3rp + pitches_thrown_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ pitches_thrown_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ pitches_thrown_2023*weight_2022_6rp,
      .default = pitches_thrown_2023*weight_2022_1rp + pitches_thrown_2022*weight_2021_1rp
    ),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    ip_2023 = ifelse(is.na(ip_2023), 0, ip_2023),
    
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    fip_2023 = ifelse(is.na(fip_2023),quantile(fip_2023, probs = 0.9, na.rm = TRUE),fip_2023),
    so = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ so_2023*weight_2022_2rp  + so_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ so_2023*weight_2022_3rp + so_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ so_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ so_2023*weight_2022_6rp,
      .default = so_2023*weight_2022_1rp + so_2022*weight_2021_1rp
    ),
    kpct_2023 = so_2023/playing_time_2023,
    kpct_2022 = so_2022/playing_time_2022,
    HRA = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ HRA_2023*weight_2022_2rp  + HRA_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ HRA_2023*weight_2022_3rp + HRA_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ HRA_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ HRA_2023*weight_2022_6rp,
      .default = HRA_2023*weight_2022_1rp + HRA_2022*weight_2021_1rp
    ),
    walks_hbp = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ walks_hbp_2023*weight_2022_2rp  + walks_hbp_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ walks_hbp_2023*weight_2022_3rp + walks_hbp_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ walks_hbp_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ walks_hbp_2023*weight_2022_6rp,
      .default = walks_hbp_2023*weight_2022_1rp + walks_hbp_2022*weight_2021_1rp
    ),
    total_basesa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ total_basesa_2023*weight_2022_2rp  + total_basesa_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ total_basesa_2023*weight_2022_3rp + total_basesa_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ total_basesa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ total_basesa_2023*weight_2022_6rp,
      .default = total_basesa_2023*weight_2022_1rp + total_basesa_2022*weight_2021_1rp
    ),
    barrelsa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ barrelsa_2023*weight_2022_2rp  + barrelsa_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ barrelsa_2023*weight_2022_3rp + barrelsa_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ barrelsa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ barrelsa_2023*weight_2022_6rp,
      .default = barrelsa_2023*weight_2022_1rp + barrelsa_2022*weight_2021_1rp
    ),
    hardhitsa = case_when(
      playing_time_2022 > 150 & playing_time_2023 < 50 ~ hardhitsa_2023*weight_2022_2rp  + hardhitsa_2022*weight_2021_2rp, 
      playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hardhitsa_2023*weight_2022_3rp + hardhitsa_2022*weight_2021_3rp,
      years_since_debut_24 == 1  & debut_month <= 4 ~ hardhitsa_2023,
      years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2023*weight_2022_4rp,
      years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2023*weight_2022_5rp,
      years_since_debut_24 == 1  &  debut_month > 8  ~ hardhitsa_2023*weight_2022_6rp,
      .default = hardhitsa_2023*weight_2022_1rp + hardhitsa_2022*weight_2021_1rp
    ),
    
    hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
    hardhit_pcta_2023 = ifelse(is.na(hardhit_pcta_2023),quantile(hardhit_pcta_2023, probs = 0.9, na.rm = TRUE),hardhit_pcta_2023),
    
    barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE), barrel_pcta_2022),
    barrel_pcta_2023 = ifelse(is.na(barrel_pcta_2023),quantile(barrel_pcta_2023, probs = 0.9, na.rm = TRUE),barrel_pcta_2023),
    
    ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
    ChaseRateA_2023 = ifelse(is.na(ChaseRateA_2023),quantile(ChaseRateA_2023, probs = 0.1, na.rm = TRUE),ChaseRateA_2023),
    
    WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
    WhiffRateA_2023 = ifelse(is.na(WhiffRateA_2023),quantile(WhiffRateA_2023, probs = 0.1, na.rm = TRUE),WhiffRateA_2023),
    
    SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
    SLGA_2023 = ifelse(is.na(SLGA_2023),quantile(SLGA_2023, probs = 0.9, na.rm = TRUE),SLGA_2023),
    
    BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
    BAA_2023 = ifelse(is.na(BAA_2023),quantile(BAA_2023, probs = 0.9, na.rm = TRUE),BAA_2023),
    
    OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
    OBPA_2023 = ifelse(is.na(OBPA_2023),quantile(OBPA_2023, probs = 0.9, na.rm = TRUE),OBPA_2023),
    
    hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022), 
    hr_pcta_2023 = ifelse(is.na(hr_pcta_2023),quantile(hr_pcta_2023, probs = 0.9, na.rm = TRUE),hr_pcta_2023),
    
    gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
    gball_rate_2023 = ifelse(is.na(gball_rate_2023), quantile(gball_rate_2023, probs = 0.9, na.rm = TRUE), gball_rate_2023),
    
    cluster_2024_rp = as.factor(cluster_2024_rp),
    
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    avg_ev_2023 = ifelse(is.na(avg_ev_2023), quantile(avg_ev_2023, probs = 0.1, na.rm = TRUE), avg_ev_2023),
    
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    percentile_90_ev_2023 = ifelse(is.na(percentile_90_ev_2023), quantile(percentile_90_ev_2023, probs = 0.1, na.rm = TRUE), percentile_90_ev_2023),
  ) %>% 
  mutate(rookie_year_2023 = ifelse(years_since_debut_24 == 1, 1, 0),
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0),
         rookie_year_2022 = ifelse(years_since_debut_24 == 2, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         kpct_2023 = ifelse(is.na(kpct_2023), quantile(kpct_2023, probs = 0.1, na.rm =TRUE), kpct_2023),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ kpct_2023*weight_2022_2rp  + kpct_2022*weight_2021_2rp,
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ kpct_2023*weight_2022_3rp + kpct_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ kpct_2023,
           .default = kpct_2023*weight_2022_1rp + kpct_2022*weight_2021_1rp),
         earned_runs = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ earned_runs_2023*weight_2022_2rp  + earned_runs_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ earned_runs_2023*weight_2022_3rp + earned_runs_2022*weight_2021_3rp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ earned_runs_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2023*weight_2022_4rp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2023*weight_2022_5rp,
           years_since_debut_24 == 1  &  debut_month > 8  ~ earned_runs_2023*weight_2022_6rp,
           .default = earned_runs_2023*weight_2022_1rp + earned_runs_2022*weight_2021_1rp
         ),
         RV = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ RV_2023*weight_2022_2rp  + RV_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ RV_2023*weight_2022_3rp + RV_2022*weight_2021_3rp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ RV_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2023*weight_2022_4rp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2023*weight_2022_5rp,
           years_since_debut_24 == 1  &  debut_month > 8  ~ RV_2023*weight_2022_6rp,
           .default = RV_2023*weight_2022_1rp + RV_2022*weight_2021_1rp
         ),
         RV100 = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ RV100_2023*weight_2022_2rp  + RV100_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ RV100_2023*weight_2022_3rp + RV100_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ RV100_2023,
           .default = RV100_2023*weight_2022_1rp + RV100_2022*weight_2021_1rp),
         ip = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ip_2023*weight_2022_2rp  + ip_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ip_2023*weight_2022_3rp + ip_2022*weight_2021_3rp,
           years_since_debut_24 == 1  & debut_month <= 4 ~ ip_2023,
           years_since_debut_24 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2023*weight_2022_4rp,
           years_since_debut_24 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2023*weight_2022_5rp,
           years_since_debut_24 == 1  &  debut_month > 8  ~ ip_2023*weight_2022_6rp,
           .default = ip_2023*weight_2022_1rp + ip_2022*weight_2021_1rp
         ),
         wOBAA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ wOBAA_2023*weight_2022_2rp  + wOBAA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ wOBAA_2023*weight_2022_3rp + wOBAA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ wOBAA_2023,
           .default = wOBAA_2023*weight_2022_1rp + wOBAA_2022*weight_2021_1rp),
         
         times_faced = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ times_faced_2023*weight_2022_2rp  + times_faced_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ times_faced_2023*weight_2022_3rp + times_faced_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ times_faced_2023,
           .default = times_faced_2023*weight_2022_1rp + times_faced_2022*weight_2021_1rp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_pitches_per_appearance_2023*weight_2022_2rp  + avg_pitches_per_appearance_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_pitches_per_appearance_2023*weight_2022_3rp + avg_pitches_per_appearance_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ avg_pitches_per_appearance_2023,
           .default = avg_pitches_per_appearance_2023*weight_2022_1rp + avg_pitches_per_appearance_2022*weight_2021_1rp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_bf_per_appearance_2023*weight_2022_2rp  + avg_bf_per_appearance_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_bf_per_appearance_2023*weight_2022_3rp + avg_bf_per_appearance_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ avg_bf_per_appearance_2023,
           .default = avg_bf_per_appearance_2023*weight_2022_1rp + avg_bf_per_appearance_2022*weight_2021_1rp),
         
         ERA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ERA_2023*weight_2022_2rp  + ERA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ERA_2023*weight_2022_3rp + ERA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ ERA_2023,
           .default = ERA_2023*weight_2022_1rp + ERA_2022*weight_2021_1rp),
         
         fip = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ fip_2023*weight_2022_2rp  + fip_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ fip_2023*weight_2022_3rp + fip_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ fip_2023,
           .default = fip_2023*weight_2022_1rp + fip_2022*weight_2021_1rp),
         
         hardhit_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ hardhit_pcta_2023*weight_2022_2rp  + hardhit_pcta_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hardhit_pcta_2023*weight_2022_3rp + hardhit_pcta_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ hardhit_pcta_2023,
           .default = hardhit_pcta_2023*weight_2022_1rp + hardhit_pcta_2022*weight_2021_1rp),
         
         barrel_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ barrel_pcta_2023*weight_2022_2rp  + barrel_pcta_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ barrel_pcta_2023*weight_2022_3rp + barrel_pcta_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ barrel_pcta_2023,
           .default = barrel_pcta_2023*weight_2022_1rp + barrel_pcta_2022*weight_2021_1rp),
         
         ChaseRateA  = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ ChaseRateA_2023*weight_2022_2rp  + ChaseRateA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ ChaseRateA_2023*weight_2022_3rp + ChaseRateA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ ChaseRateA_2023,
           .default = ChaseRateA_2023*weight_2022_1rp + ChaseRateA_2022*weight_2021_1rp),
         
         WhiffRateA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ WhiffRateA_2023*weight_2022_2rp  + WhiffRateA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ WhiffRateA_2023*weight_2022_3rp + WhiffRateA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ WhiffRateA_2023,
           .default = WhiffRateA_2023*weight_2022_1rp + WhiffRateA_2022*weight_2021_1rp),
         
         SLGA =  case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ SLGA_2023*weight_2022_2rp  + SLGA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ SLGA_2023*weight_2022_3rp + SLGA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ SLGA_2023,
           .default = SLGA_2023*weight_2022_1rp + SLGA_2022*weight_2021_1rp),
         
         BAA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ BAA_2023*weight_2022_2rp  + BAA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ BAA_2023*weight_2022_3rp + BAA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ BAA_2023,
           .default = BAA_2023*weight_2022_1rp + BAA_2022*weight_2021_1rp),
         
         OBPA = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ OBPA_2023*weight_2022_2rp  + OBPA_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ OBPA_2023*weight_2022_3rp + OBPA_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ OBPA_2023,
           .default = OBPA_2023*weight_2022_1rp + OBPA_2022*weight_2021_1rp),
         
         hr_pcta = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ hr_pcta_2023*weight_2022_2rp  + hr_pcta_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ hr_pcta_2023*weight_2022_3rp + hr_pcta_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ hr_pcta_2023,
           .default = hr_pcta_2023*weight_2022_1rp + hr_pcta_2022*weight_2021_1rp),
         
         gball_rate = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ gball_rate_2023*weight_2022_2rp  + gball_rate_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ gball_rate_2023*weight_2022_3rp + gball_rate_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ gball_rate_2023,
           .default = gball_rate_2023*weight_2022_1rp + gball_rate_2022*weight_2021_1rp),
         
         avg_ev = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ avg_ev_2023*weight_2022_2rp  + avg_ev_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ avg_ev_2023*weight_2022_3rp + avg_ev_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ avg_ev_2023,
           .default = avg_ev_2023*weight_2022_1rp + avg_ev_2022*weight_2021_1rp),
         
         percentile_90_ev = case_when(
           playing_time_2022 > 150 & playing_time_2023 < 50 ~ percentile_90_ev_2023*weight_2022_2rp  + percentile_90_ev_2022*weight_2021_2rp, 
           playing_time_2022 < 50 & playing_time_2023 > 150 & years_since_debut_24 != 1 ~ percentile_90_ev_2023*weight_2022_3rp + percentile_90_ev_2022*weight_2021_3rp,
           playing_time_2022 == 0 & playing_time_2023 > 0 & years_since_debut_24 == 1 ~ percentile_90_ev_2023,
           .default = percentile_90_ev_2023*weight_2022_1rp + percentile_90_ev_2022*weight_2021_1rp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))

batters_faced_xg_rp_2024 <- batters_faced_xg_rp_2024 %>% 
  dummy_cols(select_columns = c('cluster_2024_rp'), remove_selected_columns = TRUE)


batters_faced_xg_rp_2024$PLAYING_TIME <- predict(rp_model, 
                                                 as.matrix(batters_faced_xg_rp_2024 %>% select(-PLAYER_ID)))


#### END OF PREDICTIONS ####

glimpse(plate_app_xg_2024)
glimpse(batters_faced_xg_2024)
glimpse(batters_faced_xg_rp_2024)


submission_file <- bind_rows(plate_app_xg_2024 %>% select(PLAYER_ID, PLAYING_TIME),
                             batters_faced_xg_2024 %>% select(PLAYER_ID, PLAYING_TIME),
                             batters_faced_xg_rp_2024 %>% select(PLAYER_ID, PLAYING_TIME))

#joining at bats for ohtani
submission_file <- submission_file %>% 
  group_by(PLAYER_ID) %>% 
  reframe(PLAYING_TIME = sum(PLAYING_TIME))

# write to csv
write_csv(submission_file, 'hackathon_submission_updated.csv')



