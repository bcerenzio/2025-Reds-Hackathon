#install.packages("rsample")
library(xgboost)
library(fastDummies)
library(tidyverse)
library(rsample)

slice <- dplyr::slice

#### Tuning Model ####
find_weight1_bfsp <- function(weight_2021_1sp){
  weight_2022_1sp <- 1-weight_2021_1sp
  print(paste('Weight 2021 1 SP: ', weight_2021_1sp))

  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*0.2  + bf_second_half_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*0.8 + bf_second_half_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*4,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*0.2  + bf_diff_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*4,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*0.2  + pitches_thrown_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*0.8 + pitches_thrown_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*4,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*0.2  + so_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*0.8 + so_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*4,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*0.2  + HRA_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*4,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*0.2  + walks_hbp_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*4,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*0.2  + total_basesa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*4,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*0.2  + barrelsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*4,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*0.2  + hardhitsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*4,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*0.2  + percentile_90_velo_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*0.8 + percentile_90_velo_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*0.2  + kpct_2021*0.8,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*0.8 + kpct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*0.2  + earned_runs_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*0.8 + earned_runs_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*4,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*0.2  + RV_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*4,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*0.2  + RV100_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*0.8 + RV100_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*0.2  + ip_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*0.8 + ip_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*4,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*0.2  + wOBAA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*0.8 + wOBAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*0.2  + times_faced_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*0.8 + times_faced_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*0.2  + avg_pitches_per_appearance_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*0.8 + avg_pitches_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*0.2  + avg_bf_per_appearance_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*0.8 + avg_bf_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*0.2  + ERA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*0.8 + ERA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*0.2  + num_in_rotation_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*0.8 + num_in_rotation_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*0.2  + fip_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*0.8 + fip_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*0.2  + hardhit_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*0.8 + hardhit_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*0.2  + barrel_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*0.8 + barrel_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*0.2  + ChaseRateA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*0.8 + ChaseRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*0.2  + WhiffRateA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*0.8 + WhiffRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*0.2  + SLGA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*0.8 + SLGA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*0.2  + BAA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*0.8 + BAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*0.2  + OBPA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*0.8 + OBPA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*0.2  + hr_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*0.8 + hr_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*0.2  + gball_rate_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*0.8 + gball_rate_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*0.2  + avg_ev_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*0.2  + percentile_90_ev_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting1_function_sp_df <- tibble(
  weight_2021_1sp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight1_bfsp)
)

weighting1_function_sp_df %>% ggplot(aes(weight_2021_1sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +# rmse = 229.1 
  xlab('Weight 2021 1 (SP)') +
   ylab('RMSE')#+
  # scale_y_continuous(breaks= seq(218, 228, by = 2),
  #                    limits = c(218, 228))

ggsave('SP Weight 1 2021.png', width = 4, height = 4.76)

weight_2021_1sp <- weighting1_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_1sp) #0.2
weight_2022_1sp <- 1 - weight_2021_1sp #0.8



find_weight2_bfsp <- function(weight_2021_2sp){
  weight_2022_2sp <- 1-weight_2021_2sp
  print(paste('Weight 2021 2 SP: ', weight_2021_2sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*0.8 + bf_second_half_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*4,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*4,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*0.8 + pitches_thrown_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*4,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*0.8 + so_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*4,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*4,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*4,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*4,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*4,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*4,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*0.8 + percentile_90_velo_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
      ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*0.8 + kpct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*0.8 + earned_runs_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*4,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*4,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*0.8 + RV100_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*0.8 + ip_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*4,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*0.8 + wOBAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*0.8 + times_faced_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*0.8 + avg_pitches_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*0.8 + avg_bf_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*0.8 + ERA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*0.8 + num_in_rotation_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*0.8 + fip_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*0.8 + hardhit_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*0.8 + barrel_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*0.8 + ChaseRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*0.8 + WhiffRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*0.8 + SLGA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*0.8 + BAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*0.8 + OBPA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*0.8 + hr_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*0.8 + gball_rate_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting2_function_sp_df <- tibble(
  weight_2021_2sp = seq(0.5, 1, by = 0.05),
  rmse = map_dbl(seq(0.5,1,by = 0.05), find_weight2_bfsp)
)

weighting2_function_sp_df %>% ggplot(aes(weight_2021_2sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw()+ 
  xlab('Weight 2021 2 (SP)') +
  ylab('RMSE') #+
  scale_y_continuous(breaks= seq(218, 228, by = 2),
                     limits = c(218, 228))

ggsave('SP Weight 2 2021.png', width = 4, height = 4.76)

weight_2021_2sp <- weighting2_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_2sp) #0.8
weight_2022_2sp <- 1 - weight_2021_2sp #0.2


find_weight3_bfsp <- function(weight_2021_3sp){
  weight_2022_3sp <- 1-weight_2021_3sp
  print(paste('Weight 2021 3 SP: ', weight_2021_3sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*4,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*4,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*4,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*4,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*4,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*4,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*4,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*4,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*4,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*4,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*4,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*4,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting3_function_sp_df <- tibble(
  weight_2021_3sp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight3_bfsp)
)

weighting3_function_sp_df %>% ggplot(aes(weight_2021_3sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw()+
  xlab('Weight 2021 3 (SP)') +
  ylab('RMSE')#+
  scale_y_continuous(breaks= seq(218, 228, by = 2),
                     limits = c(218, 228))

ggsave('SP Weight 3 2021.png', width = 4, height = 4.76)

weight_2021_3sp <- weighting3_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_3sp) #0.15 
weight_2022_3sp <- 1 - weight_2021_3sp #0.85

find_weight4_bfsp <- function(weight_2022_4sp){
  print(paste('Weight 2022 4 SP: ', weight_2022_4sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*4,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*4,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*4,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*4,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*4,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*4,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*4,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*4,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*4,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*4,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*4,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*4,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting4_function_sp_df <- tibble(
  weight_2022_4sp = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), find_weight4_bfsp)
)

weighting4_function_sp_df %>% ggplot(aes(weight_2022_4sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  xlab('Weight 2022 4 (SP)') +
  ylab('RMSE')#+
  scale_y_continuous(breaks= seq(218, 228, by = 2),
                     limits = c(218, 228))

ggsave('SP Weight 4 2022.png', width = 4, height = 4.76)

weight_2022_4sp <- weighting4_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_4sp) # 1.9


find_weight5_bfsp <- function(weight_2022_5sp){
  print(paste('Weight 2022 5 SP: ', weight_2022_5sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*4,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*4,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*4,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*4,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*4,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*4,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*4,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*4,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*4,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*4,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*4,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*4,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting5_function_sp_df <- tibble(
  weight_2022_5sp = seq(2, 4, by = 0.1),
  rmse = map_dbl(seq(2,4,by = 0.1), find_weight5_bfsp)
)

weighting5_function_sp_df %>% ggplot(aes(weight_2022_5sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  xlab('Weight 2022 5 (SP)') +
  ylab('RMSE')#+
  scale_y_continuous(breaks= seq(218, 228, by = 2),
                     limits = c(218, 228))

ggsave('SP Weight 5 2022.png', width = 4, height = 4.76)

weight_2022_5sp <- weighting5_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_5sp) # 3.9

find_weight6_bfsp <- function(weight_2022_6sp){
  print(paste('Weight 2022 6 SP: ', weight_2022_6sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'SP') %>% 
    reframe(
      playing_time_2023,
      playing_time_2022,
      playing_time_2021,
      RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
      RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
      RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
      RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
      debut_month, 
      age_23,
      years_since_debut_23,
      bf_second_half = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
        .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
      ),
      times_faced_2021,
      times_faced_2022,
      avg_pitches_per_appearance_2021,
      avg_pitches_per_appearance_2022,
      avg_bf_per_appearance_2021,
      avg_bf_per_appearance_2022,
      earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
      earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
      ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
      ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
      pitches_thrown = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
        .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      num_in_rotation_2022,
      num_in_rotation_2021,
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*weight_2022_6sp,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
        .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
      ),
      
      hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
      hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
      
      barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
      barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
      
      ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
      ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
      
      WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
      WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
      
      SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
      SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
      
      BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
      BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
      
      OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
      OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
      
      hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
      hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
      
      gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
      gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
      
      cluster_2023_sp = as.factor(cluster_2023_sp),
      cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*weight_2022_6sp,
             .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*weight_2022_6sp,
             .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
           
           num_in_rotation = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
             .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  
  batters_faced_xg <- batters_faced_xg %>% 
    dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)
  
  dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting6_function_sp_df <- tibble(
  weight_2022_6sp = seq(3, 5, by = 0.1),
  rmse = map_dbl(seq(3,5,by = 0.1), find_weight6_bfsp)
)

weighting6_function_sp_df %>% ggplot(aes(weight_2022_6sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  xlab('Weight 2022 6 (SP)') +
  ylab('RMSE')#+
  scale_y_continuous(breaks= seq(218, 228, by = 2),
                     limits = c(218, 228))

ggsave('SP Weight 6 2022.png', width = 4, height = 4.76)

weight_2022_6sp <- weighting6_function_sp_df %>% slice_min(rmse, n = 1) %>% slice(1) %>% pull(weight_2022_6sp) # 4.6

batters_faced_xg <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'SP') %>% 
  reframe(
    playing_time_2023,
    playing_time_2022,
    playing_time_2021,
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
    debut_month, 
    age_23,
    years_since_debut_23,
    bf_second_half = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
      .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
      .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
    ),
    times_faced_2021,
    times_faced_2022,
    avg_pitches_per_appearance_2021,
    avg_pitches_per_appearance_2022,
    avg_bf_per_appearance_2021,
    avg_bf_per_appearance_2022,
    earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
    earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
    ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
    ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
    pitches_thrown = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
      .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    num_in_rotation_2022,
    num_in_rotation_2021,
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*weight_2022_6sp,
      .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
      .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
      .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
      .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
      .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
      .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
    ),
    
    hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
    hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
    
    barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
    barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
    
    ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
    ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
    
    WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
    WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
    
    SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
    SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
    
    BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
    BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
    
    OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
    OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
    
    hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
    hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
    
    gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
    gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
    
    cluster_2023_sp = as.factor(cluster_2023_sp),
    cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    
    percentile_90_velo = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
      playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
      .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
  ) %>% 
  mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
         debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
           .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*weight_2022_6sp,
           .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*weight_2022_6sp,
           .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
         
         num_in_rotation = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
           .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))


batters_faced_xg <- batters_faced_xg %>% 
  dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)

dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2023)), label = batters_faced_xg$playing_time_2023)


hyperparam_sp_tuning_reg <- function(max_depth_sp, weight_sp, lambda_sp,
                                     gam_sp,subsample_sp, row_num_sp){

  print(paste('Max Depth: ', max_depth_sp))
  print(paste('Weight: ', weight_sp))
  print(paste('Lambda: ', lambda_sp))
  print(paste('Gamma: ', gam_sp))
  print(paste('Subsample: ', subsample_sp))
  print(paste('Row Number: ', row_num_sp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 12,
      alpha = 0,
      max_depth = 2,
      min_child_weight = 4,
      subsample = 0.6
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_sp_df <- expand_grid(
  max_depth = c(2,3,4),
  weight = c(1, 4, 10,16),
  lambda = c(1, 12, 20),
  gam = c(1,2),
  subsample = c(0.5,0.6,0.7)
) %>% 
  mutate(row_num = row_number())

reg_tuning_sp_df <- reg_tuning_sp_df %>% 
  rowwise() %>% 
  mutate(
    rmse = pmap_dbl(list(max_depth, weight, lambda, gam, subsample,row_num), hyperparam_sp_tuning_reg)
  ) %>% 
  ungroup()

reg_tuning_sp_df %>% 
  arrange(rmse) %>% 
  head(5)

#RMSE 

reg_tuning_sp_best <- reg_tuning_sp_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

# # A tibble: 1 × 7
# max_depth weight lambda   gam      subsample row_num  rmse
# <dbl>     <dbl>  <dbl>    <dbl>     <dbl>   <int>     <dbl>
#  2          1      1       1         0.5       1      219.


gam_sp <- reg_tuning_sp_best$gam #1
lambda_sp <- reg_tuning_sp_best$lambda #20
max_depth_sp <- reg_tuning_sp_best$max_depth #2
weight_sp <- reg_tuning_sp_best$weight #16
subsample_sp <- reg_tuning_sp_best$subsample #0.6

hyperparam_sp_tuning_col <- function(by_tree_sp, by_level_sp, 
                                     by_node_sp, row_num_sp){
  
  print(paste('By Tree: ', by_tree_sp))
  print(paste('By Level: ', by_level_sp))
  print(paste('By Node: ', by_node_sp))
  print(paste('Row Number: ', row_num_sp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam_sp,
      lambda = lambda_sp,
      alpha = 0,
      max_depth = max_depth_sp,
      min_child_weight = weight_sp,
      subsample = subsample_sp,
      colsample_bytree = by_tree_sp,
      colsample_bylevel = by_level_sp,
      colsample_bynode = by_node_sp
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

sp_tuning_col <- expand_grid(
  by_tree = c(0.33,0.67,1),
  by_level = c(0.33,0.67,1),
  by_node = c(0.33, 0.67, 1)
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(by_tree, by_level, by_node, row_num),hyperparam_sp_tuning_col)) %>% 
  ungroup()

sp_tuning_col %>% 
  arrange(rmse) %>% 
  head(5)

sp_tuning_col_best <- sp_tuning_col %>% 
  slice_min(rmse, n = 1) 

#by_tree by_level by_node row_num  rmse
#<dbl>    <dbl>   <dbl>   <int>   <dbl>
# 1        1       1      27      223.

by_tree_sp <- sp_tuning_col_best$by_tree # 1
by_level_sp <- sp_tuning_col_best$by_level # 1
by_node_sp <- sp_tuning_col_best$by_node # 1


hyperparam_sp_tuning_methods <- function(tree_method_sp, grow_policy_sp, row_num_sp){
  
  print(paste('Tree Method: ', tree_method_sp))
  print(paste('Grow Policy : ', grow_policy_sp))
  print(paste('Row Number: ', row_num_sp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.0005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam_sp,
      lambda = lambda_sp,
      alpha = 0,
      max_depth = max_depth_sp,
      min_child_weight = weight_sp,
      subsample = subsample_sp,
      colsample_bytree = by_tree_sp,
      colsample_bylevel = by_level_sp,
      colsample_bynode = by_node_sp,
      tree_method = tree_method_sp,
      grow_policy = grow_policy_sp
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = 2500,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

sp_tuning_methods <- expand_grid(
  tree_method = c('hist','approx','exact'),
  grow_policy = c('depthwise','lossguide')
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(tree_method, grow_policy, row_num), hyperparam_sp_tuning_methods)) %>% 
  ungroup()

sp_tuning_methods %>% 
  arrange(rmse) %>% 
  head(5)

sp_tuning_methods_best <- sp_tuning_methods %>% 
  slice_min(rmse, n = 1) %>% # rmse:
  slice(1)
# tree_method grow_policy row_num  rmse
# <chr>       <chr>         <int> <dbl>
# approx      depthwise         3  222.

#5 CV
# # A tibble: 1 × 4
# tree_method grow_policy row_num  rmse
# <chr>       <chr>         <int> <dbl>
#   1 exact       depthwise         5  229.

tree_method_sp <- sp_tuning_methods_best$tree_method #approx
grow_policy_sp <- sp_tuning_methods_best$grow_policy #depthwise

hyperparam_sp_tuning_eta <- function(eta_sp, early_stopping_rounds_sp, row_num_sp){
  
  print(paste('eta: ', eta_sp))
  print(paste('Early Stopping Rounds : ', early_stopping_rounds_sp))
  print(paste('Row Number: ', row_num_sp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = eta_sp,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = gam_sp,
      lambda = lambda_sp,
      alpha = 0,
      max_depth = max_depth_sp,
      min_child_weight = weight_sp,
      subsample = subsample_sp,
      colsample_bytree = by_tree_sp,
      colsample_bylevel = by_level_sp,
      colsample_bynode = by_node_sp,
      tree_method = tree_method_sp,
      grow_policy = grow_policy_sp
    ),
    data = dtrain_pitching,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 500,
    early_stopping_rounds = early_stopping_rounds_sp,
    nthread = 7,
    seed = 101
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  nrounds <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(iter)
  
  df <- tibble(
    rmse, 
    nrounds,
    eta_sp,
    early_stopping_rounds_sp
  )
}

hyperparam_sp_tuning_eta_df <- tibble(
  eta = 0.0005/1:5,
  early_stopping_rounds = 2500*1:5,
  row_num = 1:5
)

hyperparam_sp_tuning_eta_results <- pmap_df(
  hyperparam_sp_tuning_eta_df, 
  hyperparam_sp_tuning_eta
)



hyperparam_sp_tuning_eta_results %>% 
  arrange(rmse) %>% 
  head(5)

best_eta_tuning_sp <- hyperparam_sp_tuning_eta_results %>% 
  slice_min(rmse, n = 1)


eta_sp <- best_eta_tuning_sp$eta_sp # 0.00025
nrounds_sp <- best_eta_tuning_sp$nrounds # 14596


### RMSE: 220.37 ###

#### Train Model ####
batters_faced_xg <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'SP') %>% 
  reframe(
    playing_time_2023,
    playing_time_2022,
    playing_time_2021,
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
    debut_month, 
    age_23,
    years_since_debut_23,
    bf_second_half = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
      .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
      .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
    ),
    times_faced_2021,
    times_faced_2022,
    avg_pitches_per_appearance_2021,
    avg_pitches_per_appearance_2022,
    avg_bf_per_appearance_2021,
    avg_bf_per_appearance_2022,
    earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
    earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
    ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
    ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
    pitches_thrown = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
      .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    num_in_rotation_2022,
    num_in_rotation_2021,
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*weight_2022_6sp,
      .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
      .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
      .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
      .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
      .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
      .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
    ),
    
    hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
    hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
    
    barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
    barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
    
    ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
    ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
    
    WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
    WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
    
    SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
    SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
    
    BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
    BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
    
    OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
    OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
    
    hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
    hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
    
    gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
    gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
    
    cluster_2023_sp = as.factor(cluster_2023_sp),
    cluster_2023_sp_prop = as.factor(cluster_2023_sp_prop),
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    
    percentile_90_velo = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
      playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
      .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
  ) %>% 
  mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
         debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
           .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*weight_2022_6sp,
           .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*weight_2022_6sp,
           .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
         
         num_in_rotation = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
           .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))


batters_faced_xg <- batters_faced_xg %>% 
  dummy_cols(select_columns = c('cluster_2023_sp', 'cluster_2023_sp_prop'), remove_selected_columns = TRUE)

set.seed(101);sp_split <- initial_split(batters_faced_xg, prop = 0.7, strata = playing_time_2023) 

train_pitching <- training(sp_split)
test_pitching <- testing(sp_split)

dtrain_pitching <- xgb.DMatrix(as.matrix(train_pitching %>% select(-playing_time_2023)), label = train_pitching$playing_time_2023)

dtest_pitching <- xgb.DMatrix(as.matrix(test_pitching %>% select(-playing_time_2023)), label = test_pitching$playing_time_2023)

set.seed(101);sp_model <- xgb.train(
  params = list(
    eta = eta_sp,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = gam_sp,
    lambda = lambda_sp,
    alpha = 0,
    max_depth = max_depth_sp,
    min_child_weight = weight_sp,
    subsample = subsample_sp,
    colsample_bytree = by_tree_sp,
    colsample_bylevel = by_level_sp,
    colsample_bynode = by_node_sp,
    tree_method = tree_method_sp,
    grow_policy = grow_policy_sp
  ),
  data = dtrain_pitching,
  nrounds = nrounds_sp,
  watchlist = list(train = dtrain_pitching, test = dtest_pitching),
  print_every_n = 100,
  nthread = 7
) 

xgb.ggplot.importance(xgb.importance(model = sp_model))+#, top_n = 10) +
  ggthemes::theme_few()

test_pitching$preds <- predict(sp_model, newdata = as.matrix(test_pitching %>% 
                                                         select(-playing_time_2023)))

test_pitching %>% ggplot(aes(playing_time_2023)) +
  geom_density(fill = 'blue', alpha = 0.5, adjust = 1) +
  geom_density(data = test_pitching, aes(preds), fill = 'red', alpha = 0.5, adjust = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,800, by = 100)) +
  geom_vline(xintercept = 638, linetype = 'dashed') + 
  geom_vline(xintercept = 95, linetype = 'dashed') +
  labs(x = 'SP BF (2023)',
       title = 'Distribution of BF in 2023 (Blue) Compared to the\nDistribution of Predicted BF (Red) for SP',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 5') +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))


test_pitching %>% 
  mutate(abs_error = abs(playing_time_2023 - preds)) %>% 
  ggplot(aes(playing_time_2021, playing_time_2022, color = abs_error)) +
  geom_jitter(position = position_jitter(width = 10, height = 10, seed = 101))+
  scale_color_continuous(low = 'green',high = 'red', name = 'Absolute\nError') +
  theme_bw()


test_pitching %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (SP)",
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)',
       subtitle = 'Predicting on Untrained Data')+
  scale_y_continuous(breaks = seq(-500, 500, by = 100)) +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No'))) #p = 0.89

test_pitching %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (SP)",
       y = 'Prediction Error (BF - Predicted BF)',
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 6')+
  scale_y_continuous(breaks = seq(-600, 600, by = 100),
                     limits = c(-600, 600)) +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No')), annotations = 'p = 0.89') +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))

test_pitching %>% 
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

#### Final Model ####
batters_faced_xg <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'SP') %>% 
  reframe(
    playing_time_2024 = playing_time_2023,#renaming for 2024 predictions
    playing_time_2022,
    playing_time_2021,
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
    debut_month, 
    age_24 = age_23,#renaming for 2024 predictions
    years_since_debut_23,
    bf_second_half = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2sp  + bf_second_half_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3sp + bf_second_half_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
      .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
      .default = bf_diff_2022*weight_2022_1sp + bf_diff_2021*weight_2021_1sp
    ),
    times_faced_2021,
    times_faced_2022,
    avg_pitches_per_appearance_2021,
    avg_pitches_per_appearance_2022,
    avg_bf_per_appearance_2021,
    avg_bf_per_appearance_2022,
    earned_runs_2022 = ifelse(is.na(earned_runs_2022), 0,earned_runs_2022),
    earned_runs_2021 = ifelse(is.na(earned_runs_2021), 0,earned_runs_2021),
    ERA_2021 = ifelse(is.na(ERA_2021),quantile(ERA_2021, probs = 0.9, na.rm = TRUE),ERA_2021),
    ERA_2022 = ifelse(is.na(ERA_2022),quantile(ERA_2022, probs = 0.9, na.rm = TRUE),ERA_2022),
    pitches_thrown = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2sp  + pitches_thrown_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3sp + pitches_thrown_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
      .default = pitches_thrown_2022*weight_2022_1sp + pitches_thrown_2021*weight_2021_1sp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    num_in_rotation_2022,
    num_in_rotation_2021,
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2sp  + so_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3sp + so_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ so_2022*weight_2022_6sp,
      .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
      .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
      .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
      .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
      .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
      .default = hardhitsa_2022*weight_2022_1sp + hardhitsa_2021*weight_2021_1sp
    ),
    
    hardhit_pcta_2021 = ifelse(is.na(hardhit_pcta_2021),quantile(hardhit_pcta_2021, probs = 0.9, na.rm = TRUE),hardhit_pcta_2021),
    hardhit_pcta_2022 = ifelse(is.na(hardhit_pcta_2022),quantile(hardhit_pcta_2022, probs = 0.9, na.rm = TRUE),hardhit_pcta_2022),
    
    barrel_pcta_2021 = ifelse(is.na(barrel_pcta_2021),quantile(barrel_pcta_2021, probs = 0.9, na.rm = TRUE), barrel_pcta_2021),
    barrel_pcta_2022 = ifelse(is.na(barrel_pcta_2022),quantile(barrel_pcta_2022, probs = 0.9, na.rm = TRUE),barrel_pcta_2022),
    
    ChaseRateA_2021 = ifelse(is.na(ChaseRateA_2021),quantile(ChaseRateA_2021, probs = 0.1, na.rm = TRUE),ChaseRateA_2021),
    ChaseRateA_2022 = ifelse(is.na(ChaseRateA_2022),quantile(ChaseRateA_2022, probs = 0.1, na.rm = TRUE),ChaseRateA_2022),
    
    WhiffRateA_2021 = ifelse(is.na(WhiffRateA_2021),quantile(WhiffRateA_2021, probs = 0.1, na.rm = TRUE),WhiffRateA_2021),
    WhiffRateA_2022 = ifelse(is.na(WhiffRateA_2022),quantile(WhiffRateA_2022, probs = 0.1, na.rm = TRUE),WhiffRateA_2022),
    
    SLGA_2021 = ifelse(is.na(SLGA_2021),quantile(SLGA_2021, probs = 0.9, na.rm = TRUE),SLGA_2021),
    SLGA_2022 = ifelse(is.na(SLGA_2022),quantile(SLGA_2022, probs = 0.9, na.rm = TRUE),SLGA_2022),
    
    BAA_2021 = ifelse(is.na(BAA_2021),quantile(BAA_2021, probs = 0.9, na.rm = TRUE),BAA_2021),
    BAA_2022 = ifelse(is.na(BAA_2022),quantile(BAA_2022, probs = 0.9, na.rm = TRUE),BAA_2022),
    
    OBPA_2021 = ifelse(is.na(OBPA_2021),quantile(OBPA_2021, probs = 0.9, na.rm = TRUE),OBPA_2021),
    OBPA_2022 = ifelse(is.na(OBPA_2022),quantile(OBPA_2022, probs = 0.9, na.rm = TRUE),OBPA_2022),
    
    hr_pcta_2021 = ifelse(is.na(hr_pcta_2021),quantile(hr_pcta_2021, probs = 0.9, na.rm = TRUE),hr_pcta_2021), 
    hr_pcta_2022 = ifelse(is.na(hr_pcta_2022),quantile(hr_pcta_2022, probs = 0.9, na.rm = TRUE),hr_pcta_2022),
    
    gball_rate_2021 = ifelse(is.na(gball_rate_2021), quantile(gball_rate_2021, probs = 0.9, na.rm = TRUE), gball_rate_2021),
    gball_rate_2022 = ifelse(is.na(gball_rate_2022), quantile(gball_rate_2022, probs = 0.9, na.rm = TRUE), gball_rate_2022),
    
    cluster_2024_sp = as.factor(cluster_2023_sp), #renaming for 2024 predictions
    cluster_2024_sp_prop = as.factor(cluster_2023_sp_prop),#renaming for 2024 predictions
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    
    percentile_90_velo = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2sp  + percentile_90_velo_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3sp + percentile_90_velo_2021*weight_2021_3sp,
      playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
      .default = percentile_90_velo_2022*weight_2022_1sp + percentile_90_velo_2021*weight_2021_1sp)
  ) %>% 
  mutate(rookie_year_2023 = ifelse(years_since_debut_23 == 1, 1, 0), #renaming for 2024 predictions
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0),#renaming for 2024 predictions
         rookie_year_2022 = ifelse(years_since_debut_23 == 2, 1, 0),#renaming for 2024 predictions
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),#renaming for 2024 predictions
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2sp  + kpct_2021*weight_2021_2sp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3sp + kpct_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1sp + kpct_2021*weight_2021_1sp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2sp  + earned_runs_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3sp + earned_runs_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
           .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ RV_2022*weight_2022_6sp,
           .default = RV_2022*weight_2022_1sp + RV_2021*weight_2021_1sp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2sp  + RV100_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3sp + RV100_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1sp + RV100_2021*weight_2021_1sp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2sp  + ip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3sp + ip_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month > 8  ~ ip_2022*weight_2022_6sp,
           .default = ip_2022*weight_2022_1sp + ip_2021*weight_2021_1sp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2sp  + wOBAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3sp + wOBAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1sp + wOBAA_2021*weight_2021_1sp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2sp  + times_faced_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3sp + times_faced_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1sp + times_faced_2021*weight_2021_1sp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2sp  + avg_pitches_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3sp + avg_pitches_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1sp + avg_pitches_per_appearance_2021*weight_2021_1sp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2sp  + avg_bf_per_appearance_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3sp + avg_bf_per_appearance_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1sp + avg_bf_per_appearance_2021*weight_2021_1sp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2sp  + ERA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3sp + ERA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1sp + ERA_2021*weight_2021_1sp),
         
         num_in_rotation = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ num_in_rotation_2022*weight_2022_2sp  + num_in_rotation_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ num_in_rotation_2022*weight_2022_3sp + num_in_rotation_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ num_in_rotation_2022,
           .default = num_in_rotation_2022*weight_2022_1sp + num_in_rotation_2021*weight_2021_1sp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2sp  + fip_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3sp + fip_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1sp + fip_2021*weight_2021_1sp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2sp  + hardhit_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3sp + hardhit_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1sp + hardhit_pcta_2021*weight_2021_1sp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2sp  + barrel_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3sp + barrel_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1sp + barrel_pcta_2021*weight_2021_1sp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2sp  + ChaseRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3sp + ChaseRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1sp + ChaseRateA_2021*weight_2021_1sp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2sp  + WhiffRateA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3sp + WhiffRateA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1sp + WhiffRateA_2021*weight_2021_1sp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2sp  + SLGA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3sp + SLGA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1sp + SLGA_2021*weight_2021_1sp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2sp  + BAA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3sp + BAA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1sp + BAA_2021*weight_2021_1sp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2sp  + OBPA_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3sp + OBPA_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1sp + OBPA_2021*weight_2021_1sp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2sp  + hr_pcta_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3sp + hr_pcta_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1sp + hr_pcta_2021*weight_2021_1sp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2sp  + gball_rate_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3sp + gball_rate_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1sp + gball_rate_2021*weight_2021_1sp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2sp  + avg_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3sp + avg_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1sp + avg_ev_2021*weight_2021_1sp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2sp  + percentile_90_ev_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3sp + percentile_90_ev_2021*weight_2021_3sp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1sp + percentile_90_ev_2021*weight_2021_1sp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month)) %>% 
  rename('playing_time_2023' = 'playing_time_2022', 'playing_time_2022' = 'playing_time_2021',
         'years_since_debut_24' = 'years_since_debut_23')


batters_faced_xg <- batters_faced_xg %>% 
  dummy_cols(select_columns = c('cluster_2024_sp', 'cluster_2024_sp_prop'), remove_selected_columns = TRUE)

glimpse(batters_faced_xg) #checking final structure

dtrain_pitching <- xgb.DMatrix(as.matrix(batters_faced_xg %>% select(-playing_time_2024)), label = batters_faced_xg$playing_time_2024)

set.seed(101);sp_mod <- xgboost(
  params = list(
    eta = eta_sp,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = gam_sp,
    lambda = lambda_sp,
    alpha = 0,
    max_depth = max_depth_sp,
    min_child_weight = weight_sp,
    subsample = subsample_sp,
    colsample_bytree = by_tree_sp,
    colsample_bylevel = by_level_sp,
    colsample_bynode = by_node_sp,
    tree_method = tree_method_sp,
    grow_policy = grow_policy_sp
  ),
  data = dtrain_pitching,
  nrounds = nrounds_sp,
  # watchlist = list(train = dtrain, test = dtest),
  print_every_n = 500
)

#saving model to R directory
save(sp_mod, file = 'BF SP Model_edited.RData')
