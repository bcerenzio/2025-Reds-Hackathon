library(dbarts)
library(fastDummies)
library(rsample)
library(tidyverse)

find_weight1_bfsp <- function(weight_2021_1sp){
  weight_2022_1sp <- 1-weight_2021_1sp
  print(paste('Weight 2021 1 SP: ', weight_2021_1sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*0.2  + bf_diff_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*0.2  + HRA_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*0.2  + walks_hbp_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*0.2  + total_basesa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*0.2  + barrelsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*0.2  + hardhitsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*0.2  + RV_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*3,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting1_function_sp_df <- tibble(
  weight_2021_1sp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight1_bfsp)
)

weighting1_function_sp_df %>% ggplot(aes(weight_2021_1sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() #rmse = 234.51

weight_2021_1sp <- weighting1_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_1sp) #0
weight_2022_1sp <- 1 - weight_2021_1sp #1



find_weight2_bfsp <- function(weight_2021_2sp){
  weight_2022_2sp <- 1-weight_2021_2sp
  print(paste('Weight 2021 2 SP: ', weight_2021_2sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*3,
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
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*3,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting2_function_sp_df <- tibble(
  weight_2021_2sp = seq(0.5, 1, by = 0.1),
  rmse = map_dbl(seq(0.5,1,by = 0.1), find_weight2_bfsp)
)

weighting2_function_sp_df %>% ggplot(aes(weight_2021_2sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() 

weight_2021_2sp <- weighting2_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_2sp) #0.7
weight_2022_2sp <- 1 - weight_2021_2sp #0.3


find_weight3_bfsp <- function(weight_2021_3sp){
  weight_2022_3sp <- 1-weight_2021_3sp
  print(paste('Weight 2021 3 SP: ', weight_2021_3sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*3,
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
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*3,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting3_function_sp_df <- tibble(
  weight_2021_3sp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight3_bfsp)
)

weighting3_function_sp_df %>% ggplot(aes(weight_2021_3sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() #rmse = ; weight = 

weight_2021_3sp <- weighting3_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_3sp) #0.15 
weight_2022_3sp <- 1 - weight_2021_3sp #0.85

find_weight4_bfsp <- function(weight_2022_4sp){
  print(paste('Weight 2022 4 SP: ', weight_2022_4sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*3,
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
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*3,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting4_function_sp_df <- tibble(
  weight_2022_4sp = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), find_weight4_bfsp)
)

weighting4_function_sp_df %>% ggplot(aes(weight_2022_4sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() 

weight_2022_4sp <- weighting4_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_4sp) # 1.6


find_weight5_bfsp <- function(weight_2022_5sp){
  print(paste('Weight 2022 5 SP: ', weight_2022_5sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*3,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*3,
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
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*3,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*3,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting5_function_sp_df <- tibble(
  weight_2022_5sp = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), find_weight5_bfsp)
)

weighting5_function_sp_df %>% ggplot(aes(weight_2022_5sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() #rmse = ; weight = 

weight_2022_5sp <- weighting5_function_sp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_5sp) # 1.9

find_weight6_bfsp <- function(weight_2022_6sp){
  print(paste('Weight 2022 6 SP: ', weight_2022_6sp))
  
  batters_faced_xg <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key == 'SP') %>% 
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
        .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
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
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*weight_2022_6sp,
        .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
        .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
        .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
        .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
        .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
        years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
        years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
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
           debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
             .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*weight_2022_6sp,
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
             years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
             years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*weight_2022_6sp,
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
  
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
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

weighting6_function_sp_df <- tibble(
  weight_2022_6sp = seq(1.5, 3.5, by = 0.1),
  rmse = map_dbl(seq(1.5,3.5,by = 0.1), find_weight6_bfsp)
)

weighting6_function_sp_df %>% ggplot(aes(weight_2022_6sp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() #rmse = ; weight = 

weight_2022_6sp <- weighting6_function_sp_df %>% slice_min(rmse, n = 1) %>% slice(1) %>% pull(weight_2022_6sp) # 1.5

batters_faced_xg <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key == 'SP') %>% 
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
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_second_half_2022*weight_2022_6sp,
      .default = bf_second_half_2022*weight_2022_1sp + bf_second_half_2021*weight_2021_1sp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2sp  + bf_diff_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3sp + bf_diff_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ bf_diff_2022*weight_2022_6sp,
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
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ pitches_thrown_2022*weight_2022_6sp,
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
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ so_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ so_2022*weight_2022_6sp,
      .default = so_2022*weight_2022_1sp + so_2021*weight_2021_1sp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2sp  + HRA_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3sp + HRA_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ HRA_2022*weight_2022_6sp,
      .default = HRA_2022*weight_2022_1sp + HRA_2021*weight_2021_1sp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2sp  + walks_hbp_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3sp + walks_hbp_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ walks_hbp_2022*weight_2022_6sp,
      .default = walks_hbp_2022*weight_2022_1sp + walks_hbp_2021*weight_2021_1sp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2sp  + total_basesa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3sp + total_basesa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ total_basesa_2022*weight_2022_6sp,
      .default = total_basesa_2022*weight_2022_1sp + total_basesa_2021*weight_2021_1sp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2sp  + barrelsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3sp + barrelsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ barrelsa_2022*weight_2022_6sp,
      .default = barrelsa_2022*weight_2022_1sp + barrelsa_2021*weight_2021_1sp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2sp  + hardhitsa_2021*weight_2021_2sp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3sp + hardhitsa_2021*weight_2021_3sp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4sp,
      years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5sp,
      years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ hardhitsa_2022*weight_2022_6sp,
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
         debut_month_2022 = ifelse(rookie_year_2021 == 1, debut_month, 0),
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
           years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ earned_runs_2022*weight_2022_6sp,
           .default = earned_runs_2022*weight_2022_1sp + earned_runs_2021*weight_2021_1sp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2sp  + RV_2021*weight_2021_2sp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3sp + RV_2021*weight_2021_3sp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4sp,
           years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ RV_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ RV_2022*weight_2022_6sp,
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
           years_since_debut_23 == 1  & debut_month >= 8 & debut_month > 6  ~ ip_2022*weight_2022_5sp,
           years_since_debut_23 == 1  & debut_month >= 10 & debut_month > 8  ~ ip_2022*weight_2022_6sp,
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

hyperparam_bart_bfsp <- function(sigdf_sp, sigquant_sp, k_sp, power_sp, row_num){
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_sp))
  print(paste('Sigquant: ', sigquant_sp))
  print(paste('K: ', k_sp))
  print(paste('Power: ', power_sp))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)

    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_sp,
                   sigquant = sigquant_sp,
                   k = k_sp,
                   power = power_sp,
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
hyperparam_bart_bfsp_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bfsp_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bfsp_0.75_df$sigdf,
                                                   hyperparam_bart_bfsp_0.75_df$sigquant, 
                                                   hyperparam_bart_bfsp_0.75_df$k, 
                                                   hyperparam_bart_bfsp_0.75_df$power,
                                                   hyperparam_bart_bfsp_0.75_df$row_num), hyperparam_bart_bfsp)

hyperparam_bart_bfsp_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bfsp_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bfsp_0.9_df$sigdf,
                                                  hyperparam_bart_bfsp_0.9_df$sigquant, 
                                                  hyperparam_bart_bfsp_0.9_df$k, 
                                                  hyperparam_bart_bfsp_0.9_df$power,
                                                  hyperparam_bart_bfsp_0.9_df$row_num), hyperparam_bart_bfsp)

hyperparam_bart_bfsp_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(0.5,1,2,3),
  power = c(1,2,3)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bfsp_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bfsp_0.99_df$sigdf,
                                                   hyperparam_bart_bfsp_0.99_df$sigquant, 
                                                   hyperparam_bart_bfsp_0.99_df$k, 
                                                   hyperparam_bart_bfsp_0.99_df$power,
                                                   hyperparam_bart_bfsp_0.99_df$row_num), hyperparam_bart_bfsp)

hyperparam_bart_bfsp_df <- bind_rows(hyperparam_bart_bfsp_0.75_df, hyperparam_bart_bfsp_0.9_df, hyperparam_bart_bfsp_0.99_df)

hyperparam_bart_bfsp_df <- hyperparam_bart_bfsp_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

sigdf_sp <- hyperparam_bart_bfsp_df$sigdf #3
sigquant_sp <- hyperparam_bart_bfsp_df$sigquant #0.9
k_sp <- hyperparam_bart_bfsp_df$k #2
power_sp <- hyperparam_bart_bfsp_df$power #1

### RMSE = 233.26

hyperparam_bart_trees_bfsp <- function(trees_sp){
  set.seed(101);split <- initial_split(batters_faced_xg, 0.7, strata = playing_time_2023)
  
  train_data <- training(split)
  test_data <- testing(split)
  
  print(paste('Trees: ', trees_sp))
  
  test_x <- test_data %>% select(-playing_time_2023)
  
  test_matrix <- as.matrix(test_x)
  
  set.seed(101);model <- bart2(playing_time_2023 ~ ., 
                               data = train_data, 
                               test = test_matrix,
                               sigdf = sigdf_sp,
                               sigquant = sigquant_sp,
                               k = k_sp,
                               power = power_sp,
                               n.trees = trees_sp,
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


find_trees_bfsp <- tibble(
  trees = seq(200, 1600, by = 100),
  rmse = map_dbl(seq(200, 1600, by = 100), hyperparam_bart_trees_bfsp)
)

find_trees_bf %>% ggplot(aes(trees, rmse)) +
  geom_line() +
  geom_smooth() #900

trees_sp <- 900

# finding final rmse with cross validation
final_rmse_bart_bfsp <- function(){
  set.seed(101);batters_faced_xg$fold <- sample(1:20, nrow(batters_faced_xg), replace = TRUE)
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- batters_faced_xg %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- batters_faced_xg %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-playing_time_2023,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(playing_time_2023 ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_sp,
                   sigquant = sigquant_sp,
                   k = k_sp,
                   power = power_sp,
                   n.trees = trees_sp,
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

(final_rmse <- final_rmse_bart_bfsp())  # rmse: 234.26

