library(xgboost)
library(fastDummies)
library(tidyverse)
library(rsample)

slice <- dplyr::slice

#### New Model ####
find_weight1_bfrp <- function(weight_2021_1rp){
  weight_2022_1rp <- 1-weight_2021_1rp
  print(paste('Weight 2021 1 RP: ', weight_2021_1rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*0.2  + bf_diff_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*3,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*3,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*0.2  + so_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*0.8 + so_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*0.2  + HRA_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*0.2  + walks_hbp_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*0.2  + total_basesa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*0.2  + barrelsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*0.2  + hardhitsa_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*3,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*0.2  + inn9apps_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*0.8 + inn9apps_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*3,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*0.2  + percentile_90_velo_2021*0.8, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*0.8 + percentile_90_velo_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*0.2  + kpct_2021*0.8,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*0.8 + kpct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*0.2  + earned_runs_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*0.8 + earned_runs_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*0.2  + RV_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*3,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*0.2  + RV100_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*0.8 + RV100_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*0.2  + ip_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*0.8 + ip_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*3,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*0.2  + wOBAA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*0.8 + wOBAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*0.2  + times_faced_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*0.8 + times_faced_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*0.2  + avg_pitches_per_appearance_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*0.8 + avg_pitches_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*0.2  + avg_bf_per_appearance_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*0.8 + avg_bf_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*0.2  + ERA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*0.8 + ERA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*0.2  + fip_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*0.8 + fip_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*0.2  + hardhit_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*0.8 + hardhit_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*0.2  + barrel_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*0.8 + barrel_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*0.2  + ChaseRateA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*0.8 + ChaseRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*0.2  + WhiffRateA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*0.8 + WhiffRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*0.2  + SLGA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*0.8 + SLGA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*0.2  + BAA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*0.8 + BAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*0.2  + OBPA_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*0.8 + OBPA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*0.2  + hr_pcta_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*0.8 + hr_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*0.2  + gball_rate_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*0.8 + gball_rate_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*0.2  + avg_ev_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*0.2  + percentile_90_ev_2021*0.8, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_rp,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 1000,
    seed = 101,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting1_function_rp_df <- tibble(
  weight_2021_1rp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight1_bfrp)
)

weighting1_function_rp_df %>% ggplot(aes(weight_2021_1rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +#rmse = 105.18
  ylab('RMSE') +
  xlab('Weight 2021 1 (RP)')
  

ggsave('RP Weight 1 2021.png', width = 4, height = 4.76)
  
weight_2021_1rp <- weighting1_function_rp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_1rp) #0.2
weight_2022_1rp <- 1 - weight_2021_1rp #0.8



find_weight2_bfrp <- function(weight_2021_2rp){
  weight_2022_2rp <- 1-weight_2021_2rp
  print(paste('Weight 2021 2 RP: ', weight_2021_2rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*0.8 + bf_second_half_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*0.8 + bf_diff_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*3,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*0.8 + pitches_thrown_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*3,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*0.8 + so_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*0.8 + HRA_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*0.8 + walks_hbp_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*0.8 + total_basesa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*0.8 + barrelsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*0.8 + hardhitsa_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*3,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*0.8 + inn9apps_2021*0.2,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*3,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*0.8 + percentile_90_velo_2021*0.2,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*0.8 + kpct_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*0.8 + earned_runs_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*0.8 + RV_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*3,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*0.8 + RV100_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*0.8 + ip_2021*0.2,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*3,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*0.8 + wOBAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*0.8 + times_faced_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*0.8 + avg_pitches_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*0.8 + avg_bf_per_appearance_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*0.8 + ERA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*0.8 + fip_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*0.8 + hardhit_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*0.8 + barrel_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*0.8 + ChaseRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*0.8 + WhiffRateA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*0.8 + SLGA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*0.8 + BAA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*0.8 + OBPA_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*0.8 + hr_pcta_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*0.8 + gball_rate_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*0.8 + avg_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*0.8 + percentile_90_ev_2021*0.2,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
    ),
    data = dtrain_rp,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    seed = 101,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting2_function_rp_df <- tibble(
  weight_2021_2rp = seq(0.5, 1, by = 0.05),
  rmse = map_dbl(seq(0.5,1,by = 0.05), find_weight2_bfrp)
)

weighting2_function_rp_df %>% ggplot(aes(weight_2021_2rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  ylab('RMSE') +
  xlab('Weight 2021 2 (RP)')

ggsave('RP Weight 2 2021.png', width = 4, height = 4.76)

weight_2021_2rp <- weighting2_function_rp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_2rp) #0.55
weight_2022_2rp <- 1 - weight_2021_2rp #0.45


find_weight3_bfrp <- function(weight_2021_3rp){
  weight_2022_3rp <- 1-weight_2021_3rp
  print(paste('Weight 2021 3 RP: ', weight_2021_3rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*3,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*3,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*3,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*weight_2022_3rp + inn9apps_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*1.4,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*3,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3rp + percentile_90_velo_2021*weight_2021_3rp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*3,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*1.4,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*3,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
    ),
    data = dtrain_rp,
    nrounds = 200000,
    nfold = 35,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting3_function_rp_df <- tibble(
  weight_2021_3rp = seq(0, 0.3, by = 0.05),
  rmse = map_dbl(seq(0,0.3,by = 0.05), find_weight3_bfrp)
)

weighting3_function_rp_df %>% ggplot(aes(weight_2021_3rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  ylab('RMSE') +
  xlab('Weight 2021 3 (RP)')
  
ggsave('RP Weight 3 2021.png', width = 4, height = 4.76)

weight_2021_3rp <- weighting3_function_rp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2021_3rp) #0.2
weight_2022_3rp <- 1 - weight_2021_3rp #0.8

find_weight4_bfrp <- function(weight_2022_4rp){
  print(paste('Weight 2022 4 RP: ', weight_2022_4rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2021_3rp  + bf_second_half_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*3,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*3,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*3,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*weight_2022_3rp + inn9apps_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*2.2,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*3,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3rp + percentile_90_velo_2021*weight_2021_3rp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*3,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*2.2,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*3,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
    ),
    data = dtrain_rp,
    seed = 101,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting4_function_rp_df <- tibble(
  weight_2022_4rp = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), find_weight4_bfrp)
)

weighting4_function_rp_df %>% ggplot(aes(weight_2022_4rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  ylab('RMSE') +
  xlab('Weight 2022 4 (RP)')

ggsave('RP Weight 4 2022.png', width = 4, height = 4.76)

weight_2022_4rp <- weighting4_function_rp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_4rp) # 1.6


find_weight5_bfrp <- function(weight_2022_5rp){
  print(paste('Weight 2022 5 RP: ', weight_2022_5rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*3,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*3,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*3,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*3,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*3,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*3,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*3,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*3,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*3,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*weight_2022_3rp + inn9apps_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*3,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3rp + percentile_90_velo_2021*weight_2021_3rp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*3,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*3,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*3,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
    ),
    data = dtrain_rp,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    early_stopping_rounds = 1000,
    seed = 101,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting5_function_rp_df <- tibble(
  weight_2022_5rp = seq(1, 3, by = 0.1),
  rmse = map_dbl(seq(1,3,by = 0.1), find_weight5_bfrp)
)

weighting5_function_rp_df %>% ggplot(aes(weight_2022_5rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  ylab('RMSE') +
  xlab('Weight 2022 5 (RP)')

ggsave('RP Weight 5 2022.png', width = 4, height = 4.76)

weight_2022_5rp <- weighting5_function_rp_df %>% slice_min(rmse, n = 1) %>% pull(weight_2022_5rp) # 1.7

find_weight6_bfrp <- function(weight_2022_6rp){
  print(paste('Weight 2022 6 RP: ', weight_2022_6rp))
  
  batters_faced_xg_rp <- batters_faced %>% 
    filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
    filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
    filter(role_key_2023 == 'RP') %>% 
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*weight_2022_6rp,
        .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
      ),
      wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
      wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
      bf_diff = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*weight_2022_6rp,
        .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*weight_2022_6rp,
        .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
      ),
      ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
      ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
      
      fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
      fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
      so = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*weight_2022_6rp,
        .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
      ),
      kpct_2022 = so_2022/playing_time_2022,
      kpct_2021 = so_2021/playing_time_2021,
      HRA = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*weight_2022_6rp,
        .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
      ),
      walks_hbp = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*weight_2022_6rp,
        .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
      ),
      total_basesa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*weight_2022_6rp,
        .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
      ),
      barrelsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*weight_2022_6rp,
        .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
      ),
      hardhitsa = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*weight_2022_6rp,
        .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
      
      cluster_2023_rp = as.factor(cluster_2023_rp),
      
      avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
      avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
      
      percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
      percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
      
      inn9apps = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*weight_2022_3rp + inn9apps_2021*weight_2021_3rp,
        years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
        years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*weight_2022_4rp,
        years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*weight_2022_5rp,
        years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*weight_2022_6rp,
        .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
      ),
      
      percentile_90_velo = case_when(
        playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
        playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3rp + percentile_90_velo_2021*weight_2021_3rp,
        playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
        .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
    ) %>% 
    mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
           debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
           rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
           debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
           kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
           kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
           kpct = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
             .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
           earned_runs = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*weight_2022_6rp,
             .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
           ),
           RV = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*weight_2022_6rp,
             .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
           ),
           RV100 = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
             .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
           ip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
             years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
             years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
             years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5rp,
             years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*weight_2022_6rp,
             .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
           ),
           wOBAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
             .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
           
           times_faced = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
             .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
           
           avg_pitches_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
             .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
           
           avg_bf_per_appearance = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
             .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
           
           ERA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
             .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
           
           
           fip = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
             .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
           
           hardhit_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
             .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
           
           barrel_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
             .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
           
           ChaseRateA  = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
             .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
           
           WhiffRateA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
             .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
           
           SLGA =  case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
             .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
           
           BAA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
             .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
           
           OBPA = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
             .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
           
           hr_pcta = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
             .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
           
           gball_rate = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
             .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
           
           avg_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
             .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
           
           percentile_90_ev = case_when(
             playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
             playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
             playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
             .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
    ) %>% 
    select(-c(starts_with('kpct_'), 
              starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
              starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
              starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
              starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
              starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
              starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))
  
  batters_faced_xg_rp <- batters_faced_xg_rp %>% 
    dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)
  
  dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = 1,
      alpha = 3,
      max_depth = 3,
      min_child_weight = 1,
      subsample = 0.4
    ),
    data = dtrain_rp,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    print_every_n = 100,
    seed = 101,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

weighting6_function_rp_df <- tibble(
  weight_2022_6rp = seq(1, 3.5, by = 0.1),
  rmse = map_dbl(seq(1,3.5,by = 0.1), find_weight6_bfrp)
)

weighting6_function_rp_df %>% ggplot(aes(weight_2022_6rp, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw() +
  ylab('RMSE') +
  xlab('Weight 2022 6 (RP)')

ggsave('RP Weight 6 2022.png', width = 4, height = 4.76)

weight_2022_6rp <- weighting6_function_rp_df %>% slice_min(rmse, n = 1) %>% slice(1) %>% pull(weight_2022_6rp) # 1.2


batters_faced_xg_rp <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'RP') %>% 
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
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*weight_2022_6rp,
      .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*weight_2022_6rp,
      .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*weight_2022_6rp,
      .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*weight_2022_6rp,
      .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*weight_2022_6rp,
      .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*weight_2022_6rp,
      .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*weight_2022_6rp,
      .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*weight_2022_6rp,
      .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*weight_2022_6rp,
      .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
    
    cluster_2023_rp = as.factor(cluster_2023_rp),
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
    
    inn9apps = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ inn9apps_2022*weight_2022_2rp  + inn9apps_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ inn9apps_2022*weight_2022_3rp + inn9apps_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ inn9apps_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ inn9apps_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ inn9apps_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ inn9apps_2022*weight_2022_6rp,
      .default = inn9apps_2022*weight_2022_1rp + inn9apps_2021*weight_2021_1rp
    ),
    
    percentile_90_velo = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_velo_2022*weight_2022_2rp  + percentile_90_velo_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_velo_2022*weight_2022_3rp + percentile_90_velo_2021*weight_2021_3rp,
      playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_velo_2022,
      .default = percentile_90_velo_2022*weight_2022_1rp + percentile_90_velo_2021*weight_2021_1rp)
  ) %>% 
  mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
         debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*weight_2022_6rp,
           .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*weight_2022_6rp,
           .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*weight_2022_6rp,
           .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))


batters_faced_xg_rp <- batters_faced_xg_rp %>% 
  dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)

dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2023)), label = batters_faced_xg_rp$playing_time_2023)


hyperparam_rp_tuning_reg <- function(max_depth_rp, weight_rp, lambda_rp,
                                     alpha_rp,subsample_rp, row_num_rp){
  
  print(paste('Max Depth: ', max_depth_rp))
  print(paste('Weight: ', weight_rp))
  print(paste('Lambda: ', lambda_rp))
  print(paste('Alpha: ', alpha_rp))
  print(paste('Subsample: ', subsample_rp))
  print(paste('Row Number: ', row_num_rp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = lambda_rp,
      alpha = alpha_rp,
      max_depth = max_depth_rp,
      min_child_weight = weight_rp,
      subsample = subsample_rp
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain_rp,
    nrounds = 200000,
    #watchlist = list(train = dtrain, test = dtest),
    nfold = 35,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_rp_df <- expand_grid(
  max_depth = c(2,3,4),
  weight = c(1, 4, 10),
  lambda = c(1, 12),
  alpha = c(1,6),
  subsample = c(0.3,0.35, 0.4)
) %>% 
  mutate(row_num = row_number())

reg_tuning_rp_df <- reg_tuning_rp_df %>% 
  rowwise() %>% 
  mutate(
    rmse = pmap_dbl(list(max_depth, weight, lambda, alpha, subsample, row_num), hyperparam_rp_tuning_reg)
  ) %>% 
  ungroup()

reg_tuning_rp_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_rp_best <- reg_tuning_rp_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

# # A tibble: 1 × 7
# max_depth weight  lambda  alpha   subsample  row_num  rmse
# <dbl>      <dbl>  <dbl>   <dbl>     <dbl>     <int>   <dbl>
#   3         4       1       6        0.35      53      104.

alpha_rp <- reg_tuning_rp_best$alpha # 6
lambda_rp <- reg_tuning_rp_best$lambda # 1
max_depth_rp <- reg_tuning_rp_best$max_depth # 3
weight_rp <- reg_tuning_rp_best$weight # 4
subsample_rp <- reg_tuning_rp_best$subsample # 0.35


hyperparam_rp_tuning_col <- function(by_tree_rp, by_level_rp, 
                                     by_node_rp, row_num_rp){
  
  print(paste('By Tree: ', by_tree_rp))
  print(paste('By Level: ', by_level_rp))
  print(paste('By Node: ', by_node_rp))
  print(paste('Row Number: ', row_num_rp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = lambda_rp,
      alpha = alpha_rp,
      max_depth = max_depth_rp,
      min_child_weight = weight_rp,
      subsample = subsample_rp,
      colsample_bytree = by_tree_rp,
      colsample_bylevel = by_level_rp,
      colsample_bynode = by_node_rp
    ),
    data = dtrain_rp,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 100,
    seed = 101,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

rp_tuning_col <- expand_grid(
  by_tree = c(0.33,0.67,1),
  by_level = c(0.33,0.67,1),
  by_node = c(0.33, 0.67, 1)
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(by_tree, by_level, by_node, row_num),hyperparam_rp_tuning_col)) %>% 
  ungroup()

rp_tuning_col %>% 
  arrange(rmse) %>% 
  head(5)

rp_tuning_col_best <- rp_tuning_col %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

by_tree_rp <- rp_tuning_col_best$by_tree # 1
by_level_rp <- rp_tuning_col_best$by_level # 1
by_node_rp <- rp_tuning_col_best$by_node # 1


hyperparam_rp_tuning_methods <- function(tree_method_rp, grow_policy_rp, row_num_rp){
  
  print(paste('Tree Method: ', tree_method_rp))
  print(paste('Grow Policy : ', grow_policy_rp))
  print(paste('Row Number: ', row_num_rp))
  
  set.seed(101);mod_pitching <- xgb.cv(
    params = list(
      eta = 0.001,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = lambda_rp,
      alpha = alpha_rp,
      max_depth = max_depth_rp,
      min_child_weight = weight_rp,
      subsample = subsample_rp,
      colsample_bytree = by_tree_rp,
      colsample_bylevel = by_level_rp,
      colsample_bynode = by_node_rp,
      tree_method = tree_method_rp,
      grow_policy = grow_policy_rp
    ),
    data = dtrain_rp,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 100,
    seed = 101,
    early_stopping_rounds = 1000,
    nthread = 7
  ) 
  rmse <- mod_pitching$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

rp_tuning_methods <- expand_grid(
  tree_method = c('hist','approx','exact'),
  grow_policy = c('depthwise','lossguide')
) %>% 
  mutate(row_num = row_number()) %>% 
  rowwise() %>% 
  mutate(rmse = pmap_dbl(list(tree_method, grow_policy, row_num), hyperparam_rp_tuning_methods)) %>% 
  ungroup()

rp_tuning_methods %>% 
  arrange(rmse) %>% 
  head(5)

rp_tuning_methods_best <- rp_tuning_methods %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

tree_method_rp <- rp_tuning_methods_best$tree_method #approx
grow_policy_rp <- rp_tuning_methods_best$grow_policy # depthwise


hyperparam_rp_tuning_eta <- function(eta_rp, early_stopping_rounds_rp, row_num_rp){
  
  print(paste('eta: ', eta_rp))
  print(paste('Early Stopping Rounds : ', early_stopping_rounds_rp))
  print(paste('Row Number: ', row_num_rp))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = eta_rp,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      gamma = 1,
      lambda = lambda_rp,
      alpha = alpha_rp,
      max_depth = max_depth_rp,
      min_child_weight = weight_rp,
      subsample = subsample_rp,
      colsample_bytree = by_tree_rp,
      colsample_bylevel = by_level_rp,
      colsample_bynode = by_node_rp,
      tree_method = tree_method_rp,
      grow_policy = grow_policy_rp
    ),
    data = dtrain_rp,
    nrounds = 200000,
    nfold = 35,
    print_every_n = 100,
    seed = 101,
    early_stopping_rounds = early_stopping_rounds_rp,
    nthread = 7
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
    eta_rp,
    early_stopping_rounds_rp
  )
}

hyperparam_rp_tuning_eta_df <- tibble(
  eta = 0.001/1:5,
  early_stopping_rounds = 1000*1:5,
  row_num = 1:5
)

hyperparam_rp_tuning_eta_results <- pmap_df(list(
  hyperparam_rp_tuning_eta_df$eta, hyperparam_rp_tuning_eta_df$early_stopping_rounds,
  hyperparam_rp_tuning_eta_df$row_num
), hyperparam_rp_tuning_eta)

hyperparam_rp_tuning_eta_results %>% 
  arrange(rmse) %>% 
  head(5)

best_eta_tuning_rp <- hyperparam_rp_tuning_eta_results %>% 
  slice_min(rmse, n = 1) 


eta_rp <- best_eta_tuning_rp$eta_rp # 0.0002
nrounds_rp <- best_eta_tuning_rp$nrounds # 32360

#### RMSE: 103.9

#### Train Model ####
batters_faced_xg_rp <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'RP') %>% 
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
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ bf_second_half_2022*weight_2022_6rp,
      .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*weight_2022_6rp,
      .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*weight_2022_6rp,
      .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*weight_2022_6rp,
      .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*weight_2022_6rp,
      .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*weight_2022_6rp,
      .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*weight_2022_6rp,
      .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*weight_2022_6rp,
      .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*weight_2022_6rp,
      .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
    
    cluster_2023_rp = as.factor(cluster_2023_rp),
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
  ) %>% 
  mutate(rookie_year_2022 = ifelse(years_since_debut_23 == 1, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         rookie_year_2021 = ifelse(years_since_debut_23 == 2, 1, 0),
         debut_month_2021 = ifelse(rookie_year_2021 == 1, debut_month, 0),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*weight_2022_6rp,
           .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*weight_2022_6rp,
           .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*weight_2022_6rp,
           .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
  ) %>% 
  select(-c(starts_with('kpct_'), 
            starts_with('earned_runs_'), starts_with('RV_'), starts_with('RV100_'),
            starts_with('ip_'), starts_with('wOBAA_'), starts_with('times_faced_'), starts_with('avg_pitches_per_appearance_'),
            starts_with('avg_bf_per_appearance_'), starts_with('ERA_'), starts_with('num_in_rotation_'), starts_with('fip_'),
            starts_with('hardhit_pcta_'), starts_with('barrel_pcta'), starts_with('ChaseRateA_'), starts_with('WhiffRateA'),
            starts_with('SLGA_'), starts_with('BAA_'), starts_with('OBPA_'), starts_with('hr_pcta_'), starts_with('gball_rate_'),
            starts_with('avg_ev_'), starts_with('percentile_90_ev_'), -debut_month))

batters_faced_xg_rp <- batters_faced_xg_rp %>% 
  dummy_cols(select_columns = c('cluster_2023_rp'), remove_selected_columns = TRUE)

set.seed(101);rp_split <- initial_split(batters_faced_xg_rp, prop = 0.7, strata = playing_time_2023) 

train_rp <- training(rp_split)
test_rp <- testing(rp_split)

dtrain_rp <- xgb.DMatrix(as.matrix(train_rp %>% select(-playing_time_2023)), label = train_rp$playing_time_2023)

dtest_rp <- xgb.DMatrix(as.matrix(test_rp %>% select(-playing_time_2023)), label = test_rp$playing_time_2023)

set.seed(101);rp_model <- xgb.train(
  params = list(
    eta = eta_rp,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = 1,
    lambda = lambda_rp,
    alpha = alpha_rp,
    max_depth = max_depth_rp,
    min_child_weight = weight_rp,
    subsample = subsample_rp,
    colsample_bytree = by_tree_rp,
    colsample_bylevel = by_level_rp,
    colsample_bynode = by_node_rp,
    tree_method = tree_method_rp,
    grow_policy = grow_policy_rp
  ),
  data = dtrain_rp,
  watchlist = list(train = dtrain_rp, test = dtest_rp),
  print_every_n = 100,
  nrounds = nrounds_rp,
  nthread = 7
) 

xgb.ggplot.importance(xgb.importance(model = rp_model))+#, top_n = 10) +
  ggthemes::theme_few()

test_rp$preds <- predict(rp_model, newdata = as.matrix(test_rp %>%  
                                                         select(-playing_time_2023)))

test_rp %>% ggplot(aes(playing_time_2023)) +
  geom_density(fill = 'blue', alpha = 0.5, adjust = 1) +
  geom_density(data = test_rp, aes(preds), fill = 'red', alpha = 0.5, adjust = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,800, by = 100)) +
  geom_vline(xintercept = 255, linetype = 'dashed') + 
  geom_vline(xintercept = 30, linetype = 'dashed') +
  labs(x = 'RP BF (2023)',
       title = 'Distribution of 2023 BF (Blue) Compared to the\nDistribution of Predicted BF (Red) for RP',
       subtitle = 'Predicting on Untrained Data',
       caption = "Figure 7"
  ) +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))


test_rp %>% 
  mutate(abs_error = abs(playing_time_2023 - preds)) %>% 
  ggplot(aes(playing_time_2021, playing_time_2022, color = abs_error)) +
  geom_jitter(position = position_jitter(width = 10, height = 10, seed = 101))+
  scale_color_continuous(low = 'green',high = 'red', name = 'Absolute\nError') +
  theme_bw()


test_rp %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (RP)",
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)')+
  #subtitle = 'Predicting on Untrained Data')+
  scale_y_continuous(breaks = seq(-500, 500, by = 100)) +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No'))) # p = 0.25

test_rp %>% 
  filter(rookie_year_2021 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2022 = ifelse(rookie_year_2022 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2022), diff, group = rookie_year_2022, fill = as.factor(rookie_year_2022))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2022",
       x = "Rookie Year 2022 (RP)",
       y = 'Prediction Error (BF - Predicted BF)',
       title = 'Prediction Error on 2022 Rookies vs Rest of Data\n(Excluding 2021 Rookies)',
       subtitle = 'Predicting on Untrained Data',
       caption = 'Figure 8')+
  scale_y_continuous(breaks = seq(-500, 500, by = 100),
                     limits = c(-500,500)) +
  ggsignif::geom_signif(comparisons = list(c('Yes', 'No')), annotations = 'p = 0.25') + # p = 0.25
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0)) +
  guides(fill = 'none')

test_rp %>% 
  filter(rookie_year_2022 != 1) %>% 
  mutate(diff = playing_time_2023 - preds,
         rookie_year_2021 = ifelse(rookie_year_2021 == 1, 'Yes',"No")) %>%
  ggplot(aes(as.factor(rookie_year_2021), diff, group = rookie_year_2021, fill = as.factor(rookie_year_2021))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Rookie Year 2021",
       x = "Rookie Year 2021 (RP)",
       title = 'Prediction Error on 2021 Rookies vs Rest of Data\n(Excluding 2022 Rookies)')+
  #subtitle = 'Predicting on Untrained Data') +
  scale_y_continuous(breaks = seq(-500, 500, by = 100))

#### Final Model ####
batters_faced_xg_rp <- batters_faced %>% 
  filter(!(playing_time_2023 == 0 & playing_time_2022 == 0)) %>% # assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2021 == 0 & playing_time_2022 == 0)) %>% # removing 2023 rookies/players with no playing time in 2021 or 2022
  filter(role_key_2023 == 'RP') %>% 
  reframe(
    playing_time_2024 = playing_time_2023, #renaming for predictions
    playing_time_2022,
    playing_time_2021,
    RV_2022 = ifelse(is.na(RV_2022), 0, RV_2022),
    RV_2021 = ifelse(is.na(RV_2021), 0, RV_2021),
    RV100_2022 = ifelse(is.na(RV100_2022), quantile(RV100_2022, probs = 0.1, na.rm = TRUE), RV100_2022),
    RV100_2021 = ifelse(is.na(RV100_2021), quantile(RV100_2021, probs = 0.1, na.rm = TRUE), RV100_2021),
    debut_month, 
    age_24 = age_23, #renaming for predictions
    years_since_debut_23,
    bf_second_half = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_second_half_2022*weight_2022_2rp  + bf_second_half_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_second_half_2022*weight_2022_3rp + bf_second_half_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_second_half_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_second_half_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_second_half_2022*weight_2022_5rp,
      years_since_debut_23 == 1  & debut_month > 8  ~ bf_second_half_2022*weight_2022_6rp,
      .default = bf_second_half_2022*weight_2022_1rp + bf_second_half_2021*weight_2021_1rp
    ),
    wOBAA_2021 = ifelse(is.na(wOBAA_2021), quantile(wOBAA_2021, probs = 0.9, na.rm = TRUE), wOBAA_2021),
    wOBAA_2022 = ifelse(is.na(wOBAA_2022), quantile(wOBAA_2022, probs = 0.9, na.rm = TRUE), wOBAA_2022),
    bf_diff = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ bf_diff_2022*weight_2022_2rp  + bf_diff_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ bf_diff_2022*weight_2022_3rp + bf_diff_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ bf_diff_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ bf_diff_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ bf_diff_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ bf_diff_2022*weight_2022_6rp,
      .default = bf_diff_2022*weight_2022_1rp + bf_diff_2021*weight_2021_1rp
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
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ pitches_thrown_2022*weight_2022_2rp  + pitches_thrown_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ pitches_thrown_2022*weight_2022_3rp + pitches_thrown_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ pitches_thrown_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ pitches_thrown_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ pitches_thrown_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ pitches_thrown_2022*weight_2022_6rp,
      .default = pitches_thrown_2022*weight_2022_1rp + pitches_thrown_2021*weight_2021_1rp
    ),
    ip_2021 = ifelse(is.na(ip_2021), 0, ip_2021),
    ip_2022 = ifelse(is.na(ip_2022), 0, ip_2022),
    
    fip_2021 = ifelse(is.na(fip_2021),quantile(fip_2021, probs = 0.9, na.rm = TRUE),fip_2021),
    fip_2022 = ifelse(is.na(fip_2022),quantile(fip_2022, probs = 0.9, na.rm = TRUE),fip_2022),
    so = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ so_2022*weight_2022_2rp  + so_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ so_2022*weight_2022_3rp + so_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ so_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ so_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ so_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ so_2022*weight_2022_6rp,
      .default = so_2022*weight_2022_1rp + so_2021*weight_2021_1rp
    ),
    kpct_2022 = so_2022/playing_time_2022,
    kpct_2021 = so_2021/playing_time_2021,
    HRA = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ HRA_2022*weight_2022_2rp  + HRA_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ HRA_2022*weight_2022_3rp + HRA_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ HRA_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ HRA_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ HRA_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ HRA_2022*weight_2022_6rp,
      .default = HRA_2022*weight_2022_1rp + HRA_2021*weight_2021_1rp
    ),
    walks_hbp = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ walks_hbp_2022*weight_2022_2rp  + walks_hbp_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ walks_hbp_2022*weight_2022_3rp + walks_hbp_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ walks_hbp_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ walks_hbp_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ walks_hbp_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ walks_hbp_2022*weight_2022_6rp,
      .default = walks_hbp_2022*weight_2022_1rp + walks_hbp_2021*weight_2021_1rp
    ),
    total_basesa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ total_basesa_2022*weight_2022_2rp  + total_basesa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ total_basesa_2022*weight_2022_3rp + total_basesa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ total_basesa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ total_basesa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ total_basesa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ total_basesa_2022*weight_2022_6rp,
      .default = total_basesa_2022*weight_2022_1rp + total_basesa_2021*weight_2021_1rp
    ),
    barrelsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrelsa_2022*weight_2022_2rp  + barrelsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrelsa_2022*weight_2022_3rp + barrelsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ barrelsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ barrelsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ barrelsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ barrelsa_2022*weight_2022_6rp,
      .default = barrelsa_2022*weight_2022_1rp + barrelsa_2021*weight_2021_1rp
    ),
    hardhitsa = case_when(
      playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhitsa_2022*weight_2022_2rp  + hardhitsa_2021*weight_2021_2rp, 
      playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhitsa_2022*weight_2022_3rp + hardhitsa_2021*weight_2021_3rp,
      years_since_debut_23 == 1  & debut_month <= 4 ~ hardhitsa_2022,
      years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ hardhitsa_2022*weight_2022_4rp,
      years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ hardhitsa_2022*weight_2022_5rp,
      years_since_debut_23 == 1  &  debut_month > 8  ~ hardhitsa_2022*weight_2022_6rp,
      .default = hardhitsa_2022*weight_2022_1rp + hardhitsa_2021*weight_2021_1rp
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
    
    cluster_2024_rp = as.factor(cluster_2023_rp), #renaming for predictions
    
    avg_ev_2021 = ifelse(is.na(avg_ev_2021), quantile(avg_ev_2021, probs = 0.1, na.rm = TRUE), avg_ev_2021),
    avg_ev_2022 = ifelse(is.na(avg_ev_2022), quantile(avg_ev_2022, probs = 0.1, na.rm = TRUE), avg_ev_2022),
    
    percentile_90_ev_2021 = ifelse(is.na(percentile_90_ev_2021), quantile(percentile_90_ev_2021, probs = 0.1, na.rm = TRUE), percentile_90_ev_2021),
    percentile_90_ev_2022 = ifelse(is.na(percentile_90_ev_2022), quantile(percentile_90_ev_2022, probs = 0.1, na.rm = TRUE), percentile_90_ev_2022),
  ) %>% 
  mutate(rookie_year_2023 = ifelse(years_since_debut_23 == 1, 1, 0),
         debut_month_2023 = ifelse(rookie_year_2023 == 1, debut_month, 0),
         rookie_year_2022 = ifelse(years_since_debut_23 == 2, 1, 0),
         debut_month_2022 = ifelse(rookie_year_2022 == 1, debut_month, 0),
         kpct_2022 = ifelse(is.na(kpct_2022), quantile(kpct_2022, probs = 0.1, na.rm =TRUE), kpct_2022),
         kpct_2021 = ifelse(is.na(kpct_2021), quantile(kpct_2021, probs = 0.1, na.rm =TRUE), kpct_2021),
         kpct = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ kpct_2022*weight_2022_2rp  + kpct_2021*weight_2021_2rp,
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ kpct_2022*weight_2022_3rp + kpct_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ kpct_2022,
           .default = kpct_2022*weight_2022_1rp + kpct_2021*weight_2021_1rp),
         earned_runs = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ earned_runs_2022*weight_2022_2rp  + earned_runs_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ earned_runs_2022*weight_2022_3rp + earned_runs_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ earned_runs_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ earned_runs_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ earned_runs_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ earned_runs_2022*weight_2022_6rp,
           .default = earned_runs_2022*weight_2022_1rp + earned_runs_2021*weight_2021_1rp
         ),
         RV = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV_2022*weight_2022_2rp  + RV_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV_2022*weight_2022_3rp + RV_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ RV_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ RV_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ RV_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ RV_2022*weight_2022_6rp,
           .default = RV_2022*weight_2022_1rp + RV_2021*weight_2021_1rp
         ),
         RV100 = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ RV100_2022*weight_2022_2rp  + RV100_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ RV100_2022*weight_2022_3rp + RV100_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ RV100_2022,
           .default = RV100_2022*weight_2022_1rp + RV100_2021*weight_2021_1rp),
         ip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ip_2022*weight_2022_2rp  + ip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ip_2022*weight_2022_3rp + ip_2021*weight_2021_3rp,
           years_since_debut_23 == 1  & debut_month <= 4 ~ ip_2022,
           years_since_debut_23 == 1  & debut_month <= 6 & debut_month > 4 ~ ip_2022*weight_2022_4rp,
           years_since_debut_23 == 1  & debut_month <= 8 & debut_month > 6  ~ ip_2022*weight_2022_5rp,
           years_since_debut_23 == 1  &  debut_month > 8  ~ ip_2022*weight_2022_6rp,
           .default = ip_2022*weight_2022_1rp + ip_2021*weight_2021_1rp
         ),
         wOBAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ wOBAA_2022*weight_2022_2rp  + wOBAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ wOBAA_2022*weight_2022_3rp + wOBAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ wOBAA_2022,
           .default = wOBAA_2022*weight_2022_1rp + wOBAA_2021*weight_2021_1rp),
         
         times_faced = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ times_faced_2022*weight_2022_2rp  + times_faced_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ times_faced_2022*weight_2022_3rp + times_faced_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ times_faced_2022,
           .default = times_faced_2022*weight_2022_1rp + times_faced_2021*weight_2021_1rp),
         
         avg_pitches_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_pitches_per_appearance_2022*weight_2022_2rp  + avg_pitches_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_pitches_per_appearance_2022*weight_2022_3rp + avg_pitches_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_pitches_per_appearance_2022,
           .default = avg_pitches_per_appearance_2022*weight_2022_1rp + avg_pitches_per_appearance_2021*weight_2021_1rp),
         
         avg_bf_per_appearance = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_bf_per_appearance_2022*weight_2022_2rp  + avg_bf_per_appearance_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_bf_per_appearance_2022*weight_2022_3rp + avg_bf_per_appearance_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_bf_per_appearance_2022,
           .default = avg_bf_per_appearance_2022*weight_2022_1rp + avg_bf_per_appearance_2021*weight_2021_1rp),
         
         ERA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ERA_2022*weight_2022_2rp  + ERA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ERA_2022*weight_2022_3rp + ERA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ERA_2022,
           .default = ERA_2022*weight_2022_1rp + ERA_2021*weight_2021_1rp),
         
         fip = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ fip_2022*weight_2022_2rp  + fip_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ fip_2022*weight_2022_3rp + fip_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ fip_2022,
           .default = fip_2022*weight_2022_1rp + fip_2021*weight_2021_1rp),
         
         hardhit_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hardhit_pcta_2022*weight_2022_2rp  + hardhit_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hardhit_pcta_2022*weight_2022_3rp + hardhit_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hardhit_pcta_2022,
           .default = hardhit_pcta_2022*weight_2022_1rp + hardhit_pcta_2021*weight_2021_1rp),
         
         barrel_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ barrel_pcta_2022*weight_2022_2rp  + barrel_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ barrel_pcta_2022*weight_2022_3rp + barrel_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ barrel_pcta_2022,
           .default = barrel_pcta_2022*weight_2022_1rp + barrel_pcta_2021*weight_2021_1rp),
         
         ChaseRateA  = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ ChaseRateA_2022*weight_2022_2rp  + ChaseRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ ChaseRateA_2022*weight_2022_3rp + ChaseRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ ChaseRateA_2022,
           .default = ChaseRateA_2022*weight_2022_1rp + ChaseRateA_2021*weight_2021_1rp),
         
         WhiffRateA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ WhiffRateA_2022*weight_2022_2rp  + WhiffRateA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ WhiffRateA_2022*weight_2022_3rp + WhiffRateA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ WhiffRateA_2022,
           .default = WhiffRateA_2022*weight_2022_1rp + WhiffRateA_2021*weight_2021_1rp),
         
         SLGA =  case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ SLGA_2022*weight_2022_2rp  + SLGA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ SLGA_2022*weight_2022_3rp + SLGA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ SLGA_2022,
           .default = SLGA_2022*weight_2022_1rp + SLGA_2021*weight_2021_1rp),
         
         BAA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ BAA_2022*weight_2022_2rp  + BAA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ BAA_2022*weight_2022_3rp + BAA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ BAA_2022,
           .default = BAA_2022*weight_2022_1rp + BAA_2021*weight_2021_1rp),
         
         OBPA = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ OBPA_2022*weight_2022_2rp  + OBPA_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ OBPA_2022*weight_2022_3rp + OBPA_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ OBPA_2022,
           .default = OBPA_2022*weight_2022_1rp + OBPA_2021*weight_2021_1rp),
         
         hr_pcta = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ hr_pcta_2022*weight_2022_2rp  + hr_pcta_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ hr_pcta_2022*weight_2022_3rp + hr_pcta_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ hr_pcta_2022,
           .default = hr_pcta_2022*weight_2022_1rp + hr_pcta_2021*weight_2021_1rp),
         
         gball_rate = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ gball_rate_2022*weight_2022_2rp  + gball_rate_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ gball_rate_2022*weight_2022_3rp + gball_rate_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ gball_rate_2022,
           .default = gball_rate_2022*weight_2022_1rp + gball_rate_2021*weight_2021_1rp),
         
         avg_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ avg_ev_2022*weight_2022_2rp  + avg_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ avg_ev_2022*weight_2022_3rp + avg_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ avg_ev_2022,
           .default = avg_ev_2022*weight_2022_1rp + avg_ev_2021*weight_2021_1rp),
         
         percentile_90_ev = case_when(
           playing_time_2021 > 150 & playing_time_2022 < 50 ~ percentile_90_ev_2022*weight_2022_2rp  + percentile_90_ev_2021*weight_2021_2rp, 
           playing_time_2021 < 50 & playing_time_2022 > 150 & years_since_debut_23 != 1 ~ percentile_90_ev_2022*weight_2022_3rp + percentile_90_ev_2021*weight_2021_3rp,
           playing_time_2021 == 0 & playing_time_2022 > 0 & years_since_debut_23 == 1 ~ percentile_90_ev_2022,
           .default = percentile_90_ev_2022*weight_2022_1rp + percentile_90_ev_2021*weight_2021_1rp)
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

batters_faced_xg_rp <- batters_faced_xg_rp %>% 
  dummy_cols(select_columns = c('cluster_2024_rp'), remove_selected_columns = TRUE)

glimpse(batters_faced_xg_rp)

dtrain_rp <- xgb.DMatrix(as.matrix(batters_faced_xg_rp %>% select(-playing_time_2024)), label = batters_faced_xg_rp$playing_time_2024)

set.seed(101);rp_model <- xgboost(
  params = list(
    eta = eta_rp,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = 1,
    lambda = lambda_rp,
    alpha = alpha_rp,
    max_depth = max_depth_rp,
    min_child_weight = weight_rp,
    subsample = subsample_rp,
    colsample_bytree = by_tree_rp,
    colsample_bylevel = by_level_rp,
    colsample_bynode = by_node_rp,
    tree_method = tree_method_rp,
    grow_policy = grow_policy_rp
  ),
  data = dtrain_rp,
  print_every_n = 100,
  nrounds = nrounds_rp,
  nthread = 7
) 


save(rp_model, file = 'BF RP Model_edited_updated.RData')
