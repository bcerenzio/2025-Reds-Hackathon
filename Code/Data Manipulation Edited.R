#install.packages("ClusterR")
library(tidyverse)
library(ClusterR)

#### PA and BF ####
plate_appearances <- 
  # start with the savant data
  statcast %>%
  # we will group by batter, season, game, and at bat and preserve the 
  group_by(
    batter,
    game_year,
    game_pk,
    at_bat_number
  ) %>%
  summarise() %>%
  ungroup() %>%
  # now we have just unique batter, season, game, and at bat observations
  # but, we need to count how many of those there are each season
  # so, we will do another group by and summarise
  group_by(
    batter,
    game_year
  ) %>%
  summarise(
    # the n() function counts the number of unique observations we have
    playing_time = n()
  ) %>%
  ungroup()


batters_faced <- 
  statcast %>%
  group_by(
    pitcher,
    game_year,
    game_pk,
    at_bat_number
  ) %>%
  summarise() %>%
  ungroup() %>%
  group_by(
    pitcher,
    game_year
  ) %>%
  summarise(
    playing_time = n()
  ) %>%
  ungroup()

# density plots with pa for each years
plate_appearances %>% bind_rows(batters_faced) %>% 
  ggplot(aes(playing_time, group = game_year, fill = as.factor(game_year))) +
  geom_density(alpha = 0.7) +
  labs(fill = 'Year',
       x = 'PA/BF',
       title = 'Distribution of PA/BF By Year',
       caption = 'Figure 1') +
  theme_classic() +
  theme(plot.caption = element_text(size = 11, face = 'italic', hjust = 0))

### Poisson Distribution

#### Pivot Wider command to give each year their own column ####
plate_appearances <- plate_appearances %>%
  pivot_wider(names_from = game_year, names_prefix = 'playing_time_',values_from = playing_time) %>%
  group_by(batter) %>% 
  reframe(playing_time_2021 = sum(playing_time_2021, na.rm = TRUE),
          playing_time_2022 = sum(playing_time_2022, na.rm = TRUE),
          playing_time_2023 = sum(playing_time_2023, na.rm = TRUE))

batters_faced <- batters_faced %>%
  pivot_wider(names_from = game_year, names_prefix = 'playing_time_',values_from = playing_time) %>%
  group_by(pitcher) %>% 
  reframe(playing_time_2021 = sum(playing_time_2021, na.rm = TRUE),
          playing_time_2022 = sum(playing_time_2022, na.rm = TRUE),
          playing_time_2023 = sum(playing_time_2023, na.rm = TRUE))

# removing pitchers from plate_appearances (except "Ohtani") ####
plate_appearances <- plate_appearances %>% 
  mutate(pitcher = ifelse(batter %in% unique(batters_faced$pitcher) & batter != '18396fcf5f98aac97ec6127f7924868d3ef7bd9e' &
                            playing_time_2022 < 10 & playing_time_2023 < 10, 1,0)) %>% 
  filter(pitcher != 1) %>% 
  select(-pitcher)

#removing batters from batters_faced (except "Ohtani") ####
batters_faced <- batters_faced %>% 
  mutate(
    batter = ifelse(pitcher %in% unique(plate_appearances$batter) & pitcher != '18396fcf5f98aac97ec6127f7924868d3ef7bd9e' &
                      (playing_time_2021 + playing_time_2022 + playing_time_2023) < 50, 1, 0)) %>% 
  filter(batter != 1) %>% 
  select(-batter)


#finding Seasonal batter run value for each batter ####
batter_run_value <- statcast %>% 
  group_by(batter,game_year) %>% 
  #reframe(RV100 = (sum(delta_run_exp, na.rm = TRUE)/n())*100) %>%
  reframe(RV = sum(delta_run_exp, na.rm = TRUE)) %>%
  pivot_wider(names_from = game_year, names_prefix = 'RV_', values_from = RV) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

plate_appearances <- plate_appearances %>% 
  left_join(batter_run_value, by = 'batter')

remove(batter_run_value)

#finding Seasonal pitcher run value####
pitcher_run_value <- statcast %>% 
  group_by(pitcher,game_year) %>% 
  reframe(RV = sum(-delta_run_exp, na.rm = TRUE),
          RV100 = sum(-delta_run_exp, na.rm = TRUE)/n()*100) %>% #-100 makes positive values good for pitchers
  pivot_wider(names_from = game_year, values_from = c(RV, RV100))

batters_faced <- batters_faced %>% 
  left_join(pitcher_run_value, by = 'pitcher')

remove(pitcher_run_value)

#adding sp or rp distinctions to batters_faced ####
sp_rp <- statcast %>% 
  select(pitcher, role_key, game_pk, game_year) %>% 
  distinct() %>% 
  group_by(pitcher, role_key, game_year) %>% 
  reframe(freq = n())

sp_rp <- sp_rp %>% 
  pivot_wider(names_from = game_year, values_from = freq, names_prefix = 'freq_') %>% 
  mutate(across(everything(), ~replace_na(., 0)))

sp_rp <- sp_rp %>% 
  mutate(weighted_freq_2023 = freq_2021*0.15+freq_2022*0.85,
         weighted_freq_2024 = freq_2022*0.15+freq_2023*0.85)

# getting 2023 weighted means
sp_rp_2023 <- sp_rp %>% 
  select(pitcher, role_key,weighted_freq_2023) %>% 
  group_by(pitcher) %>% 
  slice_max(weighted_freq_2023, n = 1) %>% 
  ungroup()

nrow(batters_faced) == nrow(sp_rp_2023) #FALSE

#filtering for where pitchers are both sp and rp
sp_rp_2023_ties <- sp_rp_2023 %>% 
  group_by(pitcher) %>% 
  filter(n_distinct(role_key) == 2) %>% 
  ungroup()

sp_rp_2023_ties <- sp_rp_2023_ties %>% 
  mutate(role_key = ifelse(weighted_freq_2023 == 0, 'RP', 'SP')) %>% 
  distinct()

#joining both tables together (while removing rows from ties table
# from original data)
sp_rp_2023 <- sp_rp_2023 %>% 
  group_by(pitcher) %>% 
  filter(n_distinct(role_key) == 1) %>% 
  ungroup() %>% 
  bind_rows(sp_rp_2023_ties) %>% 
  filter(pitcher %in% batters_faced$pitcher)

nrow(batters_faced) == nrow(sp_rp_2023) #TRUE

# following same process for sp_rp_2024 #
# getting 2024 weighted means
sp_rp_2024 <- sp_rp %>% 
  select(pitcher, role_key,weighted_freq_2024) %>% 
  group_by(pitcher) %>% 
  slice_max(weighted_freq_2024, n = 1) %>% 
  ungroup()

nrow(batters_faced) == nrow(sp_rp_2024) #FALSE

#filtering for where pitchers are both sp and rp
sp_rp_2024_ties <- sp_rp_2024 %>% 
  group_by(pitcher) %>% 
  filter(n_distinct(role_key) == 2) %>% 
  ungroup()

sp_rp_2024_ties <- sp_rp_2024_ties %>% 
  mutate(role_key = ifelse(weighted_freq_2024 == 0, 'RP', 'SP')) %>% 
  distinct()

#joining both tables together (while removing rows from ties table
# from original data)
sp_rp_2024 <- sp_rp_2024 %>% 
  group_by(pitcher) %>% 
  filter(n_distinct(role_key) == 1) %>% 
  ungroup() %>% 
  bind_rows(sp_rp_2024_ties) %>% 
  filter(pitcher %in% batters_faced$pitcher)

nrow(batters_faced) == nrow(sp_rp_2024) #TRUE


batters_faced <- batters_faced %>% 
  left_join(sp_rp_2023 %>% reframe(pitcher, role_key_2023 = role_key), by =  'pitcher') %>% 
  left_join(sp_rp_2024 %>% reframe(pitcher, role_key_2024 = role_key), by = 'pitcher')

batters_faced %>% ggplot(aes(role_key_2023,playing_time_2023)) +
  geom_boxplot() +
  ylab('Batters Faced (2023)') +
  xlab('') +
  theme_bw()

remove(sp_rp)
### batting games started, prop of games started & primary position ####
#finding total games started
batting_games_started <- statcast %>% 
  filter(inning <= 3)%>% 
  group_by(game_pk, inning_topbot,game_year) %>%
  distinct(batter) %>% 
  ungroup() %>% 
  arrange(game_pk, inning_topbot) %>% 
  group_by(batter, game_year) %>% 
  reframe(games_started = n()) %>% 
  ungroup()

batting_games_started_year <- batting_games_started %>% 
  pivot_wider(names_from = game_year, values_from = games_started, names_prefix = 'games_started_') %>% 
  mutate(across(starts_with('games_started_'), ~replace_na(., 0)))

batting_games_started <- batting_games_started %>% 
  group_by(batter) %>% 
  reframe(games_started = sum(games_started, na.rm = TRUE))

positions_df <- tibble();for (position in 2:9){
  print(position)
  # Hard-coding in. The group_by is giving the for loop a hard time
  if (position == 2){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_2) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_2, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_2') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 3){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_3) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_3, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_3') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 4){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_4) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_4, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_4') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 5){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_5) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_5, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_5') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 6){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_6) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_6, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_6') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 7){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_7) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_7, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_7') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 8){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_8) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_8, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_8') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  } else if (position == 9){
    df <- statcast %>% 
      filter(inning == 1) %>% 
      group_by(game_pk, inning_topbot, game_year) %>%
      distinct(fielder_9) %>% 
      ungroup() %>% 
      arrange(game_pk, inning_topbot, game_year) %>% 
      group_by(fielder_9, game_year) %>% 
      reframe(
        pos = position,
        games_started = n()) %>%  
      arrange(desc(games_started)) %>% 
      rename('player_id' = 'fielder_9') %>% 
      ungroup()
    positions_df <- bind_rows(positions_df, df)
  }
};remove(df)

primary_positions_df <- positions_df %>% 
  group_by(player_id, pos) %>% 
  reframe(games_started = sum(games_started, na.rm = TRUE)) %>% 
  group_by(player_id) %>% 
  slice_max(games_started, n = 1) %>% 
  slice(1) %>% 
  rename('games_started_field' = 'games_started') %>% 
  ungroup()

positions_df <- positions_df %>% 
  group_by(player_id) %>% 
  reframe(games_started = sum(games_started, na.rm = TRUE)) %>% 
  rename('games_started_field' = 'games_started')



batting_games_started <- batting_games_started %>% 
  left_join(positions_df, by = c('batter' = 'player_id'))

#finding possible DH's
batting_games_started <- batting_games_started %>% 
  mutate(field_start_prop = ifelse(is.na(games_started_field),0,games_started_field)/games_started)

batting_games_started <- batting_games_started %>% 
  mutate(dh = ifelse(field_start_prop <= 0.4 & games_started >= 30, 1,0),
         other = ifelse(field_start_prop <= 0.4 & games_started < 30, 1,0))

primary_positions_df <- primary_positions_df %>% 
  full_join(batting_games_started %>% select(batter, dh, other), by = c('player_id' = 'batter')) %>% 
  mutate(pos = ifelse(dh == 1, 10, pos),
         pos = ifelse(other == 1, 11,pos))



plate_appearances <- plate_appearances %>% 
  left_join(primary_positions_df %>% select('player_id','pos'), by = c('batter' = 'player_id'))

plate_appearances <- plate_appearances %>% 
  mutate(pos = ifelse(is.na(pos), 11, pos),
         pos = as.factor(pos))

plate_appearances %>% 
  pivot_longer(cols = c('playing_time_2021', 'playing_time_2022','playing_time_2023'), values_to = 'PA', names_to = 'year') %>% 
  mutate(pos = case_when(
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
  ),
  pos = fct_relevel(pos, c('C', '1B', '2B', '3B', 'SS', 'LF', 'CF', 'RF', 'DH', 'Unknown'))) %>% 
  ggplot(aes(pos, PA, fill = pos)) +
  geom_boxplot() +
  guides(fill = 'none') +
  xlab('Primary Position') +
  ylab('PA (2021-2023)') +
  ggtitle('PA (2021-2023) By Primary Position') +
  theme_bw()

ggsave('Batter Position vs pa.png', width = 3.98*2, height = 3.99*2)

#### Debut and Age ####
#adding age to lahman database (as of 04/01/2024)
lahman <- lahman %>% 
  mutate(age_24 = ifelse(birthMonth < 4, 2024-birthYear, 2023-birthYear),
         age_23 = ifelse(birthMonth < 4, 2023-birthYear, 2022-birthYear))

#adding debut_year and years since debut
lahman <- lahman %>% 
  mutate(debut_year = year(debut),
         years_since_debut_24 = 2024 - debut_year,
         years_since_debut_23 = 2023 - debut_year)

#adding age and service time to plate_appearances & batters_faced
plate_appearances <- plate_appearances %>% 
  left_join(lahman %>% 
              select(player_mlb_id, age_23, age_24,
                     years_since_debut_23, 
                     years_since_debut_24), by = c('batter' = 'player_mlb_id'))

batters_faced <- batters_faced %>% 
  left_join(lahman %>% 
              select(player_mlb_id, age_23, age_24,
                     years_since_debut_23, 
                     years_since_debut_24), by = c('pitcher' = 'player_mlb_id'))

lahman <- lahman %>% 
  mutate(debut_month = month(debut))

plate_appearances <- plate_appearances %>% 
  left_join(lahman %>% 
              select(player_mlb_id, debut_month), by = c('batter' = 'player_mlb_id'))

batters_faced <- batters_faced %>% 
  left_join(lahman %>% 
              select(player_mlb_id, debut_month), by = c('pitcher' = 'player_mlb_id'))



### Fielding Run Value ####
unique(statcast$events)

fielding <- statcast %>% 
  filter(!is.na(woba_value), 
         events %in% c('caught_stealing_3rd','field_out','force_out','sac_fly',
                       'single','double','grounded_into_double_play','sac_bunt',
                       'fielders_choice','field_error','other_out',
                       'caught_stealing_2nd','triple','fielders_choice_out',
                       'double_play','sac_fly_double_play','catcher_interf',
                       'sac_bunt_double_play','wild_pitch','stolen_base_2b',
                       'passed_ball','stolen_base_3b')) %>% 
  reframe(fielder_id = case_when(
    hit_location == 2 ~ fielder_2,
    hit_location == 3 ~ fielder_3,
    hit_location == 4 ~ fielder_4,
    hit_location == 5 ~ fielder_5,
    hit_location == 6 ~ fielder_6,
    hit_location == 7 ~ fielder_7,
    hit_location == 8 ~ fielder_8,
    hit_location == 9 ~ fielder_9
  ), game_year,
  woba_value, woba_denom,hit_location, events, 
  delta_run_exp = -delta_run_exp, out_prob = 1-estimated_ba_using_speedangle) %>% 
  filter(hit_location != 1)

fielding <- subset(fielding, fielding$events != "wild_pitch")
fielding <- subset(fielding, fielding$events != "stolen_base_2b")
fielding <- subset(fielding, fielding$events != "stolen_base_3b")
fielding <- fielding[!is.na(fielding$out_prob), ]


fielding$out_prob <- ifelse(fielding$events == "catcher_interf", 1, fielding$out_prob)

fielding$fielding_run_value <- ifelse(fielding$events %in% c("field_out", "force_out", "grounded_into_double_play", 
                                                             "sac_fly", "sac_bunt", "fielders_choice", "fielders_choice_out",
                                                             "other_out", "double_play", "sac_fly_double_play", "sac_bunt_double_play"),
                                      (fielding$delta_run_exp*(1-fielding$out_prob)),
                                      fielding$delta_run_exp * fielding$out_prob
)
fielding$fielding_run_value <- fielding$fielding_run_value - mean(fielding$fielding_run_value, na.rm = TRUE)
unique(fielding$events)

frv <- fielding %>%
  group_by(fielder_id, game_year) %>%
  summarise(fielding_run_value = sum(fielding_run_value, na.rm = TRUE)) %>%
  ungroup()
summary(frv)

frv <- frv %>%
  pivot_wider(names_from = game_year, values_from = fielding_run_value, names_prefix = "fielding_run_value_")%>% 
  mutate(across(everything(), ~replace_na(., 0)))

plate_appearances <- plate_appearances %>% 
  left_join(frv, by = c('batter' = 'fielder_id'))

# imputing 0s for players who don't show up at all in the frv dataset
plate_appearances <- plate_appearances %>% 
  mutate(across(starts_with('fielding_run_value_'), ~replace_na(., 0)))

# Framing #
framing <- subset(statcast, statcast$description == "blocked_ball" | statcast$description == "called_strike" | statcast$description == "ball")

framing <- framing %>%
  select(game_year, fielder_2, description, zone, delta_run_exp)

framing$is_ball <- ifelse(framing$zone > 9, "yes", "no")

framing$delta_run_exp <- -framing$delta_run_exp

wrong_balls <- subset(framing, framing$description == "ball" & framing$is_ball == "no")
wrong_strikes <- subset(framing, framing$description == "called_strike" & framing$is_ball == "yes")

framing$framing_run_value <- case_when(framing$description == "ball" & framing$is_ball == "no" ~ framing$delta_run_exp,
                                       framing$description == "called_strike" & framing$is_ball == "yes" ~ framing$delta_run_exp,
                                       .default = 0
)

framing_frv <- framing %>%
  group_by(fielder_2, game_year) %>%
  summarise(total_framing_run_value = sum(framing_run_value, na.rm = TRUE)) %>% 
  mutate(total_framing_run_value = total_framing_run_value - mean(total_framing_run_value, na.rm = TRUE) # centering mean at 0
  )

framing_frv <- framing_frv %>%
  pivot_wider(names_from = game_year, values_from = total_framing_run_value, names_prefix = "framing_run_value_")

plate_appearances <- plate_appearances %>% 
  left_join(framing_frv, by = c('batter' = 'fielder_2')) %>% 
  rowwise() %>% 
  mutate(fielding_run_value_2021 = sum(fielding_run_value_2021, framing_run_value_2021,na.rm = TRUE),
         fielding_run_value_2022 = sum(fielding_run_value_2022, framing_run_value_2022,na.rm = TRUE),
         fielding_run_value_2023 = sum(fielding_run_value_2023, framing_run_value_2023,na.rm = TRUE)
  ) %>% 
  select(-starts_with('total_framing_')) %>% 
  ungroup()

remove(framing_frv, framing, wrong_balls, wrong_strikes,
       frv, fielding)

# 9th inning appearances ####
players <- statcast %>%
  group_by(pitcher, game_year, game_date) %>%
  summarize(inn9 = any(inning == 9), .groups = 'drop') %>%
  group_by(pitcher, game_year) %>%
  summarize(inn9apps = sum(inn9), .groups = 'drop')

#View(players)

players_wide <- players %>%
  pivot_wider(names_from = game_year, values_from = inn9apps, names_prefix = "inn9apps_") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

batters_faced <- batters_faced %>% 
  left_join(players_wide, by = 'pitcher')

batters_faced %>% 
  filter(role_key_2023 == 'RP', role_key_2024 == 'RP',years_since_debut_23 > 1) %>% 
  ggplot(aes(inn9apps_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(y = 'BF 2023',
       x = '9th Inning Appearances (or later) 2022',
       title = 'BF 2023 vs 9th Inning Appearances (or later) 2022')

ggsave('9thinningapps.png', width = 3.98*2, height = 3.99*2)

# Lineup Order Position ####
# overall #
lineup_pos <- statcast %>% 
  group_by(game_pk) %>% 
  arrange(inning_topbot, at_bat_number) %>% 
  ungroup() %>% 
  select(inning_topbot, inning, at_bat_number, batter, game_pk,game_year) %>% 
  group_by(inning_topbot, game_pk,game_year) %>% 
  distinct(at_bat_number, batter) %>% 
  mutate(lineup_pos = row_number(),
         lineup_pos = ifelse(lineup_pos > 9, NA, lineup_pos),
         batter = batter) %>% 
  ungroup()


lineup_pos_avg <- lineup_pos %>% 
  group_by(batter) %>% 
  reframe(lineup_pos_avg = mean(lineup_pos, na.rm = TRUE)) %>% 
  mutate(lineup_pos_avg = ifelse(is.na(lineup_pos_avg), 10, lineup_pos_avg))

plate_appearances <- plate_appearances %>% 
  left_join(lineup_pos_avg, by = 'batter')

# yearly #
lineup_pos <- statcast %>% 
  group_by(game_pk,game_year) %>% 
  arrange(inning_topbot, at_bat_number) %>% 
  ungroup() %>% 
  select(inning_topbot, inning, at_bat_number, batter, game_pk,game_year) %>% 
  group_by(inning_topbot, game_pk,game_year) %>% 
  distinct(at_bat_number, batter) %>% 
  mutate(lineup_pos = row_number(),
         lineup_pos = ifelse(lineup_pos > 9, NA, lineup_pos),
         batter = batter) %>% 
  ungroup()


lineup_pos_avg <- lineup_pos %>% 
  group_by(batter,game_year) %>% 
  reframe(lineup_pos_avg = mean(lineup_pos, na.rm = TRUE)) %>% 
  mutate(lineup_pos_avg = ifelse(is.na(lineup_pos_avg), 10, lineup_pos_avg))

lineup_pos_avg <- lineup_pos_avg %>% 
  pivot_wider(names_from = game_year, values_from = lineup_pos_avg, names_prefix = 'lineup_pos_avg_') %>% 
  mutate(lineup_pos_avg_2021 = ifelse(is.na(lineup_pos_avg_2021),10,lineup_pos_avg_2021),
         lineup_pos_avg_2022 = ifelse(is.na(lineup_pos_avg_2022),10,lineup_pos_avg_2022),
         lineup_pos_avg_2023 = ifelse(is.na(lineup_pos_avg_2023),10,lineup_pos_avg_2023))



plate_appearances <- plate_appearances %>% 
  left_join(lineup_pos_avg, by = 'batter')

remove(lineup_pos_avg, lineup_pos)

lineup_mod <- summary(lm(playing_time_2023 ~ lineup_pos_avg_2022, data = 
                   plate_appearances %>% 
                   filter(years_since_debut_23 != 0, playing_time_2022 != 0)))

plate_appearances %>% 
  filter(years_since_debut_23 != 0, playing_time_2022 != 0) %>% 
  ggplot(aes(lineup_pos_avg_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  theme_bw() +
  labs(x = 'Average Lineup Position (2022)',
       y = 'PA 2023',
       title = 'PA 2023 vs Average Lineup Position (2022)') #+
  # annotate('text', x = 9, y = 700, label = paste('r =', round(corrr::correlate(plate_appearances %>% 
  #                                                                            filter(years_since_debut_23 != 0, playing_time_2022 != 0) %>%
  #                                                                            pull(lineup_pos_avg_2022), plate_appearances %>% 
  #                                                                            filter(years_since_debut_23 != 0, playing_time_2022 != 0) %>%
  #                                                                            pull(playing_time_2023))$x,3))) +
  # annotate('text', x = 9, y = 665, label = paste('p = 2e-16'))

ggsave('lineup pos vs pa 2023.png', width = 3.98*2, height = 3.99*2)

### Monthly PA/BF ####
plate_appearances_monthly <- 
  # start with the savant data
  statcast %>%
  mutate(game_month = month(game_date)) %>% 
  # we will group by batter, season, game, and at bat and preserve the 
  group_by(
    batter,
    game_year,
    game_pk,
    at_bat_number,
    game_month
  ) %>%
  summarise() %>%
  ungroup() %>%
  mutate(game_month = case_when( 
    game_month == 3 ~ 4, # Counting March Games to April
    game_month == 10 ~ 9, # Counting October Games to September
    .default = game_month
  )) %>% 
  # now we have just unique batter, season, game, and at bat observations
  # but, we need to count how many of those there are each season
  # so, we will do another group by and summarise
  group_by(
    batter,
    game_year,
    game_month
  ) %>%
  summarise(
    # the n() function counts the number of unique observations we have
    playing_time = n()
  ) %>%
  ungroup()


plate_appearances_monthly <- plate_appearances_monthly %>% 
  group_by(batter,game_year) %>% 
  mutate(
    total_pa = sum(playing_time),
    playing_time_prop_of_season = playing_time/sum(playing_time)*100) %>% 
  ungroup()

plate_appearances_monthly_prop <- plate_appearances_monthly %>%
  select(-total_pa) %>% 
  arrange(game_year, game_month) %>% 
  pivot_wider(names_from = c(game_month, game_year), values_from = c(playing_time, playing_time_prop_of_season))

remove(plate_appearances_monthly)

plate_appearances_monthly_prop <- plate_appearances_monthly_prop %>% 
  mutate(across(everything(), ~replace_na(.,0)))

plate_appearances <- plate_appearances %>% 
  left_join(plate_appearances_monthly_prop, by = 'batter')
remove(plate_appearances_monthly_prop)

batters_faced_monthly <- 
  # start with the savant data
  statcast %>%
  mutate(game_month = month(game_date)) %>% 
  # we will group by batter, season, game, and at bat and preserve the 
  group_by(
    pitcher,
    game_year,
    game_pk,
    at_bat_number,
    game_month
  ) %>%
  summarise() %>%
  ungroup() %>%
  mutate(game_month = case_when( 
    game_month == 3 ~ 4, # Counting March Games to April
    game_month == 10 ~ 9, # Counting October Games to September
    .default = game_month
  )) %>% 
  # now we have just unique batter, season, game, and at bat observations
  # but, we need to count how many of those there are each season
  # so, we will do another group by and summarise
  group_by(
    pitcher,
    game_year,
    game_month
  ) %>%
  summarise(
    # the n() function counts the number of unique observations we have
    playing_time = n()
  ) %>%
  ungroup()


batters_faced_monthly <- batters_faced_monthly %>% 
  group_by(pitcher,game_year) %>% 
  mutate(
    total_pa = sum(playing_time),
    playing_time_prop_of_season = playing_time/sum(playing_time)*100) %>% 
  ungroup()


batters_faced_monthly_prop <- batters_faced_monthly %>%
  select(-total_pa) %>% 
  arrange(game_year, game_month) %>% 
  pivot_wider(names_from = c(game_month, game_year), values_from = c(playing_time, playing_time_prop_of_season))

remove(batters_faced_monthly)

batters_faced_monthly_prop <- batters_faced_monthly_prop %>% 
  mutate(across(everything(), ~replace_na(.,0)))

batters_faced <- batters_faced %>% 
  left_join(batters_faced_monthly_prop, by = 'pitcher')



remove(batters_faced_monthly_prop)


# Second Half PA/BF ####
second_half_21 <- filter(statcast, game_date >= as.Date("2021-08-01"), game_date < as.Date("2021-10-10"))
second_half_22 <- filter(statcast, game_date >= as.Date("2022-08-01"), game_date < as.Date("2022-10-10"))
second_half_23 <- filter(statcast, game_date >= as.Date("2023-08-01"), game_date < as.Date("2023-10-10"))

sh_pa_21 <- second_half_21 %>% group_by(game_pk) %>% distinct(at_bat_number, batter) %>% mutate(game_year = '2021')
sh_pa_22 <- second_half_22 %>% group_by(game_pk) %>% distinct(at_bat_number, batter) %>% mutate(game_year = '2022')
sh_pa_23 <- second_half_23 %>% group_by(game_pk) %>% distinct(at_bat_number, batter) %>% mutate(game_year = '2023')

sh_bf_21 <- second_half_21 %>% group_by(game_pk) %>% distinct(at_bat_number, pitcher) %>% mutate(game_year = '2021')
sh_bf_22 <- second_half_22 %>% group_by(game_pk) %>% distinct(at_bat_number, pitcher) %>% mutate(game_year = '2022')
sh_bf_23 <- second_half_23 %>% group_by(game_pk) %>% distinct(at_bat_number, pitcher) %>% mutate(game_year = '2023')

pa_21 <- sh_pa_21 %>% group_by(batter) %>% reframe(pa_second_half_2021 = n()) %>% distinct()
pa_22 <- sh_pa_22 %>% group_by(batter) %>% reframe(pa_second_half_2022 = n()) %>% distinct()
pa_23 <- sh_pa_23 %>% group_by(batter) %>% reframe(pa_second_half_2023 = n()) %>% distinct()

bf_21 <- sh_bf_21 %>% group_by(pitcher) %>% reframe(bf_second_half_2021 = n()) %>% distinct()
bf_22 <- sh_bf_22 %>% group_by(pitcher) %>% reframe(bf_second_half_2022 = n()) %>% distinct()
bf_23 <- sh_bf_23 %>% group_by(pitcher) %>% reframe(bf_second_half_2023 = n()) %>% distinct()

plate_appearances <- plate_appearances %>% 
  left_join(pa_21, by = 'batter') %>% 
  left_join(pa_22, by = 'batter') %>% 
  left_join(pa_23, by = 'batter')

plate_appearances <- plate_appearances %>% 
  mutate(across(c(starts_with('pa_second_half_')), ~replace_na(., 0)))

batters_faced <- batters_faced %>% 
  left_join(bf_21, by = 'pitcher') %>% 
  left_join(bf_22, by = 'pitcher') %>% 
  left_join(bf_23, by = 'pitcher') %>% 
  mutate(across(c(starts_with('bf_second_half_')), ~replace_na(., 0)))

remove(bf_21, bf_22, bf_23, sh_pa_21, sh_pa_22, sh_pa_23, sh_bf_21,
       sh_bf_22, sh_bf_23, pa_21, pa_22, pa_23, second_half_21,second_half_22,
       second_half_23)

############## BATTING ###############

# BA, OBP, SLG, ISO, BABIP, wOBA, ChaseRate, WhiffRate, GB%

hits <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    hits = sum(events %in% c("single", "double", "triple", "home_run")),
    abs = sum(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out", 'force_out',
                            'ground_into_double_play','double_play', 'fielders_choice','fielders_choice_out',
                            'triple_play', 'field_error','other_out','strikeout_double_play')),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c('hits','abs'))%>% 
  mutate(across(everything(), ~replace_na(., 0)))


on_base <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    onbase = sum(events %in% c("single", "double", "triple", "home_run", 'walk','hit_by_pitch')),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = onbase, names_prefix = "onbase_")%>% 
  mutate(across(everything(), ~replace_na(., 0)))




total_bases <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    total_bases = sum(case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      .default = 0
    )),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = total_bases, names_prefix = "total_bases") %>% 
  mutate(across(everything(), ~replace_na(., 0)))

walks_hbp_strikeouts <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    walks_hbp = sum(events %in% c('walk','hit_by_pitch')),
    strikeouts = sum(events %in% c('strikeout')),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c('walks_hbp','strikeouts'))%>% 
  mutate(across(everything(), ~replace_na(., 0)))

chases <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    chases = sum(description %in% c("swinging_strike", 'foul','hit_into_play','foul_tip',
                                    'swinging_strike_blocked','foul_pitchout') & zone > 9, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = chases, names_prefix = "chases_")%>% 
  mutate(across(everything(), ~replace_na(., 0)))

whiffs <- statcast %>%
  group_by(batter, game_year) %>%
  summarise(
    whiffs = sum(description %in% c("swinging_strike", "swinging_strike_blocked"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = whiffs, names_prefix = "whiffs_") %>% 
  mutate(across(everything(), ~replace_na(., 0)))

barrels_hardhit_woba <- statcast %>% 
  group_by(batter, game_year) %>% 
  summarise(
    barrels = sum(launch_speed_angle == 6, na.rm = TRUE),
    barrel_pct = sum(launch_speed_angle == 6, na.rm = TRUE)/sum(description == 'hit_into_play', na.rm = TRUE),
    hardhits = sum(launch_speed >= 95 & description == 'hit_into_play', na.rm = TRUE),
    hardhit_pct = sum(launch_speed >= 95 & description == 'hit_into_play', na.rm = TRUE)/sum(description == 'hit_into_play', na.rm = TRUE),
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    .groups = 'drop'
  ) %>% pivot_wider(names_from = game_year, values_from = c('barrels','hardhits','wOBA',
                                                            'barrel_pct','hardhit_pct')) %>% 
  mutate(across(c(starts_with('barrels_20'),starts_with('hardhits_20')), ~replace_na(., 0)))

hr <- statcast %>% 
  group_by(batter, game_year) %>% 
  summarise(
    hr = sum(events %in% c('home_run'), na.rm = TRUE),
    hr_pct = sum(events %in% c('home_run'), na.rm = TRUE)/sum(description %in% c('hit_into_play'), na.rm = TRUE)
    ,.groups = 'drop'
  ) %>% 
  pivot_wider(names_from = game_year, values_from = c(hr, hr_pct)) %>% 
  mutate(across(starts_with('hr_20'), ~replace_na(., 0)))


batter_stats<- hits %>%
  left_join(on_base, by = "batter") %>%
  left_join(total_bases, by = "batter") %>%
  left_join(walks_hbp_strikeouts, by = "batter") %>%
  left_join(chases, by = "batter") %>%
  left_join(whiffs, by = "batter") %>%
  left_join(barrels_hardhit_woba, by = "batter") %>% 
  left_join(hr, by = 'batter')

# adding slugging percentage
batter_stats <- batter_stats %>% 
  mutate(slg_2021 = total_bases2021/abs_2021,
         slg_2022 = total_bases2022/abs_2022,
         slg_2023 = total_bases2023/abs_2023)


plate_appearances <- plate_appearances %>% 
  left_join(batter_stats, by = 'batter')

remove(batter_stats, hits, on_base, total_bases,
       walks_hbp_strikeouts, chases, whiffs, barrels_hardhit_woba, hr)
############## Pitching ###############

# BA, OBP, SLG, ISO, BABIP, wOBA, ChaseRate, WhiffRate, GB%

baa <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    hitsa = sum(events %in% c("single", "double", "triple", "home_run")),
    BAA = sum(events %in% c("single", "double", "triple", "home_run")) / 
      sum(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out", 'force_out',
                        'ground_into_double_play','double_play', 'fielders_choice','fielders_choice_out',
                        'triple_play', 'field_error','other_out','strikeout_double_play')),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c(hitsa, BAA)) %>% 
  mutate(across(starts_with('hitsa'), ~replace_na(., 0)))


obpa <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    on_basea = (sum(events %in% c("single", "double", "triple", "home_run", 'walk','hit_by_pitch'))),
    OBPA = (sum(events %in% c("single", "double", "triple", "home_run", 'walk','hit_by_pitch')))/
      (sum(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out", 'force_out',
                         'ground_into_double_play','double_play', 'fielders_choice','fielders_choice_out',
                         'triple_play', 'field_error','other_out','strikeout_double_play', 'walk','hit_by_pitch',
                         'sac_fly','sac_bunt','sac_fly_double_play','sac_bunt_double_play'))),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c(on_basea,OBPA)) %>% 
  mutate(across(starts_with('on_base'), ~replace_na(.,0)))

slga <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    total_basesa = sum(case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      .default = 0
    )),
    SLGA = sum(case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      .default = 0
    )) /
      sum(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out", 'force_out',
                        'ground_into_double_play','double_play', 'fielders_choice','fielders_choice_out',
                        'triple_play', 'field_error','other_out','strikeout_double_play')),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c(total_basesa, SLGA)) %>% 
  mutate(across(starts_with('total_bases'), ~replace_na(.,0)))

wobaa <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    wOBAA = sum(woba_value, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = wOBAA, names_prefix = "wOBAA_")

chase_ratea <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    chasea = sum(description %in% c("swinging_strike", 'foul','hit_into_play','foul_tip',
                                    'swinging_strike_blocked','foul_pitchout') & zone > 9, na.rm = TRUE),
    ChaseRateA = sum(description %in% c("swinging_strike", 'foul','hit_into_play','foul_tip',
                                        'swinging_strike_blocked','foul_pitchout') & zone > 9, na.rm = TRUE) / 
      sum(zone > 9, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c(ChaseRateA, chasea)) %>% 
  mutate(across(starts_with('chasea'), ~replace_na(., 0)))

whiff_ratea <- statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    whiffsa = sum(description %in% c("swinging_strike", "swinging_strike_blocked"), na.rm = TRUE),
    WhiffRateA = sum(description %in% c("swinging_strike", "swinging_strike_blocked"), na.rm = TRUE) / 
      sum(description %in% c("foul", "swinging_strike", "hit_into_play", "swinging_strike_blocked",
                             "foul_tip",'foul_pitchout'), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = c(game_year), values_from = c('whiffsa','WhiffRateA')) %>% 
  mutate(across(starts_with('whiffsa'), ~replace_na(., 0)))

barrels_hardhitsa <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  summarise(
    barrelsa = sum(launch_speed_angle == 6, na.rm = TRUE),
    barrel_pcta = sum(launch_speed_angle == 6, na.rm = TRUE)/sum(description == 'hit_into_play', na.rm = TRUE),
    hardhitsa = sum(launch_speed >= 95 & description == 'hit_into_play', na.rm = TRUE),
    hardhit_pcta = sum(launch_speed >= 95 & description == 'hit_into_play', na.rm = TRUE)/sum(description == 'hit_into_play', na.rm = TRUE),
    .groups = 'drop'
  ) %>% pivot_wider(names_from = game_year, values_from = c('barrelsa','hardhitsa',
                                                            'barrel_pcta','hardhit_pcta')) %>% 
  mutate(across(c(starts_with('barrelsa_20'),starts_with('hardhitsa_20')), ~replace_na(., 0)))

hra <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  summarise(
    hra = sum(events %in% c('home_run'), na.rm = TRUE),
    hr_pcta = sum(events %in% c('home_run'), na.rm = TRUE)/sum(description %in% c('hit_into_play'), na.rm = TRUE)
    ,.groups = 'drop'
  ) %>% 
  pivot_wider(names_from = game_year, values_from = c(hra, hr_pcta)) %>% 
  mutate(across(starts_with('hra_20'), ~replace_na(., 0)))

# groundball rate
groundball_rate <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  summarise(
    gball_rate = sum(bb_type %in% c('ground_ball'))/sum(!is.na(events))
    ,.groups = 'drop') %>% 
  pivot_wider(names_from = game_year, values_from = gball_rate, names_prefix = 'gball_rate_')

pitcher_stats <- baa %>%
  left_join(obpa, by = "pitcher") %>%
  left_join(slga, by = "pitcher") %>%
  left_join(wobaa, by = "pitcher") %>%
  left_join(chase_ratea, by = "pitcher") %>%
  left_join(whiff_ratea, by = "pitcher") %>% 
  left_join(barrels_hardhitsa, by = 'pitcher') %>% 
  left_join(hra, by = 'pitcher') %>% 
  left_join(groundball_rate, by = 'pitcher')

batters_faced <- batters_faced %>% 
  left_join(pitcher_stats, by = 'pitcher')

batters_faced %>% filter(years_since_debut_23 > 1) %>% ggplot(aes(total_basesa_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

remove(pitcher_stats, obpa, baa, wobaa, chase_ratea,
       whiff_ratea,  barrels_hardhitsa, slga, hra)

### PA per Game ####
plate_appearances_per_game <- statcast %>%
  group_by(
    batter,
    game_year,
    game_pk,
    at_bat_number
  ) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(batter, game_year, game_pk) %>% 
  summarise(PA = n()) %>% 
  ungroup() %>% 
  group_by(batter, game_year) %>% 
  reframe(PA_per_game = mean(PA)) %>% 
  pivot_wider(names_from = game_year, values_from = PA_per_game, names_prefix = 'PA_per_game_')

plate_appearances_per_game <- plate_appearances_per_game %>% 
  mutate(across(everything(), ~replace_na(., 0)))

plate_appearances <- plate_appearances %>% 
  left_join(plate_appearances_per_game, by = 'batter')

remove(plate_appearances_per_game)

plate_appearances %>% filter(PA_per_game_2022 != 0) %>% 
  ggplot(aes(PA_per_game_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(c(0,800))+
  theme_bw()

#### Team Runs Per Game ####
batters_ineachgame <- statcast %>% 
  distinct(batter, game_pk, game_year, inning_topbot)

home_runs_scored <- statcast %>% 
  group_by(game_pk, game_year) %>% 
  slice_max(post_home_score, n = 1) %>% 
  select(post_home_score, game_pk, game_year) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(inning_topbot = 'Bot')

away_runs_scored <- statcast %>% 
  group_by(game_pk, game_year) %>% 
  slice_max(post_away_score, n = 1) %>% 
  select(post_away_score, game_pk, game_year) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(inning_topbot = 'Top')

runs_scored <- bind_rows(away_runs_scored, home_runs_scored)


batters_ineachgame <- batters_ineachgame %>% 
  left_join(runs_scored, by = c('inning_topbot', 'game_pk','game_year'))

batters_ineachgame <- batters_ineachgame %>% pivot_longer(
  values_to = 'num_runs',
  cols = c('post_home_score','post_away_score')
) %>% 
  drop_na(num_runs)


runs_per_game <- batters_ineachgame %>% 
  group_by(batter, game_year) %>% 
  reframe(runs_per_game = mean(num_runs))

runs_per_game <- runs_per_game %>% 
  pivot_wider(names_from = game_year, values_from = runs_per_game, names_prefix = 'runs_per_game_')


runs_per_game <- runs_per_game %>% 
  mutate(across(c(starts_with('runs_per_game_')), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

plate_appearances <- plate_appearances %>% 
  left_join(runs_per_game, by = 'batter')

remove(runs_per_game, batters_ineachgame, runs_scored,away_runs_scored, home_runs_scored)

#### Platoon Splits (IE Difference between PA/BF for Right vs Left)####
plate_appearances_lr <- 
  # start with the savant data
  statcast %>%
  # we will group by batter, season, game, and at bat and preserve the 
  group_by(
    batter,
    game_year,
    game_pk,
    at_bat_number,
    p_throws
  ) %>%
  summarise() %>%
  ungroup() %>%
  # now we have just unique batter, season, game, and at bat observations
  # but, we need to count how many of those there are each season
  # so, we will do another group by and summarise
  group_by(
    batter,
    game_year,
    p_throws
  ) %>%
  summarise(
    # the n() function counts the number of unique observations we have
    playing_time = n()
  ) %>%
  ungroup()

plate_appearances_lr <- plate_appearances_lr %>% 
  pivot_wider(names_from = c(game_year, p_throws), values_from = playing_time, names_prefix = 'PA_') %>% 
  mutate(across(-'batter', ~replace_na(.,0)))

plate_appearances_lr <- plate_appearances_lr %>% 
  mutate(pa_diff_2021 = abs(PA_2021_R - PA_2021_L),
         pa_diff_2022 = abs(PA_2022_R - PA_2022_L),
         pa_diff_2023 = abs(PA_2023_R- PA_2023_L)
  ) %>% 
  select(batter, starts_with('pa_diff'))

plate_appearances <- plate_appearances %>% 
  left_join(plate_appearances_lr, by = 'batter')

batters_faced_lr <- 
  # start with the savant data
  statcast %>%
  # we will group by batter, season, game, and at bat and preserve the 
  group_by(
    pitcher,
    game_year,
    game_pk,
    at_bat_number,
    stand
  ) %>%
  summarise() %>%
  ungroup() %>%
  # now we have just unique batter, season, game, and at bat observations
  # but, we need to count how many of those there are each season
  # so, we will do another group by and summarise
  group_by(
    pitcher,
    game_year,
    stand
  ) %>%
  summarise(
    # the n() function counts the number of unique observations we have
    playing_time = n()
  ) %>%
  ungroup()

batters_faced_lr <- batters_faced_lr %>% 
  pivot_wider(names_from = c(game_year, stand), values_from = playing_time, names_prefix = 'BF_') %>% 
  mutate(across(-'pitcher', ~replace_na(.,0)))

batters_faced_lr <- batters_faced_lr %>% 
  mutate(bf_diff_2021 = abs(BF_2021_R - BF_2021_L),
         bf_diff_2022 = abs(BF_2022_R - BF_2022_L),
         bf_diff_2023 = abs(BF_2023_R- BF_2023_L)
  ) %>% 
  select(pitcher, starts_with('bf_diff'))

batters_faced <- batters_faced %>% 
  left_join(batters_faced_lr, by = 'pitcher')

remove(batters_faced_lr, plate_appearances_lr)

#### Player height and weight ####
plate_appearances <- plate_appearances %>% 
  left_join(lahman %>% select(player_mlb_id, weight, height), by = c('batter' = 'player_mlb_id'))

batters_faced <- batters_faced %>% 
  left_join(lahman %>% select(player_mlb_id, weight, height), by = c('pitcher' = 'player_mlb_id'))

#### Pitches Per AB, BF per Appearance, Times through the order ####
#pitches in order
in_order <- statcast %>% 
  arrange(game_date, game_pk, at_bat_number, pitch_number)

pitchers <- in_order %>% 
  group_by(game_pk, game_year) %>% 
  distinct(pitcher_at_bat_number, pitch_number, pitch_number_appearance, pitcher)

pitchers_in_order <- pitchers %>% 
  arrange(pitcher, game_pk, pitch_number_appearance)

pitches_per_at_bat <- pitchers_in_order %>% 
  group_by(pitcher, pitcher_at_bat_number, game_pk, game_year) %>% 
  summarize(max_pitch = max(pitch_number), .groups = 'drop')

pitches_and_ab_per_game <- pitchers_in_order %>% 
  group_by(pitcher, game_pk, game_year) %>% 
  summarize(max_pitch = max(pitch_number_appearance), 
            max_at_bat_number= max(pitcher_at_bat_number), .groups = 'drop')

#pitches per atbat
per_batter_metric <- pitches_per_at_bat %>% 
  group_by(pitcher, game_year) %>% 
  summarize(avg_pitches_per_batter = mean(max_pitch), .groups = 'drop')

# pitches per game and batters per game
per_game_metrics <- pitches_and_ab_per_game %>% group_by(pitcher, game_year) %>% 
  summarize(avg_pitches_per_appearance = mean(max_pitch), 
            avg_bf_per_appearance = mean(max_at_bat_number), .groups = 'drop')

# times through order
times_through <- in_order %>% 
  group_by(pitcher, game_year, game_pk) %>% 
  slice_max(times_faced, n = 1) %>% 
  reframe(times_faced) %>% 
  distinct()

times_through <- times_through %>% 
  group_by(pitcher, game_year) %>% 
  reframe(times_faced = mean(times_faced))

per_game_metrics <- per_game_metrics %>% 
  pivot_wider(names_from = game_year, values_from = c(avg_pitches_per_appearance, avg_bf_per_appearance)) %>% 
  mutate(across(everything(), ~replace_na(., 0)))


times_through <- times_through %>% 
  pivot_wider(names_from = game_year, values_from = times_faced, names_prefix = 'times_faced_') %>% 
  mutate(across(everything(), ~replace_na(., 1)))

batters_faced <- batters_faced %>% 
  left_join(per_game_metrics, by = 'pitcher') %>% 
  left_join(times_through, by = 'pitcher')

# Games Played Percentage ####
# Finding number of games the team played after each rookie's debut
# otherwise, We assume that the player was up for all 162 games
#### 2021 ####
debuts_2021 <- plate_appearances %>% 
  filter(years_since_debut_23 == 2) %>% 
  left_join(lahman %>% select(player_mlb_id, debut), by = c('batter' = 'player_mlb_id'))


debuts_2021_teams <- statcast %>% 
  filter(batter %in% debuts_2021$batter, game_year == 2021) %>% 
  mutate(bat_team = ifelse(inning_topbot == 'Top', away_team, home_team)) %>% 
  select(batter, bat_team) %>% 
  distinct() %>% 
  distinct(batter, .keep_all = TRUE) #removing players on 2 teams, assumption: teams play similar amount of games after any given date

debuts_2021 <- debuts_2021 %>% 
  left_join(debuts_2021_teams, by = 'batter') %>% 
  select(batter, bat_team, debut)

team_games <- numeric();for (batter in 1:nrow(debuts_2021)){
  bat_team <- pull(debuts_2021[batter, 'bat_team'])
  print(bat_team)
  debut <- pull(debuts_2021[batter, 'debut'])
  print(class(debut))
  team_gp <- statcast %>% 
    mutate(bat_team1 = ifelse(inning_topbot == 'Top', away_team, home_team)) %>%
    filter(bat_team1 == bat_team,
           game_year == 2021, game_date >= as.Date(debut)) %>% 
    distinct(game_pk, bat_team1) %>% 
    group_by(bat_team1) %>% 
    reframe(games_played = n()) %>% 
    pull(games_played)
  team_games <- c(team_games, team_gp)
  
};debuts_2021$team_games_2021 <- team_games

#### 2022 ####
debuts_2022 <- plate_appearances %>% 
  filter(years_since_debut_23 == 1) %>% 
  left_join(lahman %>% select(player_mlb_id, debut), by = c('batter' = 'player_mlb_id'))


debuts_2022_teams <- statcast %>% 
  filter(batter %in% debuts_2022$batter, game_year == 2022) %>% 
  mutate(bat_team = ifelse(inning_topbot == 'Top', away_team, home_team)) %>% 
  select(batter, bat_team) %>% 
  distinct() %>% 
  distinct(batter, .keep_all = TRUE) #removing players on 2 teams, assumption: teams play similar amount of games after any given date

debuts_2022 <- debuts_2022 %>% 
  left_join(debuts_2022_teams, by = 'batter') %>% 
  select(batter, bat_team, debut)

team_games <- numeric();for (batter in 1:nrow(debuts_2022)){
  bat_team <- pull(debuts_2022[batter, 'bat_team'])
  print(bat_team)
  debut <- pull(debuts_2022[batter, 'debut'])
  print(class(debut))
  team_gp <- statcast %>% 
    mutate(bat_team1 = ifelse(inning_topbot == 'Top', away_team, home_team)) %>%
    filter(bat_team1 == bat_team,
           game_year == 2022, game_date >= as.Date(debut)) %>% 
    distinct(game_pk, bat_team1) %>% 
    group_by(bat_team1) %>% 
    reframe(games_played = n()) %>% 
    pull(games_played)
  team_games <- c(team_games, team_gp)
  
};debuts_2022$team_games_2022 <- team_games

#### 2023 ####
debuts_2023 <- plate_appearances %>% 
  filter(years_since_debut_23 == 0) %>% 
  left_join(lahman %>% select(player_mlb_id, debut), by = c('batter' = 'player_mlb_id'))


debuts_2023_teams <- statcast %>% 
  filter(batter %in% debuts_2023$batter, game_year == 2023) %>% 
  mutate(bat_team = ifelse(inning_topbot == 'Top', away_team, home_team)) %>% 
  select(batter, bat_team) %>% 
  distinct() %>% 
  distinct(batter, .keep_all = TRUE) #removing players on 2 teams, assumption: teams play similar amount of games after any given date

debuts_2023 <- debuts_2023 %>% 
  left_join(debuts_2023_teams, by = 'batter') %>% 
  select(batter, bat_team, debut)

team_games <- numeric();for (batter in 1:nrow(debuts_2023)){
  bat_team <- pull(debuts_2023[batter, 'bat_team'])
  print(bat_team)
  debut <- pull(debuts_2023[batter, 'debut'])
  print(class(debut))
  team_gp <- statcast %>% 
    mutate(bat_team1 = ifelse(inning_topbot == 'Top', away_team, home_team)) %>%
    filter(bat_team1 == bat_team,
           game_year == 2023, game_date >= as.Date(debut)) %>% 
    distinct(game_pk, bat_team1) %>% 
    group_by(bat_team1) %>% 
    reframe(games_played = n()) %>% 
    pull(games_played)
  team_games <- c(team_games, team_gp)
  
};debuts_2023$team_games_2023 <- team_games


plate_appearances <- plate_appearances %>% 
  left_join(debuts_2021 %>% select(batter, team_games_2021), by = 'batter') %>% 
  left_join(debuts_2022 %>% select(batter, team_games_2022), by = 'batter') %>% 
  left_join(debuts_2023 %>% select(batter, team_games_2023), by = 'batter')%>% 
  mutate(across(starts_with('team_games'), ~replace_na(., 162)))

plate_appearances <- plate_appearances %>% 
  left_join(batting_games_started_year, by = 'batter')


plate_appearances <- plate_appearances %>% 
  mutate(prop_games_started_2021 = games_started_2021/team_games_2021,
         prop_games_started_2022 = games_started_2022/team_games_2022,
         prop_games_started_2023 = games_started_2023/team_games_2023) %>% 
  mutate(across(starts_with('prop_games_started_'), ~replace_na(., 0)))

remove(batting_games_started_year, batting_games_started, debuts_2021, debuts_2021_teams,
       debuts_2022, debuts_2022_teams, debuts_2023, debuts_2023_teams)

#### IP, ERA, and FIP ####
#IP
in_order <- in_order %>% 
  group_by(game_pk, inning, inning_topbot) %>% 
  mutate(post_outs = lead(outs_when_up)) %>% 
  ungroup() %>% 
  mutate(across(post_outs, ~replace_na(., 3)))

players <- in_order %>%
  mutate(outs_made = post_outs - outs_when_up) %>% 
  group_by(pitcher, game_year) %>%
  summarize(TotalOuts = sum(outs_made), .groups = 'drop')

players <- players %>%
  mutate(ip = floor(TotalOuts/3) + (TotalOuts%%3)/10)

players_wide <- players %>% 
  pivot_wider(names_from = game_year, values_from = c(TotalOuts, ip))

# ERA
earned_runs <- statcast %>% 
  drop_na(events) %>% 
  relocate(game_pk, .before = pitch_type) %>% 
  relocate(at_bat_number, .after = 'game_pk') %>% 
  relocate(pitch_number, .after = 'at_bat_number') %>% 
  arrange(game_pk, at_bat_number, pitch_number)

earned_runs <- earned_runs %>% 
  select(pitcher, game_pk, game_year, at_bat_number, pitch_number, events, outs_when_up, inning, 
         inning_topbot, batter, starts_with('on_'), bat_score, post_bat_score)

#runs scored
earned_runs <- earned_runs %>% 
  mutate(runs = post_bat_score - bat_score,
         earned_runs = post_bat_score - bat_score #editing column
  )

# fielding error
earned_runs <- earned_runs %>% 
  mutate(fielding_error = ifelse(events %in% c('field_error', 'pickoff_error_3b'), 1,0))

earned_runs <- earned_runs %>% 
  group_by(game_pk, inning, inning_topbot) %>% 
  mutate(errors_in_inning = cumsum(fielding_error)) %>% 
  ungroup()

#finding outs post-play
earned_runs <- earned_runs %>% 
  group_by(game_pk, inning, inning_topbot) %>% 
  mutate(post_outs = lead(outs_when_up)) %>% 
  ungroup() %>% 
  group_by(game_pk) %>% 
  mutate(post_outs = ifelse(is.na(post_outs) & at_bat_number == max(at_bat_number) & runs > 0, outs_when_up, post_outs)) %>% 
  ungroup() %>% 
  replace_na(list(post_outs = 3))

# getting batter_id if error is made
# max errors in an inning is 2
#saving in two columns: batter_error_1 and batter_error_2
earned_runs <- earned_runs %>% 
  mutate(batter_error_1 = ifelse(errors_in_inning == 1 & events == 'field_error', batter, NA),
         batter_error_2 = ifelse(errors_in_inning == 2 & events == 'field_error',batter, NA))

earned_runs <- earned_runs %>% 
  group_by(game_pk, inning, inning_topbot) %>% 
  fill(batter_error_1, .direction = 'down') %>% 
  fill(batter_error_2, .direction = 'down') %>% 
  replace_na(list(batter_error_1 = '', batter_error_2 = '')) %>% 
  ungroup()

earned_runs <- earned_runs %>% 
  rowwise() %>% 
  mutate(batter_error_1_on = batter_error_1 %in% c(on_1b,on_2b,on_3b),
         batter_error_2_on = batter_error_2 %in% c(on_1b,on_2b,on_3b)) %>% 
  ungroup()

#finding runners in front of batter
earned_runs <- earned_runs %>% 
  mutate(runners_infront_batter_error_1 = case_when(
    batter_error_1_on == TRUE & batter_error_1 == on_1b & is.na(on_2b) & is.na(on_3b) ~ 0,
    batter_error_1_on == TRUE & batter_error_1 == on_1b & !is.na(on_2b) & is.na(on_3b) ~ 1,
    batter_error_1_on == TRUE & batter_error_1 == on_1b & is.na(on_2b) & !is.na(on_3b) ~ 1,
    batter_error_1_on == TRUE & batter_error_1 == on_1b & !is.na(on_2b) & !is.na(on_3b) ~ 2,
    batter_error_1_on == TRUE & batter_error_1 == on_2b & is.na(on_3b) ~ 0,
    batter_error_1_on == TRUE & batter_error_1 == on_2b & !is.na(on_3b) ~ 1,
    batter_error_1_on == TRUE & batter_error_1 == on_3b ~ 0,
    .default = NA
  ),
  runners_infront_batter_error_2 = case_when(
    batter_error_2_on == TRUE & batter_error_2 == on_1b & is.na(on_2b) & is.na(on_3b) ~ 0,
    batter_error_2_on == TRUE & batter_error_2 == on_1b & !is.na(on_2b) & is.na(on_3b) ~ 1,
    batter_error_2_on == TRUE & batter_error_2 == on_1b & is.na(on_2b) & !is.na(on_3b) ~ 1,
    batter_error_2_on == TRUE & batter_error_2 == on_1b & !is.na(on_2b) & !is.na(on_3b) ~ 2,
    batter_error_2_on == TRUE & batter_error_2 == on_2b & is.na(on_3b) ~ 0,
    batter_error_2_on == TRUE & batter_error_2 == on_2b & !is.na(on_3b) ~ 1,
    batter_error_2_on == TRUE & batter_error_2 == on_3b ~ 0,
    .default = NA
  )
  )

nrow(statcast %>% filter(events == 'pickoff_error_3b')) #1
nrow(statcast %>% filter(events == 'passed_ball'))#1, occurs on a walkoff

#only one observation of both passed_ball and pickoff_error_3b
earned_runs <- earned_runs %>% 
  mutate(earned_runs = ifelse(events %in% c('pickoff_error_3b','passed_ball'), 0, earned_runs))

# finding when an errored batter gets off the basepaths
earned_runs <- earned_runs %>% 
  group_by(game_pk, inning, inning_topbot) %>% 
  mutate(batter_1_offbasepaths = batter_error_1_on & lead(batter_error_1_on) == FALSE,
         batter_2_offbasepaths = batter_error_2_on & lead(batter_error_2_on) == FALSE) %>% 
  ungroup()

# adding first earned_runs deduction
# if an error occured in the inning and there are 2 outs after the play and the play didn't result in a hit, 
# remove any runs scored after the fact
earned_runs <- earned_runs %>% 
  mutate(earned_runs = case_when(
    errors_in_inning >= 1 & post_outs >= 2 & 
      !(events %in% c('single', 'double','triple','home_run')) ~ 0,
    errors_in_inning >= 1 & post_outs >= 2 & outs_when_up == 2 ~ 0,
    .default = earned_runs))

# when 2 errors are made, it reduces to 1 outs
earned_runs <- earned_runs %>% 
  mutate(earned_runs = case_when(
    errors_in_inning >= 2 & post_outs >= 1 & 
      !(events %in% c('single', 'double','triple','home_run')) ~ 0,
    errors_in_inning >= 2 & post_outs >= 1 & outs_when_up >= 1 ~ 0,
    .default = earned_runs))

# finding if errored runner scored
earned_runs <- earned_runs %>% 
  mutate(
    batter_error_1_scored = case_when(
      runs > 0 & runs < runners_infront_batter_error_1 ~ FALSE,
      runs > 0 & runs >= runners_infront_batter_error_1 ~ TRUE,
      .default = NA),
    batter_error_2_scored = case_when(
      runs > 0 & runs < runners_infront_batter_error_2 ~ FALSE,
      runs > 0 & runs >= runners_infront_batter_error_2 ~ TRUE,
      .default = NA)
  )

# removing runs where the batter came around to score
earned_runs <- earned_runs %>% 
  mutate(earned_runs = case_when(
    earned_runs != 0 & batter_error_1_scored == TRUE & 
      (batter_error_2_scored == FALSE | is.na(batter_error_2_scored)) ~ earned_runs - 1,
    earned_runs != 0 & batter_error_2_scored == TRUE & 
      (batter_error_1_scored == FALSE | is.na(batter_error_1_scored)) ~ earned_runs - 1,
    earned_runs != 0 & batter_error_2_scored == TRUE & 
      batter_error_1_scored == TRUE ~ earned_runs - 2,
    .default = earned_runs
  ))
#getting yearly league ERA for FIP calculations
lg_ERA <- earned_runs %>% 
  mutate(outs_made = post_outs - outs_when_up) %>% 
  group_by(game_year) %>% 
  reframe(
    earned_runs = sum(earned_runs, na.rm = TRUE),
    TotalOuts = sum(outs_made, na.rm = TRUE)
  ) %>% 
  mutate(
    ip = floor(TotalOuts/3) + (TotalOuts%%3)/10,
    lgERA = (earned_runs/ip)*9
  ) %>% 
  select(game_year, lgERA, ip) %>% 
  pivot_wider(names_from = game_year, values_from = c(lgERA,ip))


#getting earned runs on the season
season_earned_runs <- earned_runs %>% 
  group_by(pitcher, game_year) %>% 
  reframe(earned_runs = sum(earned_runs),
          pa = n()) %>% 
  arrange(desc(game_year), desc(earned_runs)) %>% 
  pivot_wider(names_from = game_year, values_from = c(earned_runs, pa))

season_earned_runs <- season_earned_runs %>% 
  left_join(players_wide, by = 'pitcher') %>% 
  mutate(ERA_2021 = (earned_runs_2021/ip_2021)*9,
         ERA_2022 = (earned_runs_2022/ip_2022)*9,
         ERA_2023 = (earned_runs_2023/ip_2023)*9) %>% 
  mutate(across(starts_with('ERA_'), ~ifelse(is.infinite(.), 1000,.)))

batters_faced <- batters_faced %>% 
  left_join(season_earned_runs %>% 
              select(pitcher, starts_with('earned_runs_'), starts_with('ERA_'), starts_with('ip_')), by = 'pitcher')


# FIP
#Homeruns, walks+hbp, and K for pitcher
pitcher_fip <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  reframe(HRA = sum(events %in% c('home_run'), na.rm = TRUE),
          walks_hbp = sum(events %in% c('walk', 'hit_by_pitch'), na.rm = TRUE),
          so = sum(events %in% c('strikeout','strikeout_double_play'))) %>% 
  left_join(players %>% select(pitcher, game_year, ip), by = c('pitcher','game_year'))

#Homeruns, walks+hbp, and K for league
lg_fip_stats <- statcast %>% 
  group_by(game_year) %>% 
  reframe(lgHRA = sum(events %in% c('home_run'), na.rm = TRUE),
          lg_walks_hbp = sum(events %in% c('walk', 'hit_by_pitch'), na.rm = TRUE),
          lg_so = sum(events %in% c('strikeout','strikeout_double_play'))) %>% 
  pivot_wider(names_from = game_year, values_from = c(lgHRA, lg_walks_hbp, lg_so))


#calculating fip constant for each year
fip_constant_fun <- function(lgERA, lgHR, lgBB, lgK, lgIP){
  num <- 13*lgHR + 3*lgBB - 2*lgK # where lgBB is league Walks + league HBP
  denom <- lgIP
  fip_constant <- lgERA - (num/denom)
  
  return(fip_constant)
}

(fip_constant2021 <- fip_constant_fun(lgERA = lg_ERA$lgERA_2021,
                                      lgHR = lg_fip_stats$lgHRA_2021,
                                      lgBB = lg_fip_stats$lg_walks_hbp_2021,
                                      lgK = lg_fip_stats$lg_so_2021,
                                      lgIP = lg_ERA$ip_2021))

(fip_constant2022 <- fip_constant_fun(lgERA = lg_ERA$lgERA_2022,
                                      lgHR = lg_fip_stats$lgHRA_2022,
                                      lgBB = lg_fip_stats$lg_walks_hbp_2022,
                                      lgK = lg_fip_stats$lg_so_2022,
                                      lgIP = lg_ERA$ip_2022))

(fip_constant2023 <- fip_constant_fun(lgERA = lg_ERA$lgERA_2023,
                                      lgHR = lg_fip_stats$lgHRA_2023,
                                      lgBB = lg_fip_stats$lg_walks_hbp_2023,
                                      lgK = lg_fip_stats$lg_so_2023,
                                      lgIP = lg_ERA$ip_2023))


fip_calc_fun <- function(game_year, hr, bb, so, ip){
  num <- 13*hr + 3*bb - 2*so
  fip_bef_cons <- num/ip
  if(game_year == 2021){
    fip <- fip_bef_cons + fip_constant2021
  } else if(game_year == 2022){
    fip <- fip_bef_cons + fip_constant2022
  } else if (game_year == 2023){
    fip <- fip_bef_cons + fip_constant2023
  }
  
}

pitcher_fip_wide <- pitcher_fip %>% 
  rowwise() %>% 
  mutate(fip = fip_calc_fun(game_year, HRA, walks_hbp, so, ip)) %>% 
  mutate(fip = ifelse(is.infinite(fip), 1000, fip)) %>%
  ungroup() %>% 
  select(-starts_with('ip')) %>% 
  pivot_wider(names_from = game_year, values_from = c('HRA','walks_hbp','so','fip')) %>% 
  mutate(across(c(starts_with('HRA_'),starts_with('walks_')
                  ,starts_with('so_')), ~replace_na(., 0)))

batters_faced <- batters_faced %>% 
  left_join(pitcher_fip_wide, by = 'pitcher')

remove(earned_runs, pitcher_fip, pitcher_fip_wide)
# Pitches Faced/Thrown ####
batter_pitches_faced <- statcast %>% 
  group_by(batter, game_year) %>% 
  reframe(pitches_faced = n()) %>% 
  pivot_wider(names_from = game_year, values_from = pitches_faced,
              names_prefix = 'pitches_faced_') %>% 
  mutate(across(everything(), ~replace_na(., 0)))

plate_appearances <- plate_appearances %>% 
  left_join(batter_pitches_faced, by = 'batter')
remove(batter_pitches_faced)

plate_appearances %>% 
  filter(years_since_debut_23 > 1) %>% 
  ggplot(aes(pitches_faced_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

pitches_thrown <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  reframe(pitches_thrown = n()) %>% 
  pivot_wider(names_from = game_year, values_from = pitches_thrown,
              names_prefix = 'pitches_thrown_') %>% 
  mutate(across(everything(), ~replace_na(., 0)))

batters_faced <- batters_faced %>% 
  left_join(pitches_thrown, by = 'pitcher')
remove(pitches_thrown)

batters_faced %>% 
  filter(years_since_debut_23 > 1) %>% 
  ggplot(aes(pitches_thrown_2022, playing_time_2023)) +
  geom_point() +
  geom_smooth() +
  theme_bw()




#### Opening Day Season Rotation ####
#finding first 5 games of each player's season
first_five_games <- statcast %>% 
  mutate(pitching_team = ifelse(inning_topbot == 'Top', home_team, away_team)) %>%
  filter(inning == 1) %>% 
  distinct(game_pk, game_date, game_year,pitching_team, pitcher)

first_five_games <- first_five_games %>% 
  group_by(pitching_team, game_year) %>% 
  slice_min(game_date, n = 5) %>%
  ungroup()

#finding order in rotation on opening day
first_five_games <- first_five_games %>% 
  group_by(pitching_team, game_year) %>% 
  mutate(num_in_rotation = row_number()) %>% 
  ungroup()

first_five_games_wide <- first_five_games %>% 
  select(pitcher, game_year, num_in_rotation) %>% 
  group_by(pitcher, game_year) %>% 
  slice_min(num_in_rotation, n = 1) %>% # incase opening day starter starts in both game
  # 1 and 5 
  ungroup() %>% 
  pivot_wider(names_from = game_year, values_from = num_in_rotation, names_prefix = 'num_in_rotation_') %>% 
  mutate(across(everything(), ~replace_na(., 6))) # if they're not in the starting rotaiton, assume they're the
# "6" starter

batters_faced <- batters_faced %>% 
  left_join(first_five_games_wide, by = 'pitcher') %>% 
  mutate(across(starts_with('num_in_rotation_'), ~replace_na(.,6)))

batters_faced %>% 
  ggplot(aes(num_in_rotation_2022, playing_time_2023, 
             group = num_in_rotation_2022, fill = as.factor(num_in_rotation_2022))) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(1,6,by = 1)) +
  theme_bw() +
  labs(fill = 'SP Number (2022)',
       y = 'BF 2023',
       x = 'SP Number (2022)')

### Pitcher archetypes ####
pitcher_pitch_types <- statcast %>% 
  filter(!is.na(pitch_type)) %>% 
  group_by(pitcher,game_year,  pitch_type) %>% 
  reframe(release_speed = max(release_speed, na.rm = TRUE), 
          num_pitches = n()) %>% 
  distinct() %>% 
  filter(!pitch_type %in% c('FA','EP', 'PO')) # removing EEphus, other, or pitchouts

###removing pitches thrown less than 5 percent of the time
pitcher_pitch_types <- pitcher_pitch_types %>% 
  group_by(pitcher, game_year) %>% 
  mutate(pitch_pct = num_pitches/sum(num_pitches)) %>% 
  ungroup() %>% 
  filter(pitch_pct > 0.05) %>% 
  group_by(pitcher, game_year) %>% 
  mutate(num_pitches = n()) %>% 
  ungroup()

#finding number of fastballs in a pitcher's arsenal
pitcher_pitch_types_fastballs <- pitcher_pitch_types %>% 
  filter(pitch_type %in% c('FF','SI','FC')) %>% 
  select(-num_pitches, -pitch_type) %>% 
  group_by(pitcher, game_year) %>% 
  mutate(num_fastballs = n()) %>% 
  slice_max(pitch_pct, n = 1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename('main_fastball_pitch_pct' = 'pitch_pct',
         'max_fastball_velo' = 'release_speed')

#finding the number of breaking balls in an arsenal
pitcher_pitch_types_breakingballs <- pitcher_pitch_types %>% 
  filter(pitch_type %in% c('CU','KC','KN', 'SC', 'SL','CS','SV','SV')) %>% 
  select(-num_pitches, -release_speed, -pitch_type) %>% 
  group_by(pitcher, game_year) %>% 
  mutate(num_breakingballs = n()) %>% 
  slice_max(pitch_pct, n = 1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename('main_breakingball_pitch_pct' = 'pitch_pct')

# finding the number of offspeed in an arsenal
pitcher_pitch_types_offspeed <- pitcher_pitch_types %>% 
  filter(pitch_type %in% c('CH', 'FO','FS')) %>% 
  select(-num_pitches, -release_speed, -pitch_type) %>% 
  group_by(pitcher, game_year) %>% 
  mutate(num_offspeed = n()) %>% 
  slice_max(pitch_pct, n = 1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename('main_offspeed_pitch_pct' = 'pitch_pct')

# rejoining tables 
pitcher_pitch_arsenal_df <- pitcher_pitch_types_fastballs %>% 
  full_join(pitcher_pitch_types_breakingballs, by = c('pitcher','game_year')) %>% 
  full_join(pitcher_pitch_types_offspeed, by = c('pitcher','game_year'))

pitcher_pitch_arsenal_df_wide <- pitcher_pitch_arsenal_df %>% 
  pivot_wider(names_from = game_year, values_from = c(max_fastball_velo, main_fastball_pitch_pct, num_fastballs, 
                                                      main_breakingball_pitch_pct, num_breakingballs,
                                                      main_offspeed_pitch_pct, num_offspeed)) %>% 
  mutate(across(-starts_with('max_fastball_velo'), ~replace_na(., 0)))

batters_faced <- batters_faced %>% 
  left_join(pitcher_pitch_arsenal_df_wide, by = 'pitcher')

# fixing nas for pitcher fastball velo and pitch%
batters_faced <- batters_faced %>% 
  mutate(max_fastball_velo_2021  = ifelse(is.na(max_fastball_velo_2021),
                                          mean(max_fastball_velo_2021, na.rm = TRUE),
                                          max_fastball_velo_2021),
         max_fastball_velo_2022  = ifelse(is.na(max_fastball_velo_2022),
                                          mean(max_fastball_velo_2022, na.rm = TRUE),
                                          max_fastball_velo_2022),
         max_fastball_velo_2023  = ifelse(is.na(max_fastball_velo_2023),
                                          mean(max_fastball_velo_2023, na.rm = TRUE),
                                          max_fastball_velo_2023),
  )

batters_faced <- batters_faced %>% 
  mutate(across(c(starts_with('main_'), starts_with('num_off'), starts_with('num_break'), starts_with('num_fast')), ~replace_na(.,0)))


remove(pitcher_pitch_arsenal_df, pitcher_pitch_arsenal_df_wide, pitcher_pitch_types, 
       pitcher_pitch_types_breakingballs, pitcher_pitch_types_fastballs, pitcher_pitch_types_offspeed)
#### Avg EV & 90th Percentile EV ####

batter_ev <- statcast %>% 
  group_by(batter, game_year) %>% 
  reframe(
    avg_ev = mean(ifelse(description == 'hit_into_play', launch_speed, NA),na.rm = TRUE),
    percentile_90_ev = quantile(
      ifelse(description == 'hit_into_play', launch_speed, NA), probs = 0.9, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = game_year, values_from = c(avg_ev, percentile_90_ev))

plate_appearances <- plate_appearances %>% 
  left_join(batter_ev, by = 'batter')

pitcher_ev <- statcast %>% 
  group_by(pitcher, game_year) %>% 
  reframe(
    avg_ev = mean(ifelse(description == 'hit_into_play', launch_speed, NA),na.rm = TRUE),
    percentile_90_ev = quantile(
      ifelse(description == 'hit_into_play', launch_speed, NA), probs = 0.9, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = game_year, values_from = c(avg_ev, percentile_90_ev))

batters_faced <- batters_faced %>% 
  left_join(pitcher_ev, by = 'pitcher')

remove(pitcher_ev, batter_ev)

#### Pitcher 90th percentile fastball velo ####
fastball_velo <- statcast %>% 
  filter(pitch_type %in% c('FF','SI','FC')) %>% 
  group_by(pitcher, game_year) %>% 
  reframe(
    percentile_90_velo = quantile(release_speed, probs = 0.9, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = game_year, values_from = percentile_90_velo, names_prefix = 'percentile_90_velo_') %>% 
  mutate(
    percentile_90_velo_2021 = ifelse(is.na(percentile_90_velo_2021), mean(percentile_90_velo_2021, na.rm = TRUE), percentile_90_velo_2021),
    percentile_90_velo_2022 = ifelse(is.na(percentile_90_velo_2022), mean(percentile_90_velo_2022, na.rm = TRUE), percentile_90_velo_2022),
    percentile_90_velo_2023 = ifelse(is.na(percentile_90_velo_2023), mean(percentile_90_velo_2023, na.rm = TRUE), percentile_90_velo_2023)
  )

batters_faced <- batters_faced %>% 
  left_join(fastball_velo, by = 'pitcher')

remove(fastball_velo)
#### KMeans PA####
kmeans_df <- plate_appearances %>% 
  filter(!(playing_time_2022 == 0 & playing_time_2023 == 0)) %>%  #assumed retired
  filter(years_since_debut_23 > 0 | !(playing_time_2022 == 0 & playing_time_2021 == 0)) %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2022 = ifelse(years_since_debut_23 == 1, 1,0),
         rookie_2021 = ifelse(years_since_debut_23 == 2, 1,0)) %>% 
  select(starts_with('playing_time_prop_of_season_'), starts_with('playing_time_'), rookie_2022, rookie_2021,
         batter) %>% 
  select(-c(ends_with('_2023'), 'playing_time_2022','playing_time_2021'),'playing_time_2023') %>% 
  drop_na(rookie_2021)


set.seed(101);optimal_clusters <- Optimal_Clusters_KMeans(kmeans_df %>% select(-batter, -playing_time_2023) %>% 
                                                            mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                          criterion = 'WCSSE',
                                                          max_clusters = 30,
                                                          num_init = 100,
                                                          max_iters = 300, 
                                                          seed = 101,
                                                          verbose = TRUE,
                                                          plot_clusters = FALSE) 

wcsse <- tibble(
  'Cluster' = 1:30,
  'WCSSE' = optimal_clusters
) 

wcsse%>% 
  ggplot(aes(Cluster, WCSSE)) +
  geom_col(fill = 'blue') + 
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30, by = 2))

wcsse <- wcsse %>% 
  mutate(
    derivative = WCSSE-lag(WCSSE)
  ) #18


set.seed(101);kmeans_vec <- KMeans_rcpp(kmeans_df %>% select(-batter, -playing_time_2023) %>% 
                                          mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                        clusters = 18,
                                        num_init = 100,
                                        max_iters = 300, 
                                        seed = 101,
                                        verbose = FALSE)

kmeans_df$cluster_2023 <- kmeans_vec$clusters


kmeans_summary <- kmeans_df %>% 
  select(-batter) %>% 
  group_by(cluster_2023) %>% 
  reframe(across(everything(), ~mean(.)))

plate_appearances <- plate_appearances %>% 
  left_join(kmeans_df %>% select(batter, cluster_2023), by = 'batter')
#### KMeans SP Playing Time####
kmeans_df_startingpitcher <- batters_faced %>% 
  filter(!(playing_time_2022 == 0 & playing_time_2023 == 0)) %>%  #assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2022 == 0 & playing_time_2021 == 0), role_key_2023 == 'SP') %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2022 = ifelse(years_since_debut_23 == 1, 1,0),
         rookie_2021 = ifelse(years_since_debut_23 == 2, 1,0)) %>% 
  select(starts_with('playing_time_'), rookie_2022, rookie_2021,
         pitcher) %>% 
  select(-c(ends_with('_2023'), contains('_prop_'),'playing_time_2022','playing_time_2021'),'playing_time_2023') %>% 
  drop_na(rookie_2021)


set.seed(101);optimal_clusters <- Optimal_Clusters_KMeans(kmeans_df_startingpitcher %>% select(-pitcher, -playing_time_2023) %>% 
                                                            mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                          criterion = 'WCSSE',
                                                          max_clusters = 30,
                                                          num_init = 100,
                                                          max_iters = 300, 
                                                          seed = 101,
                                                          tol_optimal_init = 100,
                                                          verbose = TRUE,
                                                          plot_clusters = FALSE) 

wcsse <- tibble(
  'Cluster' = 1:30,
  'WCSSE' = optimal_clusters
) 

wcsse%>% 
  ggplot(aes(Cluster, WCSSE)) +
  geom_col(fill = 'blue') + 
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30, by = 2))

wcsse <- wcsse %>% 
  mutate(
    derivative = WCSSE-lag(WCSSE)
  ) #10


set.seed(101);kmeans_vec_sp <- KMeans_rcpp(kmeans_df_startingpitcher %>% select(-pitcher, -playing_time_2023) %>% 
                                             mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                           clusters = 7,
                                           num_init = 100,
                                           max_iters = 300, 
                                           seed = 101,
                                           tol_optimal_init = 100, #distance from each cluster
                                           verbose = FALSE)

kmeans_df_startingpitcher$cluster_2023 <- kmeans_vec_sp$clusters


table(kmeans_df_startingpitcher$cluster_2023) # need to have 20 observations in each cluster (or at least) 6 of the seven

kmeans_summary_sp <- kmeans_df_startingpitcher %>% 
  select(-pitcher) %>% 
  group_by(cluster_2023) %>% 
  reframe(across(everything(), ~mean(.)))

write_csv(kmeans_summary_sp, 'kmeans_summary_sp.csv')

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_startingpitcher %>% reframe(pitcher, cluster_2023_sp = cluster_2023), by = 'pitcher')


#### KMeans SP Prop of Playing Time####
kmeans_df_startingpitcher_prop <- batters_faced %>% 
  filter(!(playing_time_2022 == 0 & playing_time_2023 == 0)) %>%  #assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2022 == 0 & playing_time_2021 == 0), role_key_2023 == 'SP') %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2022 = ifelse(years_since_debut_23 == 1, 1,0),
         rookie_2021 = ifelse(years_since_debut_23 == 2, 1,0)) %>% 
  select(starts_with('playing_time_prop_'), rookie_2022, rookie_2021,playing_time_2023, playing_time_2021,
         pitcher) %>% 
  select(-c(ends_with('_2023'), ends_with('2021')), 'playing_time_2021', 'rookie_2021','playing_time_2023') %>% 
  drop_na(rookie_2021)


set.seed(101);optimal_clusters <- Optimal_Clusters_KMeans(kmeans_df_startingpitcher_prop %>% select(-pitcher, -playing_time_2023) %>% 
                                                            mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                          criterion = 'WCSSE',
                                                          max_clusters = 30,
                                                          num_init = 100,
                                                          max_iters = 300, 
                                                          seed = 101,
                                                          tol_optimal_init = 0.5,
                                                          verbose = TRUE,
                                                          plot_clusters = FALSE) 

wcsse <- tibble(
  'Cluster' = 1:30,
  'WCSSE' = optimal_clusters
) 

wcsse%>% 
  ggplot(aes(Cluster, WCSSE)) +
  geom_col(fill = 'blue') + 
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30, by = 2))

wcsse <- wcsse %>% 
  mutate(
    derivative = WCSSE-lag(WCSSE)
  ) #10


set.seed(101);kmeans_vec_sp_prop <- KMeans_rcpp(kmeans_df_startingpitcher_prop %>% select(-pitcher, -playing_time_2023) %>% 
                                                  mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                clusters = 6,
                                                num_init = 100,
                                                max_iters = 300, 
                                                seed = 101,
                                                tol_optimal_init = 5, #distance from each cluster
                                                verbose = FALSE)

kmeans_df_startingpitcher_prop$cluster_2023 <- kmeans_vec_sp_prop$clusters


table(kmeans_df_startingpitcher_prop$cluster_2023) # need to have 20 observations in each cluster (or at least) 6 of the seven

kmeans_summary_sp_prop <- kmeans_df_startingpitcher_prop %>% 
  select(-pitcher) %>% 
  group_by(cluster_2023) %>% 
  reframe(across(everything(), ~mean(.)))

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_startingpitcher_prop %>% reframe(pitcher, cluster_2023_sp_prop = cluster_2023), by = 'pitcher')


#### KMeans RP Playing Time####
kmeans_df_reliefpitcher <- batters_faced %>% 
  filter(!(playing_time_2022 == 0 & playing_time_2023 == 0)) %>%  #assumed retired
  filter(years_since_debut_23 > 0, !(playing_time_2022 == 0 & playing_time_2021 == 0), role_key_2023 == 'RP') %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2022 = ifelse(years_since_debut_23 == 1, 1,0),
         rookie_2021 = ifelse(years_since_debut_23 == 2, 1,0)) %>% 
  select(starts_with('playing_time_'), rookie_2022, rookie_2021,
         pitcher) %>% 
  select(-c(ends_with('_2023'), 'playing_time_2022','playing_time_2021'),'playing_time_2023') %>% 
  drop_na(rookie_2021)


set.seed(101);optimal_clusters <- Optimal_Clusters_KMeans(kmeans_df_reliefpitcher %>% select(-pitcher, -playing_time_2023) %>% 
                                                            mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                          criterion = 'WCSSE',
                                                          max_clusters = 30,
                                                          num_init = 100,
                                                          max_iters = 300, 
                                                          seed = 101,
                                                          tol_optimal_init = 1,
                                                          verbose = TRUE,
                                                          plot_clusters = FALSE) 

wcsse_rp <- tibble(
  'Cluster' = 1:30,
  'WCSSE' = optimal_clusters
) 

wcsse_rp %>% 
  ggplot(aes(Cluster, WCSSE)) +
  geom_col(fill = 'blue') + 
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30, by = 2))

wcsse_rp <- wcsse_rp %>% 
  mutate(
    derivative = WCSSE-lag(WCSSE)
  ) #14


set.seed(101);kmeans_vec_rp <- KMeans_rcpp(kmeans_df_reliefpitcher %>% select(-pitcher, -playing_time_2023) %>% 
                                             mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                           clusters = 14,
                                           num_init = 100,
                                           max_iters = 300, 
                                           seed = 101,
                                           tol_optimal_init = 1, #distance from each cluster
                                           verbose = FALSE)

kmeans_df_reliefpitcher$cluster_2023 <- kmeans_vec_rp$clusters


table(kmeans_df_reliefpitcher$cluster_2023) # need to have 20 observations in each cluster (or at least) 6 of the seven

kmeans_summary_rp <- kmeans_df_reliefpitcher %>% 
  select(-pitcher) %>% 
  group_by(cluster_2023) %>% 
  reframe(across(everything(), ~mean(.)))

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_reliefpitcher %>% reframe(pitcher, cluster_2023_rp = cluster_2023), by = 'pitcher')

remove(pitchers, pitchers_in_order, pitches_and_ab_per_game, pitches_per_at_bat,
       in_order, groundball_rate, first_five_games, first_five_games_wide,
       lg_ERA, lg_fip_stats, per_batter_metric, per_game_metrics, players,
       players_wide, positions_df, primary_positions_df, season_earned_runs,
       times_through) #saves space in R Environment

#### PA Clustering 2024 ####
kmeans_df_2024 <- plate_appearances %>% 
  filter(batter %in% unique(submission$PLAYER_ID)) %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2023 = ifelse(years_since_debut_24 == 1, 1,0),
         rookie_2022 = ifelse(years_since_debut_24 == 2, 1,0)) %>% 
  select(starts_with('playing_time_prop_of_season_'), starts_with('playing_time_'), rookie_2023, rookie_2022,
         batter) %>% 
  select(-c(ends_with('_2021'), 'playing_time_2022','playing_time_2023'))

glimpse(kmeans_df_2024)

kmeans_df_2024 <- kmeans_df_2024 %>% 
  rename('playing_time_prop_of_season_4_2yrsago' = 'playing_time_prop_of_season_4_2022',
         'playing_time_prop_of_season_5_2yrsago' = 'playing_time_prop_of_season_5_2022',
         'playing_time_prop_of_season_6_2yrsago' = 'playing_time_prop_of_season_6_2022',
         'playing_time_prop_of_season_7_2yrsago' = 'playing_time_prop_of_season_7_2022',
         'playing_time_prop_of_season_8_2yrsago' = 'playing_time_prop_of_season_8_2022',
         'playing_time_prop_of_season_9_2yrsago' = 'playing_time_prop_of_season_9_2022',
         'playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2023',
         'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2023',
         'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2023',
         'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2023',
         'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2023',
         'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2023',
         'playing_time_4_2yrsago' = 'playing_time_4_2022',
         'playing_time_5_2yrsago' = 'playing_time_5_2022',
         'playing_time_6_2yrsago' = 'playing_time_6_2022',
         'playing_time_7_2yrsago' = 'playing_time_7_2022',
         'playing_time_8_2yrsago' = 'playing_time_8_2022',
         'playing_time_9_2yrsago' = 'playing_time_9_2022',
         'playing_time_4_prevseason' = 'playing_time_4_2023',
         'playing_time_5_prevseason' = 'playing_time_5_2023',
         'playing_time_6_prevseason' = 'playing_time_6_2023',
         'playing_time_7_prevseason' = 'playing_time_7_2023',
         'playing_time_8_prevseason' = 'playing_time_8_2023',
         'playing_time_9_prevseason' = 'playing_time_9_2023',
         'rookie_2yrsago' = 'rookie_2022',
         'rookie_prevseason' = 'rookie_2023'
  )

set.seed(101);kmeans_vec_2024 <- KMeans_rcpp(kmeans_df %>% select(-batter, -playing_time_2023, -cluster_2023) %>% 
                                               rename('playing_time_prop_of_season_4_2yrsago' = 'playing_time_prop_of_season_4_2021',
                                                      'playing_time_prop_of_season_5_2yrsago' = 'playing_time_prop_of_season_5_2021',
                                                      'playing_time_prop_of_season_6_2yrsago' = 'playing_time_prop_of_season_6_2021',
                                                      'playing_time_prop_of_season_7_2yrsago' = 'playing_time_prop_of_season_7_2021',
                                                      'playing_time_prop_of_season_8_2yrsago' = 'playing_time_prop_of_season_8_2021',
                                                      'playing_time_prop_of_season_9_2yrsago' = 'playing_time_prop_of_season_9_2021',
                                                      'playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2022',
                                                      'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2022',
                                                      'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2022',
                                                      'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2022',
                                                      'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2022',
                                                      'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2022',
                                                      'playing_time_4_2yrsago' = 'playing_time_4_2021',
                                                      'playing_time_5_2yrsago' = 'playing_time_5_2021',
                                                      'playing_time_6_2yrsago' = 'playing_time_6_2021',
                                                      'playing_time_7_2yrsago' = 'playing_time_7_2021',
                                                      'playing_time_8_2yrsago' = 'playing_time_8_2021',
                                                      'playing_time_9_2yrsago' = 'playing_time_9_2021',
                                                      'playing_time_4_prevseason' = 'playing_time_4_2021',
                                                      'playing_time_5_prevseason' = 'playing_time_5_2021',
                                                      'playing_time_6_prevseason' = 'playing_time_6_2021',
                                                      'playing_time_7_prevseason' = 'playing_time_7_2021',
                                                      'playing_time_8_prevseason' = 'playing_time_8_2021',
                                                      'playing_time_9_prevseason' = 'playing_time_9_2021',
                                                      'rookie_2yrsago' = 'rookie_2021',
                                                      'rookie_prevseason' = 'rookie_2022'
                                               ) %>% 
                                               mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                             clusters = 18,
                                             num_init = 100,
                                             max_iters = 300, 
                                             seed = 101,
                                             verbose = FALSE)

#getting additional rows for kmeans clustering
set.seed(101); extra_rows_pa <- sample(1:nrow(kmeans_df_2024), 
                                       size = nrow(kmeans_df)-nrow(kmeans_df_2024), replace = FALSE)

kmeans_df_2024_new <- kmeans_df_2024 %>% 
  bind_rows(kmeans_df_2024[extra_rows_pa, ])

kmeans_df_2024_new$cluster_2024 <- predict(kmeans_vec_2024, kmeans_df_2024_new %>% 
                                             select(-batter) %>% 
                                             mutate(across(-c(starts_with('rookie_')), ~scale(.))))

table(kmeans_df_2024_new$cluster_2024)

kmeans_summary_2024 <- kmeans_df_2024_new %>% 
  select(-batter) %>% 
  group_by(cluster_2024) %>% 
  reframe(across(everything(), ~mean(.)))

#getting distinct batter cluster combinations
kmeans_df_2024_new <- kmeans_df_2024_new %>% distinct(batter, cluster_2024)

#write_csv(kmeans_summary_2024, 'kmeans_summary_2024.csv') # for cross comparison with cluster_2023

plate_appearances <- plate_appearances %>% 
  left_join(kmeans_df_2024_new %>% select(batter, cluster_2024), by = 'batter')

submission <- read_csv('sample_submission.csv')
#### KMeans SP Playing Time 2024 ####
kmeans_df_startingpitcher_2024 <- batters_faced %>% 
  filter(pitcher %in% unique(submission$PLAYER_ID),role_key_2024 == 'SP') %>% #removing rookies/players who didn't play in 2021 or 2022
  mutate(rookie_2023 = ifelse(years_since_debut_24 == 1, 1,0),
         rookie_2022 = ifelse(years_since_debut_24 == 2, 1,0)) %>% 
  select(starts_with('playing_time_'), rookie_2023, rookie_2022,
         pitcher) %>% 
  select(-c(ends_with('_2021'), contains('_prop_'),'playing_time_2022','playing_time_2023')) 

glimpse(kmeans_df_startingpitcher_2024)

kmeans_df_startingpitcher_2024 <- kmeans_df_startingpitcher_2024 %>% 
  rename('playing_time_4_2yrsago' = 'playing_time_4_2022',
         'playing_time_5_2yrsago' = 'playing_time_5_2022',
         'playing_time_6_2yrsago' = 'playing_time_6_2022',
         'playing_time_7_2yrsago' = 'playing_time_7_2022',
         'playing_time_6_2yrsago' = 'playing_time_6_2022',
         'playing_time_7_2yrsago' = 'playing_time_7_2022',
         'playing_time_8_2yrsago' = 'playing_time_8_2022',
         'playing_time_9_2yrsago' = 'playing_time_9_2022',
         'playing_time_4_prevseason' = 'playing_time_4_2023',
         'playing_time_5_prevseason' = 'playing_time_5_2023',
         'playing_time_6_prevseason' = 'playing_time_6_2023',
         'playing_time_7_prevseason' = 'playing_time_7_2023',
         'playing_time_8_prevseason' = 'playing_time_8_2023',
         'playing_time_9_prevseason' = 'playing_time_9_2023',
         'rookie_2yrsago' = 'rookie_2022',
         'rookie_prevseason' = 'rookie_2023')


set.seed(101);kmeans_vec_sp_2024 <- KMeans_rcpp(kmeans_df_startingpitcher %>% select(-pitcher, -playing_time_2023,
                                                                                     -cluster_2023) %>% 
                                                  rename('playing_time_4_2yrsago' = 'playing_time_4_2021',
                                                         'playing_time_5_2yrsago' = 'playing_time_5_2021',
                                                         'playing_time_6_2yrsago' = 'playing_time_6_2021',
                                                         'playing_time_7_2yrsago' = 'playing_time_7_2021',
                                                         'playing_time_6_2yrsago' = 'playing_time_6_2021',
                                                         'playing_time_7_2yrsago' = 'playing_time_7_2021',
                                                         'playing_time_8_2yrsago' = 'playing_time_8_2021',
                                                         'playing_time_9_2yrsago' = 'playing_time_9_2021',
                                                         'playing_time_4_prevseason' = 'playing_time_4_2022',
                                                         'playing_time_5_prevseason' = 'playing_time_5_2022',
                                                         'playing_time_6_prevseason' = 'playing_time_6_2022',
                                                         'playing_time_7_prevseason' = 'playing_time_7_2022',
                                                         'playing_time_8_prevseason' = 'playing_time_8_2022',
                                                         'playing_time_9_prevseason' = 'playing_time_9_2022',
                                                         'rookie_2yrsago' = 'rookie_2021',
                                                         'rookie_prevseason' = 'rookie_2022') %>% 
                                                  mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                clusters = 7,
                                                num_init = 100,
                                                max_iters = 300, 
                                                seed = 101,
                                                tol_optimal_init = 100, #distance from each cluster
                                                verbose = FALSE)

#resampling rows so kmeans can predict on new data
set.seed(101); extra_rows_sp <- sample(1:nrow(kmeans_df_startingpitcher_2024), 
                                       size = nrow(kmeans_df_startingpitcher)-nrow(kmeans_df_startingpitcher_2024), replace = FALSE)

kmeans_df_startingpitcher_2024_new <- kmeans_df_startingpitcher_2024 %>% 
  bind_rows(kmeans_df_startingpitcher_2024[extra_rows_sp, ])

kmeans_df_startingpitcher_2024_new$cluster_2024 <- predict(kmeans_vec_sp_2024,
                                                           kmeans_df_startingpitcher_2024_new %>% 
                                                             select(-pitcher) %>% 
                                                             mutate(across(-c(starts_with('rookie_')), ~scale(.))))

table(kmeans_df_startingpitcher_2024_new$cluster_2024) # 
table(kmeans_df_startingpitcher$cluster_2023)

kmeans_summary_sp_2024 <- kmeans_df_startingpitcher_2024_new %>% 
  select(-pitcher) %>% 
  group_by(cluster_2024) %>% 
  reframe(across(everything(), ~mean(.)))

write_csv(kmeans_summary_sp_2024, 'kmeans_summary_sp_2024.csv')

kmeans_df_startingpitcher_2024_new <- kmeans_df_startingpitcher_2024_new %>% 
  distinct(pitcher, cluster_2024)

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_startingpitcher_2024_new %>% reframe(pitcher, cluster_2024_sp = cluster_2024), by = 'pitcher')
#### KMeans SP Prop of Playing Time 2024 ####
kmeans_df_startingpitcher_prop_2024 <- batters_faced %>% 
  filter(pitcher %in% unique(submission$PLAYER_ID),role_key_2024 == 'SP') %>%
  mutate(rookie_2023 = ifelse(years_since_debut_24 == 1, 1,0),
         rookie_2022 = ifelse(years_since_debut_24 == 2, 1,0)) %>% 
  select(starts_with('playing_time_prop_'), rookie_2023, rookie_2022,playing_time_2022,
         pitcher) %>% 
  select(-c(ends_with('_2022'), ends_with('2021')), 'playing_time_2022', 'rookie_2022')

glimpse(kmeans_df_startingpitcher_prop_2024)

kmeans_df_startingpitcher_prop_2024 <- kmeans_df_startingpitcher_prop_2024 %>% 
  rename('playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2023',
         'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2023',
         'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2023',
         'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2023',
         'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2023',
         'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2023',
         'playing_time_2yrsago' = 'playing_time_2022',
         'rookie_2yrsago' = 'rookie_2022',
         'rookie_prevseason' = 'rookie_2023')

set.seed(101);kmeans_vec_sp_prop_2024 <- KMeans_rcpp(kmeans_df_startingpitcher_prop %>% select(-pitcher, -playing_time_2023,-cluster_2023) %>%
                                                       rename('playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2022',
                                                              'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2022',
                                                              'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2022',
                                                              'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2022',
                                                              'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2022',
                                                              'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2022',
                                                              'playing_time_2yrsago' = 'playing_time_2021',
                                                              'rookie_2yrsago' = 'rookie_2021',
                                                              'rookie_prevseason' = 'rookie_2022') %>% 
                                                       mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                     clusters = 6,
                                                     num_init = 100,
                                                     max_iters = 300, 
                                                     seed = 101,
                                                     tol_optimal_init = 5, #distance from each cluster
                                                     verbose = FALSE)
#adding extra rows
set.seed(101);extra_rows_sp_prop <- sample(1:nrow(kmeans_df_startingpitcher_prop_2024), 
                                           size = nrow(kmeans_df_startingpitcher_prop) - nrow(kmeans_df_startingpitcher_prop_2024),
                                           replace = FALSE)
kmeans_df_startingpitcher_prop_2024_new <- kmeans_df_startingpitcher_prop_2024 %>% 
  bind_rows(kmeans_df_startingpitcher_prop_2024[extra_rows_sp_prop,])

kmeans_df_startingpitcher_prop_2024_new$cluster_2024 <- predict(kmeans_vec_sp_prop_2024, kmeans_df_startingpitcher_prop_2024_new %>% 
                                                                  select(-pitcher) %>% 
                                                                  mutate(across(-c(starts_with('rookie_')), ~scale(.))))

table(kmeans_df_startingpitcher_prop_2024_new$cluster_2024)
table(kmeans_df_startingpitcher_prop$cluster_2023) # comparison

kmeans_summary_sp_prop_2024 <- kmeans_df_startingpitcher_prop_2024_new %>% 
  select(-pitcher) %>% 
  group_by(cluster_2024) %>% 
  reframe(across(everything(), ~mean(.)))

kmeans_df_startingpitcher_prop_2024_new <- kmeans_df_startingpitcher_prop_2024_new %>% 
  distinct(pitcher, cluster_2024)

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_startingpitcher_prop_2024_new %>% reframe(pitcher, cluster_2024_sp_prop = cluster_2024), by = 'pitcher')




#### KMeans RP Playing Time 2024 ####
kmeans_df_reliefpitcher_2024 <- batters_faced %>% 
  filter(pitcher %in% unique(submission$PLAYER_ID), role_key_2024 == 'RP') %>%
  mutate(rookie_2023 = ifelse(years_since_debut_24 == 1, 1,0),
         rookie_2022 = ifelse(years_since_debut_24 == 2, 1,0)) %>% 
  select(starts_with('playing_time_'), rookie_2023, rookie_2022,
         pitcher) %>% 
  select(-c(ends_with('_2021'), 'playing_time_2022', 'playing_time_2023'))

glimpse(kmeans_df_reliefpitcher_2024)

kmeans_df_reliefpitcher_2024 <- kmeans_df_reliefpitcher_2024 %>% 
  rename('playing_time_prop_of_season_4_2yrsago' = 'playing_time_prop_of_season_4_2022',
         'playing_time_prop_of_season_5_2yrsago' = 'playing_time_prop_of_season_5_2022',
         'playing_time_prop_of_season_6_2yrsago' = 'playing_time_prop_of_season_6_2022',
         'playing_time_prop_of_season_7_2yrsago' = 'playing_time_prop_of_season_7_2022',
         'playing_time_prop_of_season_8_2yrsago' = 'playing_time_prop_of_season_8_2022',
         'playing_time_prop_of_season_9_2yrsago' = 'playing_time_prop_of_season_9_2022',
         'playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2023',
         'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2023',
         'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2023',
         'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2023',
         'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2023',
         'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2023',
         'playing_time_4_2yrsago' = 'playing_time_4_2022',
         'playing_time_5_2yrsago' = 'playing_time_5_2022',
         'playing_time_6_2yrsago' = 'playing_time_6_2022',
         'playing_time_7_2yrsago' = 'playing_time_7_2022',
         'playing_time_8_2yrsago' = 'playing_time_8_2022',
         'playing_time_9_2yrsago' = 'playing_time_9_2022',
         'playing_time_4_prevseason' = 'playing_time_4_2023',
         'playing_time_5_prevseason' = 'playing_time_5_2023',
         'playing_time_6_prevseason' = 'playing_time_6_2023',
         'playing_time_7_prevseason' = 'playing_time_7_2023',
         'playing_time_8_prevseason' = 'playing_time_8_2023',
         'playing_time_9_prevseason' = 'playing_time_9_2023',
         'rookie_2yrsago' = 'rookie_2022',
         'rookie_prevseason' = 'rookie_2023'
  )

set.seed(101);kmeans_vec_rp_2024 <- KMeans_rcpp(kmeans_df_reliefpitcher %>% select(-pitcher, -playing_time_2023, -cluster_2023) %>%
                                                  rename('playing_time_prop_of_season_4_2yrsago' = 'playing_time_prop_of_season_4_2021',
                                                         'playing_time_prop_of_season_5_2yrsago' = 'playing_time_prop_of_season_5_2021',
                                                         'playing_time_prop_of_season_6_2yrsago' = 'playing_time_prop_of_season_6_2021',
                                                         'playing_time_prop_of_season_7_2yrsago' = 'playing_time_prop_of_season_7_2021',
                                                         'playing_time_prop_of_season_8_2yrsago' = 'playing_time_prop_of_season_8_2021',
                                                         'playing_time_prop_of_season_9_2yrsago' = 'playing_time_prop_of_season_9_2021',
                                                         'playing_time_prop_of_season_4_prevseason' = 'playing_time_prop_of_season_4_2022',
                                                         'playing_time_prop_of_season_5_prevseason' = 'playing_time_prop_of_season_5_2022',
                                                         'playing_time_prop_of_season_6_prevseason' = 'playing_time_prop_of_season_6_2022',
                                                         'playing_time_prop_of_season_7_prevseason' = 'playing_time_prop_of_season_7_2022',
                                                         'playing_time_prop_of_season_8_prevseason' = 'playing_time_prop_of_season_8_2022',
                                                         'playing_time_prop_of_season_9_prevseason' = 'playing_time_prop_of_season_9_2022',
                                                         'playing_time_4_2yrsago' = 'playing_time_4_2021',
                                                         'playing_time_5_2yrsago' = 'playing_time_5_2021',
                                                         'playing_time_6_2yrsago' = 'playing_time_6_2021',
                                                         'playing_time_7_2yrsago' = 'playing_time_7_2021',
                                                         'playing_time_8_2yrsago' = 'playing_time_8_2021',
                                                         'playing_time_9_2yrsago' = 'playing_time_9_2021',
                                                         'playing_time_4_prevseason' = 'playing_time_4_2021',
                                                         'playing_time_5_prevseason' = 'playing_time_5_2021',
                                                         'playing_time_6_prevseason' = 'playing_time_6_2021',
                                                         'playing_time_7_prevseason' = 'playing_time_7_2021',
                                                         'playing_time_8_prevseason' = 'playing_time_8_2021',
                                                         'playing_time_9_prevseason' = 'playing_time_9_2021',
                                                         'rookie_2yrsago' = 'rookie_2021',
                                                         'rookie_prevseason' = 'rookie_2022'
                                                  ) %>% 
                                                  mutate(across(-c(starts_with('rookie_')), ~scale(.))),
                                                clusters = 14,
                                                num_init = 100,
                                                max_iters = 300, 
                                                seed = 101,
                                                tol_optimal_init = 1, #distance from each cluster
                                                verbose = FALSE)


#adding rows to kmeans_relief pitcher for kmeans model
set.seed(101);extra_rows_rp <- sample(1:nrow(kmeans_df_reliefpitcher_2024), 
                                      size = nrow(kmeans_df_reliefpitcher) - nrow(kmeans_df_reliefpitcher_2024),
                                      replace = FALSE)
kmeans_df_reliefpitcher_2024_new <- kmeans_df_reliefpitcher_2024 %>% 
  bind_rows(kmeans_df_reliefpitcher_2024[extra_rows_rp,])

kmeans_df_reliefpitcher_2024_new$cluster_2024 <- predict(kmeans_vec_rp_2024,
                                                         kmeans_df_reliefpitcher_2024_new %>% 
                                                           select(-pitcher) %>% 
                                                           mutate(across(-c(starts_with('rookie_')), ~scale(.))))

table(kmeans_df_reliefpitcher_2024_new$cluster_2024)
table(kmeans_df_reliefpitcher$cluster_2023) 

kmeans_summary_rp_2024 <- kmeans_df_reliefpitcher_2024_new %>% 
  select(-pitcher) %>% 
  group_by(cluster_2024) %>% 
  reframe(across(everything(), ~mean(.)))

kmeans_df_reliefpitcher_2024_new <- kmeans_df_reliefpitcher_2024_new %>% 
  distinct(pitcher, cluster_2024)

batters_faced <- batters_faced %>% 
  left_join(kmeans_df_reliefpitcher_2024_new %>% reframe(pitcher, cluster_2024_rp = cluster_2024), by = 'pitcher')

