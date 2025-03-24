#install.packages('arrow')
library(tidyverse)


lahman <- read_csv('lahman_people.csv')

statcast <- read_csv('savant_data_2021_2023.csv')

#writing to a parquet file to 
arrow::write_parquet(statcast, 'savant_data_2021_2023.parquet')

#statcast <- arrow::read_parquet('savant_data_2021_2023.parquet')

#read in submission file
submission <- read_csv('sample_submission.csv')
