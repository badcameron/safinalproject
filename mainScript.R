library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(stringr)
setwd("~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2024-25/Sport Analytics/final proj data")
data <- fread("stats.csv")

data <- data %>% 
  mutate(Name = str_c(str_split(`last_name, first_name`, ", ", simplify = TRUE)[,2], 
                                str_split(`last_name, first_name`, ", ", simplify = TRUE)[,1], sep = " ")) %>% 
  select(-`last_name, first_name`)

#Data for MLB seasons 2021-2024 (only on qualified hitters)
data24 <- baseball_data_2024
data23 <- baseball_data_2023
data22 <- baseball_data_2022
data21 <- baseball_data_2021

#Create vector of protected names from last two images
protected_names <- c(
  "Bobby Witt Jr.",
  "Vladimir Guerrero Jr.",
  "Jazz Chisholm Jr.",
  "Luis García Jr.",
  "Lourdes Gurriel Jr.",
  "Ronald Acuña Jr.",
  "Luis Robert Jr.",
  "Fernando Tatis Jr.",
  "LaMonte Wade Jr."
)

#Process names in the data table
data$Name <- ifelse(
  data$Name %in% protected_names,
  data$Name,
  ifelse(
    grepl("Jr\\.$", data$Name),
    trimws(sub("Jr\\.$", "", data$Name)),
    data$Name
  )
)

#Merge data21-data24 to get a new and better version of data 
merged_data <- rbindlist(list(data24, data23, data22, data21), use.names = TRUE, fill = TRUE)

final_merged_data <- merge(merged_data, data, by = c("Name", "year"), all = TRUE) %>% 
  select(Name, year, `BB%`, `K%`, AVG, `wRC+`, k_percent, bb_percent, on_base_percent, on_base_plus_slg, 
         babip, exit_velocity_avg, launch_angle_avg, hard_hit_percent, z_swing_percent, whiff_percent, 
         groundballs_percent, flyballs_percent)

data_processed <- final_merged_data %>%
  # Arrange by player and season to calculate changes
  arrange(Name, year) %>%
  group_by(Name) %>%
  # Calculate the change in wRC+ for next season
  mutate(
    year_next = year + 1,
    wRCplus_next = lead(`wRC+`),
    wRCplus_change = wRCplus_next - `wRC+`
  ) %>%
  # Remove rows where we don't have next season's data
  filter(!is.na(wRCplus_change))





