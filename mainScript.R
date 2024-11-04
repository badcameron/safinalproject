library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(stringr)
setwd("~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2024-25/Sport Analytics/final proj data")
data <- fread("stats.csv")

data <- data %>% 
  mutate(name = str_c(str_split(`last_name, first_name`, ", ", simplify = TRUE)[,2], 
                                str_split(`last_name, first_name`, ", ", simplify = TRUE)[,1], sep = " ")) %>% 
  select(-`last_name, first_name`)

