library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(stringr)
library(caret)
library(lme4)
library(xgboost)
library(randomForest)
library(ltm)

# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2024-25/Sport Analytics/final proj data")

# Read data
data <- fread("stats.csv")

# Process names (similar to original script)
data <- data %>%
  mutate(Name = str_c(str_split(`last_name, first_name`, ", ", simplify = TRUE)[,2],
                      str_split(`last_name, first_name`, ", ", simplify = TRUE)[,1], sep = " ")) %>%
  dplyr::select(-`last_name, first_name`)


data %>%
  # Remove percentage signs and convert to numeric
  mutate(
    BBpercent = as.numeric(str_replace(BBpercent, "%", "")),
    Kpercent = as.numeric(str_replace(Kpercent, "%", "")),
    # Ensure all relevant columns are numeric
    across(c(AVG, k_percent, bb_percent, on_base_percent, 
             on_base_plus_slg, babip, exit_velocity_avg, 
             launch_angle_avg, hard_hit_percent, z_swing_percent, 
             whiff_percent, groundballs_percent, flyballs_percent), 
           as.numeric)
  ) %>%
  # Remove rows with any NA values in key columns
  drop_na(BBpercent, Kpercent, wRCplus, wRCplus_next)


# Read year-specific data
data24 <- fread("baseball data - 2024.csv") %>% 
  rename(
    wRCplus = `wRC+`,
    BBpercent = `BB%`,
    Kpercent = `K%`
  )
data23 <- fread("baseball data - 2023.csv") %>% 
  rename(
    wRCplus = `wRC+`,
    BBpercent = `BB%`,
    Kpercent = `K%`
  )
data22 <- fread("baseball data - 2022.csv") %>% 
  rename(
    wRCplus = `wRC+`,
    BBpercent = `BB%`,
    Kpercent = `K%`
  )
data21 <- fread("baseball data - 2021.csv") %>% 
  rename(
    wRCplus = `wRC+`,
    BBpercent = `BB%`,
    Kpercent = `K%`
  )

# Protected names handling (as in original script)
protected_names <- c(
  "Bobby Witt Jr.", "Vladimir Guerrero Jr.", "Jazz Chisholm Jr.",
  "Luis García Jr.", "Lourdes Gurriel Jr.", "Ronald Acuña Jr.",
  "Luis Robert Jr.", "Fernando Tatis Jr.", "LaMonte Wade Jr."
)

data$Name <- ifelse(
  data$Name %in% protected_names,
  data$Name,
  ifelse(
    grepl("Jr\\.$", data$Name),
    trimws(sub("Jr\\.$", "", data$Name)),
    data$Name
  )
)

# Merge data across years
merged_data <- rbindlist(list(data24, data23, data22, data21), use.names = TRUE, fill = TRUE)

# Final merged data preparation
final_merged_data <- merge(merged_data, data, by = c("Name", "year"), all = TRUE) %>%
  dplyr::select(Name, year, BBpercent, Kpercent, AVG, wRCplus, k_percent, bb_percent, 
         on_base_percent, on_base_plus_slg, babip, exit_velocity_avg, 
         launch_angle_avg, hard_hit_percent, z_swing_percent, whiff_percent,
         groundballs_percent, flyballs_percent)

# Prepare data for prediction of next year's wRC+
data_processed <- final_merged_data %>%
  arrange(Name, year) %>%
  group_by(Name) %>%
  mutate(
    year_next = year + 1,
    wRCplus_next = lead(wRCplus)
  ) %>%
  # Remove rows where we don't have next season's data
  filter(!is.na(wRCplus_next))


data_processed <- data_processed %>%
  # Remove percentage signs and convert to numeric
  mutate(
    BBpercent = as.numeric(str_replace(BBpercent, "%", "")),
    Kpercent = as.numeric(str_replace(Kpercent, "%", "")),
    # Ensure all relevant columns are numeric
    across(c(AVG, k_percent, bb_percent, on_base_percent, 
             on_base_plus_slg, babip, exit_velocity_avg, 
             launch_angle_avg, hard_hit_percent, z_swing_percent, 
             whiff_percent, groundballs_percent, flyballs_percent), 
           as.numeric)
  ) %>%
  # Remove rows with any NA values in key columns
  drop_na(BBpercent, Kpercent, wRCplus, wRCplus_next)

data_processed <- na.omit(data_processed)

# Define predictors (expanded from original)
predictors <- c(
  "wRCplus", "bb_percent", "k_percent", "AVG", "babip",
  "exit_velocity_avg", "launch_angle_avg", "hard_hit_percent",
  "whiff_percent", "groundballs_percent", "on_base_percent", 
  "on_base_plus_slg"
)




# Prepare data for modeling
model_data <- data_processed %>%
  dplyr::select(Name, year, wRCplus_next, all_of(predictors)) %>%
  drop_na()

X <- model_data[, !(names(model_data) %in% c("Name", "year", "wRCplus_next"))]

X <- model_data 

X<- as.matrix(X)

y <- model_data$wRCplus_next

# Set seed for reproducibility
set.seed(123)

# Feature Importance Random Forest
rf_model <- randomForest(x = X, y = y, 
                         importance = TRUE, 
                         ntree = 500, 
                         mtry = floor(sqrt(ncol(X)) *2/3))

# Print feature importance
feature_importance <- data.frame(
  Predictor = predictors,
  Importance = importance(rf_model)[,1]
) %>% 
  arrange(desc(Importance))


ggplot(feature_importance, aes(x = reorder(Predictor, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Feature Importance for Next Season's wRC+",
       x = "Predictors", 
       y = "Importance Score") +
  theme_minimal()


prediction_data <- data_processed %>%
  group_by(Name) %>%
  slice_max(year) %>%
  dplyr::select(Name, year, all_of(predictors)) %>%
  drop_na()


X_pred <- prediction_data %>% 
  dplyr::select(-c(Name, year)) %>%
  as.matrix()

# Predict wRC+ for next season
prediction_data$predicted_wRCplus_next <- predict(rf_model, X_pred)

top_predictions <- prediction_data %>%
  arrange(desc(predicted_wRCplus_next)) %>%
  select(Name, year, predicted_wRCplus_next)




# Cross-validation to validate model performance
control <- trainControl(method = "cv", number = 5)
rf_cv <- train(x = X, y = y, 
               method = "rf", 
               trControl = control,
               importance = TRUE)




