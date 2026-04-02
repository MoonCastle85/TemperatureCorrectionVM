# Swiss heat prediction pipeline
# Fits a hybrid LM + KNN model on historical heating profiles, then predicts
# hourly heating load on normal-year temperatures.

library(tidyverse)
library(tidymodels)
library(janitor)
library(skimr)

pred_functions <- list.files("./Functions", pattern = ".R", full.names = TRUE)
walk(pred_functions, source)

# =================
# Load data
# =================
orig_profiles <- read_csv2("original_profiles_gbg.csv", show_col_types = FALSE)

orig_profiles_prepared <- orig_profiles %>%
  clean_names() %>%
  rename(month = manad, weekday = veckodag, day = dag, time = klockslag, hour = timme, temperature = utetemp, 
         heat_load_kw = varmeprofil_foretag_aldre_flerbostadshus) %>%
  select(month, weekday, day, time, hour, temperature, heat_load_kw) %>%
  mutate(heat_load_kw = heat_load_kw * 193000,
         is_weekend = if_else(weekday >= 6, 1, 0))

temp_files_url <- list.files("./Temperatures for prediction", full.names = TRUE)
temp_files <- map(temp_files_url, ~read_csv2(.x, skip = 2, col_types = cols(.default = col_character()), show_col_types = FALSE)) %>%
  list_rbind()

temp_files_prepared <- temp_files %>%
  clean_names() %>%
  rename(year = number_year, time = hour, temperature = dry_bulb_temperature_c) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-")),
         weekday = wday(date, week_start = getOption("lubridata.week.start", 1))) %>%
  select(year, month, weekday, day, time, temperature) %>%
  mutate(across(everything(), as.numeric),
         is_weekend = if_else(weekday >= 6, 1, 0)) %>%
  mutate(hour = row_number(), .by = year) %>%
  nest(data = everything(), .by = year)

# =================
# Predict and check
# =================

model_fits <- fit_lm_and_knn(orig_profiles_prepared, stop_temp_c = 15)

heating_predictions <- map(temp_files_prepared$data, ~pred_lm_and_knn(.x, model_fits, stop_temp_c = 15)) 
heating_predictions2 <- heating_predictions %>%
  list_rbind()

summer_predictions <- map(temp_files_prepared$data, ~get_pred_load_summer(data = orig_profiles_prepared, new = .x))
summer_predictions2 <- summer_predictions %>%
  list_rbind()

all_predictions <- rbind(heating_predictions2, summer_predictions2)
skim(all_predictions)

ggplot() +
  geom_point(data = orig_profiles_prepared, aes(x = temperature, y = heat_load_kw, colour = "Original")) +
  geom_point(data = all_predictions, aes(x = temperature, y = pred_load_new, colour = "Anpassad")) +
  scale_colour_manual(values = c("Original" = "blue", "Anpassad" = "red"), name = "Källa") +
  labs(x = "Utomhustemperatur per timme [°C]", y = "Värmeeffekt (inkl varmvatten) per timme [kW]") +
  facet_wrap(~year) +
  theme_minimal()

# =================
# Save
# =================

write_csv2(all_predictions, file = "Outputs/Värmeprofiler GBG 2015-2025.csv")
