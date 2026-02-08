# cleaning script for the real data

library(tidyverse)
library(here)
library(janitor)


files <- list.files(here("FOLDER"), pattern = ".*\\.csv", full.names = T)

# combines all the data files into one dataframe
dt <-
  files |> 
  map(\(f) read_csv(f)) |> 
  list_rbind() |> 
  # removes everything that is not a legit trial (anything else should not be needed)
  filter(!is.na(spaceHoldDur_custom) & !is.na(trialsLoop.thisTrialN)) |> 
  clean_names() |>
  # cleans empty and constant rows and columns
  remove_empty() |>
  # removing constant columns breaks if there is only one participant
  remove_constant() |> 
  group_by(participant) |> 
  # adds clear trial number (should be 1 to 72 for each participant)
  mutate(trial_number = row_number(), .after = 2) |> 
  ungroup() |> 
  mutate(
    response = space_hold_dur_custom,
    tempo = beats_per_second,
    participant = as.character(participant)
  )


dt_clean <- 
  dt |> 
  # select variables relevant for analysis
  select(
    participant, group, trial_number, trial_id, tempo, 
    interval_length, beat_count, response, date
  ) |> 
  mutate(
    response_log = log(response),
    response_ratio = response / interval_length,
    response_difference  = response - interval_length,
    group = factor(group, levels = c("slow", "normal", "fast"))
  )


write_rds(dt_clean, file = here("data_cleaning/data_clean.rds"))
