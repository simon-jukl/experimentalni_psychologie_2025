# cleaning script for the testing data

library(tidyverse)
library(here)
library(janitor)


# manually found usable files for preliminary test analysis
files <- c(
  "experiment_psychopy/2026-01-12_Experiment/data/342824_experiment_2026-01-15_11h05.48.600.csv",
  "experiment_psychopy/2026-01-12_Experiment/data/99887766_experiment_2026-01-15_09h35.45.715.csv"
)

file_old <- "experiment_psychopy/2026-01-12_Experiment/data/pilot_experiment_2026-01-06_10h49.47.084_kaplan.csv"


# combines all the data files into one dataframe
dt <-
  files |> 
  map(\(f) read_csv(here(f))) |> 
  list_rbind() |> 
  # removes everything that is not a legit trial (anything else should not be needed)
  filter(!is.na(spaceHoldDur_custom) & !is.na(trialsLoop.thisTrialN)) |> 
  clean_names() |>
  # cleans empty and constant rows and columns
  remove_empty() |>
  remove_constant() |> 
  group_by(participant) |> 
  mutate(trial_number = row_number(), .after = 2) |> 
  ungroup() |> 
  mutate(
    response = space_hold_dur_custom,
    participant = as.character(participant)
  )


# I have to do this because data from older version have different structure
dt_old <- 
  read_csv(here(file_old)) |> 
  filter(!is.na(keyResponse.duration) & !is.na(trialsLoop.thisTrialN)) |> 
  clean_names() |>
  # cleans empty and constant rows and columns
  remove_empty() |>
  rename(group = group_25) |> 
  mutate(trial_number = row_number(), .after = 2) |> 
  mutate(response = key_response_duration)


dt_clean <- 
  dt |> 
  bind_rows(dt_old) |> 
  # select variables relevant for analysis
  select(
    # subject_id, 
    trial_id, trial_number, beats_per_second, interval_length, 
    beat_count, response, group, participant, date
  ) |> 
  rename(
    tempo = beats_per_second
  ) |> 
  mutate(
    response_log = log(response),
    response_ratio = response / interval_length,
    response_difference  = response - interval_length,
    group = factor(group, levels = c("slow", "normal", "fast"))
  )


write_rds(dt_clean, file = here("data_cleaning_test/data_clean_test.rds"))
