library(tidyverse)
library(here)
library(janitor)


# manually found usable files for preliminary test analysis
files <- c(
  "experiment_psychopy/2026-01-12_Experiment/data/342824_experiment_2026-01-15_11h05.48.600.csv",
  "experiment_psychopy/2026-01-12_Experiment/data/99887766_experiment_2026-01-15_09h35.45.715.csv"
)

file_old <- "experiment_psychopy/2026-01-12_Experiment/data/pilot_experiment_2026-01-06_10h49.47.084_kaplan.csv"


# extracts relevant data from all files
dt <-
  files |> 
  map(\(f) read_csv(f, 
                    col_select = c(participant, textbox.text),
                    col_types = cols(participant = "c", textbox.text = "c"))) |> 
  list_rbind() |> 
  clean_names() |>
  filter(!is.na(textbox_text))


# I have to do this because data from older version have different structure
dt_old <- 
  read_csv(here(file_old), 
                col_select = c(participant, textbox.text),
                col_types = cols(participant = "c", textbox.text = "c")) |> 
  clean_names() |>
  filter(!is.na(textbox_text))


dt_text <- 
  dt |> 
  bind_rows(dt_old)

write_rds(dt_text, here("data_textbox_test/data_text_test.rds"))
