# cleaning script for the real data

library(tidyverse)
library(here)
library(janitor)


# read raw data from both experiments 
# prints out warning about parsing issues - deemed irrelevant after inspection (wrong column type in irrelevant column)  
data_linear_raw <- 
  read_csv(here("data_raw/Database_AllParticipants_time_tempo_linear0126_504210_2026-02-04_12h19.09_aa0e0a55-e6d5-4be0-a110-578b1bdf4c5f.csv")) |> 
  # make names consistent
  clean_names() |> 
  # filter only trials
  filter(!is.na(space_hold_dur_custom) & !is.na(trial_id)) |> 
  # remove constant columns
  remove_constant(quiet = FALSE)
  # unnecessary to remove empty rows and columns - there are none after removing constant
  # remove_empty(which = c("rows", "cols"), quiet = FALSE)
  
  
data_geometric_raw <- 
  read_csv(here("data_raw/Database_AllParticipants_time_tempo_geometric0126_504213_2026-02-04_12h18.46_a2353e53-bc59-4216-8d92-2763ab3fc3f2.csv")) |> 
  clean_names() |> 
  filter(!is.na(space_hold_dur_custom) & !is.na(trial_id)) |> 
  remove_constant(quiet = FALSE)
  # remove_empty(which = c("rows", "cols"), quiet = FALSE)
  



# variable names in the raw data differ between the two experiments
#     janitor::clean_names() solves most differences in capitalization etc.
#     janitor::remove_constant() and janitor::remove_empty() solve the rest of the issues

# names in X that are not in Y
dplyr::setdiff(names(data_linear_raw), names(data_geometric_raw))
dplyr::setdiff(names(data_geometric_raw), names(data_linear_raw))

# names in one not in the other
dplyr::symdiff(names(data_linear_raw), names(data_geometric_raw))



# data_linear_raw
# data_geometric_raw


# some participants didn't complete both experiments
participants_linear <- data_linear_raw |> 
  count(participant_2) |> 
  pull(participant_2)

participants_geometric <- data_geometric_raw |> 
  count(participant_2) |> 
  pull(participant_2)


# participants in linear that are not in 
participants_to_remove <- dplyr::symdiff(participants_linear, participants_geometric)


# remove participants who didn't complete both experiments
data_linear_raw <-
  data_linear_raw |> 
  filter(!(participant_2 %in% participants_to_remove))

data_geometric_raw <- 
  data_geometric_raw |> 
  filter(!(participant_2 %in% participants_to_remove))



# data_linear_raw 
# data_geometric_raw


# combine data from both experiments
data_full <- bind_rows(
  linear = data_linear_raw,
  geometric = data_geometric_raw,
  .id = "experiment"
)


# manual inspection
# data_full |> 
#   count(participant_2, experiment) |> 
#   view()


# remove duplicate participants

n_trials_linear <- 72
n_trials_geometric <- 48

participants_to_remove_duplicate <- 
  data_full |> 
  count(participant_2, experiment) |> 
  filter((n != n_trials_linear & experiment == "linear") | (n != n_trials_geometric & experiment == "geometric")) |> 
  pull(participant_2)

data_full <-
  data_full |> 
  filter(!(participant_2 %in% participants_to_remove_duplicate))


# anonymization of participants who put their real name in
real_names <- 
  data_full |> 
  distinct(participant_2) |> 
  # finds all names/ids that aren't "P###" where "#" is a number, e.g. "P001"
  filter(str_detect(participant_2, pattern = "P\\d+", negate = TRUE)) |> 
  mutate(
    # creates new anonymous id for each name
    # replaces ids from the back to prevent id duplication isssues
    id_replace = str_c("P", str_pad(9999:(9999 - n() + 1), width = 3, side = "left", pad = "0"))
  )

data_full <-
  data_full |> 
  left_join(real_names, by = "participant_2") |> 
  mutate(participant_2 = coalesce(id_replace, participant_2)) |> 
  select(-id_replace)




# select relevant columns and rename for clarity
data_full <-
  data_full |>
  select(-c(feedback_routine_started, feedback_routine_stopped, 
            response_routine_started, response_routine_stopped, 
            stimul_routine_started, stimul_routine_stopped,
            key_feedback_next_keys, key_feedback_next_rt,
            blank500_started, blank500_stopped,
            trials_loop_this_index, trials_loop_this_n, trials_loop_this_trial_n,
            date)) |> 
  rename(
    fatigue = jak_moc_unaveny_a_se_v_tuto_chvili_citite,
    music_train = absolvoval_a_jste_nekdy_formalni_hudebni_vyuku_nastroj_nebo_zpev,
    music_sing = zpivate_pravidelne_solo_nebo_ve_sboru_skupine,
    music_instr = hral_a_jste_nekdy_pravidelne_na_hudebni_nastroj_alespon_nekolik_mesicu,
    gender = pohlavi,
    age = vek,
    participant = participant_2,
    groups = group,
    response = space_hold_dur_custom
  )


data_full <- 
  data_full |> 
  mutate(trial_index = row_number(), .by = c(participant, experiment)) |> 
  mutate(
    experiment = factor(experiment, levels = c("linear", "geometric")),
    experiment_index = factor(if_else(experiment == "linear", 1, 2), levels = c("1", "2")),
    experiment_contr = if_else(experiment == "linear", -0.5, 0.5),
    groups = factor(groups, levels = c("slow", "normal", "fast")),
    gender = case_when(
      gender == "Muž" ~ "M",
      gender == "Žena" ~ "F",
      gender == "Jiné" ~ "O"
    ) |> factor(),
    male = case_when(
      gender == "M" ~ 1,
      gender == "F" ~ 0,
      gender == "O" ~ NA
    ) |> factor(),
    female = case_when(
      gender == "M" ~ 0,
      gender == "F" ~ 1,
      gender == "O" ~ NA
    ) |> factor(),
    gender_index = factor(as.numeric(male)),
    gender_contr = case_when(
      gender == "M" ~ -0.5, 
      gender == "F" ~ 0.5
    ),
    # one participant wrote "19 let", parse to "19"
    age = parse_number(age),
    # participant P034 put age as 10 in one of the experiments -> correcting to 19
    age = if_else(participant == "P034", 19, age),
    # centered age
    age_c = age - mean(age),
    datetime = ymd_hms(datetime),
    
    
    interval_length_log = log(interval_length),
    
    response_log = log(response),
    response_ratio = response / interval_length,
    
    fatigue = factor(fatigue,
                     levels = c("Velmi unavený/á", "Trochu unavený/á", "Ani unavený/á, ani svěží", "Spíše svěží", "Úplně svěží"),
                     ordered = TRUE),
    fatigue_score = as.numeric(fatigue) - 1,
    
    music_train = factor(music_train, 
                         levels = c("Ne", "Ano, kratší dobu (do 2 let)", "Ano, delší dobu (3 a více let)"), 
                         ordered = TRUE),
    music_sing = factor(music_sing,
                        levels = c("Ne, nikdy", "Občas", "Pravidelně"),
                        ordered = TRUE),
    music_instr = factor(music_instr,
                         levels = c("Ne, nikdy", "Ano, v minulosti", "Ano, hraji stále"),
                         ordered = TRUE),
    music_train_score = as.numeric(music_train) - 1,
    music_sing_score = as.numeric(music_sing) - 1,
    music_instr_score = as.numeric(music_instr) - 1,
    music_experience_score = music_train_score + music_sing_score + music_instr_score
  )




# reorder data for clarity
data_full <- 
  data_full |> 
  relocate(
    # participant ID
    participant,
    
    # demographics
    # gender - M, F, O (other)
    gender,
    # gender as index variable (M = 2, F = 1, O = NA)
    gender_index,
    # gender as sum contrast (M = -0.5, f = 0.5)
    gender_contr,
    # M = 1, F = 0, O = NA
    male,
    # F = 1, M = 0, O = NA
    female,
    # age (years)
    age,
    # mean-centered age (age_c = age - mean(age))
    age_c,
    
    # operating system
    os,
    # screen framerate
    frame_rate,
    # date (YYYY-MM-DD HH:mm:ss)
    datetime,
    
    # experiment (linear or geometric scale)
    experiment,
    # experiment as index variable ("linear" = 1, "geometric" = 2)
    experiment_index,
    # experiment as sum contrast ("linear" = -0.5, "geometric" = 0.5)
    experiment_contr,
    # groups (slow, normal, fast)
    groups,
    # trial number (linear = 1:72, geometric = 1:48)
    trial_index,
    # id for specific combination of beats_per_second and interval_length
    trial_id,
    # tempo/frequency/bps
    beats_per_second,
    
    # diagnostic values for bps
    # number of beats in the interval
    beat_count,
    beeps_planned,
    beeps_played,
    beeps_skipped,
    max_beep_lateness_s,
    
    # length of the played interval (seconds)
    interval_length,
    # natural log of interval_length
    interval_length_log,
    
    # response (seconds) - duration for which participants held spacebar
    response,
    # natural log of response
    response_log,
    # ratio of response and interval_length
    response_ratio,
    
    # how tired were participants (0-4 points)
    fatigue,
    fatigue_score,
    
    music_train,
    music_sing,
    music_instr,
    music_train_score,
    music_sing_score,
    music_instr_score,
    # composite of 3 music scores above (0-6 points)
    music_experience_score
  )


# data checks
stopifnot(str_detect(data_full$participant, pattern = "P\\d+"))
stopifnot(data_full$gender %in% c("M", "F", "O"))
stopifnot(between(data_full$age, 18, 99))

stopifnot(data_full$groups %in% c("slow", "normal", "fast"))
stopifnot(between(data_full$beats_per_second, 0, 24))
stopifnot(between(data_full$interval_length, 0, 8))

stopifnot(between(data_full$response, 0, 30))

stopifnot(between(data_full$fatigue_score, 0, 4))

stopifnot(between(data_full$music_train_score, 0, 2))
stopifnot(between(data_full$music_sing_score, 0, 2))
stopifnot(between(data_full$music_instr_score, 0, 2))
stopifnot(between(data_full$music_experience_score, 0, 6))


data_analysis <- 
  data_full |> 
  select(
    participant, gender, gender_index, gender_contr, male, female, age, age_c,
    experiment, experiment_index, experiment_contr, groups, trial_index,
    beats_per_second, interval_length, interval_length_log, 
    response, response_log, response_ratio,
    fatigue_score, music_train_score, music_sing_score, music_instr_score,
    music_experience_score
  )


# save clean data to file
write_rds(data_analysis, file = here("data_cleaning/data_analysis.rds"))
write_rds(data_full, file = here("data_cleaning/data_full.rds"))
