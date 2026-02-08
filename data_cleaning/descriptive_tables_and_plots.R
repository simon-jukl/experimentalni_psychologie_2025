library(tidyverse)
library(here)


dt <- read_rds(here("data_cleaning/data_full.rds"))

dt


theme_set(theme_minimal())


# n_trials per participant, gender colored
ggplot(dt, aes(participant, fill = gender)) +
  geom_bar() +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(guide = guide_axis(angle = 90))


# age_trials
dt |> count(gender)

ggplot(dt |> filter(!is.na(male)), aes(age, fill = male)) +
  geom_histogram(na.rm = T) +
  facet_wrap(facets = vars(male), nrow = 2)

# age_participants
dt |> distinct(participant, .keep_all = T) |> count(gender)

ggplot(dt |> distinct(participant, .keep_all = T) |> filter(!is.na(male)), aes(age, fill = male)) +
  geom_histogram(na.rm = T) +
  facet_wrap(facets = vars(male), nrow = 2)



# participant experiment start
ggplot(dt, aes(datetime, participant, colour = gender, size = age)) +
  geom_point()
  

# n_trials per group per experiment
dt |> count(experiment, group)
# n_participants per group per experiment
dt |> distinct(participant, experiment, .keep_all = T) |> count(experiment, group)

# check combination frequency
dt |> count(experiment, beats_per_second, interval_length)
dt |> filter(experiment == "linear") |> janitor::tabyl(beats_per_second, interval_length)
dt |> filter(experiment == "geometric") |> janitor::tabyl(beats_per_second, interval_length)

dt |> count(experiment, beats_per_second)
dt |> count(experiment, interval_length)


ggplot(dt, aes(interval_length, fill = experiment)) +
  geom_density() +
  facet_wrap(vars(interval_length))


dt |> distinct(participant, .keep_all = T) |> count(fatigue_score)

dt |> distinct(participant, .keep_all = T) |> count(music_train_score)
dt |> distinct(participant, .keep_all = T) |> count(music_sing_score)
dt |> distinct(participant, .keep_all = T) |> count(music_instr_score)
dt |> distinct(participant, .keep_all = T) |> count(music_experience_score)


