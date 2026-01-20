
# setup -------------------------------------------------------------------


library(tidyverse)
library(here)
library(brms)


theme_set(theme_bw())

data_test <- read_rds(here("data_cleaning_test/data_clean_test.rds"))


n_participants_per_group <- 20
n_repeats <- 3


tempo_levels_slow <- c(1.7, 3.4, 5.1, 6.8)
tempo_levels_normal <- c(6.8, 8.5, 10.2, 11.9)
tempo_levels_fast <- c(11.9, 13.6, 15.3, 17.0)

tempo_groups <- list(
  slow = tempo_levels_slow,
  normal = tempo_levels_normal,
  fast = tempo_levels_fast
)


interval_levels <- c(1.177, 1.765, 2.353, 2.942, 3.530, 4.706)


# function to create all trial combinations (repeated) for all participants in one group
create_trials_group <- function(group_name, tempo_levels) {
  expand_grid(
    participant_within_group = 1:n_participants_per_group,
    tempo = tempo_levels,
    interval_length = interval_levels,
    repeat_trial = 1:n_repeats
  ) |> 
    mutate(
      group = group_name,
      # makes id group-flexible
      participant = (match(group_name, names(tempo_groups)) - 1) * n_participants_per_group + participant_within_group
    ) |> 
    # removes temporary columns
    select(-participant_within_group, -repeat_trial)
}


# creates all trials for all 3 groups
trials <- map2(names(tempo_groups), tempo_groups, .f = create_trials_group) |> 
  list_rbind() |> 
  relocate(participant, group)

n_participants <- length(unique(trials$participant))



# data simulation ---------------------------------------------------------


# population-level (fixed) effects
b_Intercept <- 0
b_tempo <- 0.01
b_interval <- 1
b_interaction <- -0.008

# sds for group-level (random) effects
sd_intercept <- 0.01
sd_tempo <- 0.01
sd_interval <- 0.01

# random effects
re <- tibble(
  participant = unique(trials$participant),
  re_intercept = rnorm(n_participants, 0, sd_intercept),
  re_tempo = rnorm(n_participants, 0, sd_tempo),
  re_interval = rnorm(n_participants, 0, sd_interval)
)

# sigma
sigma_Intercept <- -2


# simulates response for values above based on the model formula
data_sim <- 
  trials |> 
  left_join(re, join_by(participant)) |> 
  mutate(
    mu = b_Intercept + re_intercept +
      (b_interval + re_interval) * log(interval_length) +
      (b_tempo + re_tempo) * tempo +
      b_interaction * tempo * log(interval_length),
    sigma = exp(sigma_Intercept),
    response = rlnorm(n(), mean = mu, sd = sigma)
  )


data_sim


data_sim |> 
  ggplot(aes(interval_length, response)) +
  geom_jitter(aes(color = tempo), width = 0.1, height = 0, alpha = 0.3) +
  geom_smooth(aes(group = group), method = "lm") +
  geom_hline(yintercept = interval_levels, linetype = 2, color = "grey")


# model fit on simulated data
fit1_log_sim <-
  brm(
    formula = bf(
      response ~
        0 + Intercept +
        tempo +
        log(interval_length) +
        tempo:log(interval_length) +
        (tempo + log(interval_length) || participant),
      sigma ~ 1
    ),
    data_sim,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 0.05)", class = "b", coef = "Intercept"),
      set_prior("normal(1, 0.01)", class = "b", coef = "loginterval_length"),
      set_prior("normal(0, 0.01)", class = "b", coef = "tempo"),
      set_prior("normal(0, 0.005)", class = "b", coef = "tempo:loginterval_length"),
      set_prior("normal(0, 0.01)", class = "sd"),
      set_prior("normal(-2, 0.1)", class = "Intercept", dpar = "sigma")
    ),
    cores = 4,
    chains = 4,
    file = here("data_simulation/fit1_log_sim.rds")
  )


summary(fit1_log_sim)
pp_check(fit1_log_sim, ndraws = 50)


# prior predictive simulations --------------------------------------------


simulate_prior_predictive <- function(trials) {
  
  # sample priors for fixed effects
  fe <- tibble(
    b_Intercept = rnorm(1, 0, 0.05),
    b_interval = rnorm(1, 1, 0.01),
    b_tempo = rnorm(1, 0, 0.01),
    b_interaction = rnorm(1, 0, 0.005)
  )
  
  re <- tibble(
    # sample priors for random effect sds
    sd_intercept = abs(rnorm(1, 0, 0.01)),
    sd_interval = abs(rnorm(1, 0, 0.01)),
    sd_tempo = abs(rnorm(1, 0, 0.01)),
    
    participant = unique(trials$participant),
    
    # sample random effects
    re_intercept = rnorm(n_participants, 0, sd_intercept),
    re_interval = rnorm(n_participants, 0, sd_interval),
    re_tempo = rnorm(n_participants, 0, sd_tempo)
  )
  
  # sample sigma priors
  sd <- tibble(
    sigma_Intercept = rnorm(1, -2, 0.1)
  )
  
  # join random effects and compute mu & sigma
  trials |> 
    bind_cols(fe, sd) |> 
    left_join(re, join_by(participant)) |> 
    mutate(
      mu = b_Intercept + re_intercept +
        (b_interval + re_interval) * log(interval_length) +
        (b_tempo + re_tempo) * tempo +
        b_interaction * tempo * log(interval_length),
      sigma = exp(sigma_Intercept),
      response = rlnorm(n(), mean = mu, sd = sigma)
    )
}


# simulate samples
n_sim <- 500
prior_predictive_samples <- map(1:n_sim, ~simulate_prior_predictive(trials)) |> 
  set_names(1:n_sim) |> 
  list_rbind(names_to = "sample")


prior_predictive_samples |> 
  filter(sample %in% sample(1:n_sim, 20)) |> 
  ggplot() +
  geom_density(aes(response, group = sample), color = "lightblue") +
  geom_vline(aes(xintercept = interval_length)) +
  facet_wrap(vars(interval_length), axes = "all_x")


prior_predictive_samples |> 
  slice_sample(n = 5000) |>  
  ggplot() +
  geom_point(aes(interval_length, response, group = sample), color = "lightblue") +
  geom_hline(aes(yintercept = interval_length), linetype = 2, color = "grey")
