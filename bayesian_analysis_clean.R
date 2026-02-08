library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(marginaleffects)


dt <- read_rds(here("data_cleaning/data_analysis.rds"))
dt <- dt |> filter(!is.na(gender_index))

# full model
m05 <- brm(
  formula = bf(
    response ~
      0 + Intercept +
      beats_per_second +
      interval_length_log +
      experiment_contr +
      gender_contr +
      age_c +
      interval_length_log:experiment_contr +
      beats_per_second:experiment_contr +
      beats_per_second:interval_length_log +
      beats_per_second:interval_length_log:experiment_contr +
      (1 + beats_per_second + interval_length_log | participant)
  ),
  data = dt,
  family = lognormal(),
  prior = c(
    set_prior("normal(0, 0.1)", class = "b", coef = "Intercept"),
    set_prior("normal(0, 0.2)", class = "b", coef = "beats_per_second"),
    set_prior("normal(0, 0.2)", class = "b", coef = "experiment_contr"),
    set_prior("normal(0, 0.2)", class = "b", coef = "gender_contr"),
    set_prior("normal(0, 0.2)", class = "b", coef = "age_c"),
    set_prior("normal(1, 0.1)", class = "b", coef = "interval_length_log"),
    set_prior("normal(0, 0.1)", class = "b", coef = "interval_length_log:experiment_contr"),
    set_prior("normal(0, 0.1)", class = "b", coef = "beats_per_second:experiment_contr"),
    set_prior("normal(0, 0.1)", class = "b", coef = "beats_per_second:interval_length_log"),
    set_prior("normal(0, 0.1)", class = "b", coef = "beats_per_second:interval_length_log:experiment_contr"),
    set_prior("normal(0, 0.15)", class = "sd"),
    set_prior("lkj(4)", class = "cor"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m05.rds"),
  seed = 20260207,
  backend = "cmdstanr"
)

m05
# posterior predictive check (model reproduces real data quite nicely)
pp_check(m05, ndraws = 20)


# dataset for predictions based on real data
olddata_m05 <- dt |>
  distinct(beats_per_second,
           interval_length_log,
           experiment_contr,
           gender_contr) |>
  mutate(age_c = 0)


# counterfactual dataset for predictions
newdata_m05 <- bind_rows(
  expand_grid(
    beats_per_second = seq(min(dt$beats_per_second), max(dt$beats_per_second), by = 1),
    interval_length_log = seq(log(1), log(8), by = 0.25),
    age_c = 0,
    gender_contr = c(-0.5, 0.5),
    experiment_contr = -0.5
  ),
  expand_grid(
    beats_per_second = seq(min(dt$beats_per_second), max(dt$beats_per_second), by = 1),
    interval_length_log = seq(log(1), log(8), by = 0.25),
    age_c = 0,
    gender_contr = c(-0.5, 0.5),
    experiment_contr = 0.5
  )
)


# posterior mean (= epred) draws from olddata
posterior_epred_old_m05 <-
  m05 |>
  epred_draws(newdata = olddata_m05, re_formula = NA) |>
  ungroup()

# posterior mean (= epred) draws from newdata
posterior_epred_new_m05 <-
  m05 |>
  epred_draws(newdata = newdata_m05, re_formula = NA) |>
  ungroup()

# model predictions for old data
posterior_epred_old_m05 |>
  slice_sample(n = 1e5) |>
  ggplot(aes(beats_per_second, .epred, color = factor(round(
    exp(interval_length_log), 2
  )))) +
  # geom_jitter(width = 0, height = 0) +
  stat_lineribbon(alpha = 0.8) +
  facet_wrap(vars(experiment_contr),
             nrow = 2,
             labeller = as_labeller(c("-0.5" = "linear", "0.5" = "geometric"))) +
  labs(color = "interval length", y = "response")

# model predictions for counterfactual new data
posterior_epred_new_m05 |>
  slice_sample(n = 1e5) |>
  ggplot(aes(beats_per_second, .epred, color = factor(round(
    exp(interval_length_log), 2
  )))) +
  # geom_jitter(width = 0, height = 0) +
  stat_lineribbon(alpha = 0.8) +
  facet_wrap(vars(experiment_contr),
             nrow = 2,
             labeller = as_labeller(c("-0.5" = "linear", "0.5" = "geometric"))) +
  labs(color = "interval length", y = "response")


# slope (= first derivative of response) 
# conditional on interval_length_log and experiment_contr
p_slopes <- plot_slopes(
  m05,
  variables = "beats_per_second",
  by = c("interval_length_log", "experiment_contr")
)


p_slopes + 
  labs(x = "interval_length_log", y = "effect of beats per second on estimate (slope)")

