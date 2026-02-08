library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(marginaleffects)



dt <- read_rds(here("data_cleaning/data_analysis.rds"))
dt <- dt |> filter(!is.na(gender_index))


# m00 ---------------------------------------------------------------------

m00 <- brm(
  formula = bf(
    response_log ~ 0 + k + t * interval_length_log,
    k ~ 1,
    t ~ 1,
    nl = TRUE
  ),
  data = dt,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.5)", nlpar = "k"),
    set_prior("normal(1, 0.2)", nlpar = "t"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m00.rds"),
  seed = 20260205,
  backend = "cmdstanr"
)


m00

pp_check(m00, ndraws = 30)
mcmc_plot(m00)

plot(conditional_effects(m00), points = TRUE)

m00_post_epred <- epred_draws(m00, dt |> distinct(interval_length_log))
m00_post_pred <- predicted_draws(m00, dt |> distinct(interval_length_log))


m00_post_epred |> 
  mutate(
    interval_length = exp(interval_length_log),
    .epred_exp = exp(.epred)
  ) |> 
  ggplot(aes(interval_length, .epred_exp)) +
  geom_point()


m00_post_pred |> 
  mutate(
    interval_length = exp(interval_length_log),
    .prediction_exp = exp(.prediction)
  ) |> 
  ggplot(aes(interval_length, .prediction_exp)) +
  geom_jitter(width = 0.1, height = 0) +
  geom_jitter(data = dt, aes(interval_length, response), color = "red", width = 0.1, height = 0)


m00_post_pred2 <-
  m00 |> 
  predicted_draws(
    newdata = tibble(interval_length_log = log(seq(1, 8, by = 0.1)))
  ) |> 
  mutate(
    interval_length = exp(interval_length_log),
    .prediction_exp = exp(.prediction)
  )

m00_post_pred2 |> 
  ggplot(aes(interval_length, .prediction_exp)) +
  stat_lineribbon() +
  geom_jitter(data = dt, aes(interval_length, response), color = "red", width = 0.1, height = 0, alpha = 0.1)



# m01 ---------------------------------------------------------------------

m01 <- brm(
  formula = bf(
    response_log ~ 0 + g + e + t * interval_length_log,
    g ~ 0 + gender_index,
    e ~ 0 + experiment_index,
    t ~ 1,
    nl = TRUE
  ),
  data = dt,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.2)", nlpar = "g"),
    set_prior("normal(0, 0.2)", nlpar = "e"),
    set_prior("normal(1, 0.2)", nlpar = "t"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m01.rds"),
  seed = 20260205,
  backend = "cmdstanr"
)


m01

pp_check(m01, ndraws = 30)
mcmc_plot(m01)


plot(conditional_effects(m01), points = TRUE)

m01_post_epred <- epred_draws(m01, dt |> distinct(interval_length_log))
m01_post_pred <- predicted_draws(m01, dt |> distinct(interval_length_log))


m01_post_pred2 <-
  m01 |> 
  predicted_draws(newdata = expand_grid(
    interval_length_log = unique(dt$interval_length_log),
    gender_index = c("1", "2"),
    experiment_index = c("1", "2")
  )) |> 
  mutate(
    interval_length = exp(interval_length_log),
    .prediction_exp = exp(.prediction)
  )

m01_post_pred2 |> 
  ggplot(aes(interval_length, .prediction_exp)) +
  stat_lineribbon() +
  geom_jitter(data = dt, aes(interval_length, response), color = "red", width = 0.1, height = 0, alpha = 0.1)


m01_post_pred2 |> 
  ggplot(aes(interval_length, .prediction_exp)) +
  stat_lineribbon() +
  geom_jitter(data = dt, aes(interval_length, response), color = "red", width = 0.1, height = 0, alpha = 0.1) +
  facet_wrap(vars(experiment), nrow = 2, scales = "free_x")



m01_post_pred2 |> 
  ggplot(aes(experiment_index, .prediction_exp)) +
  geom_boxplot(alpha = 0.1) +
  geom_boxplot(data = dt, aes(experiment_index, response), color = "red", alpha = 0.1)


m01_post_pred2 |> 
  ggplot(aes(gender_index, .prediction_exp)) +
  geom_boxplot(alpha = 0.1) +
  geom_boxplot(data = dt |> filter(!is.na(gender_index)), aes(gender_index, response), color = "red", alpha = 0.1)




loo_m00 <- loo(m00)
loo_m01 <- loo(m01)

loo_compare(loo_m00, loo_m01)



# m02 ---------------------------------------------------------------------

m02 <- brm(
  formula = bf(
    response_log ~ 0 + g + e + u +
      t * interval_length_log +
      a * age_c +
      b * beats_per_second +
      te * experiment_index * interval_length_log +
      eb * experiment_index * beats_per_second +
      tb * beats_per_second * interval_length_log,
    g ~ 0 + gender_index,
    e ~ 0 + experiment_index,
    u + t + b ~ 1 + (1 |p| participant),
    te + eb + tb ~ 1,
    a ~ 1,
    nl = TRUE
  ),
  data = dt,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.2)", nlpar = "g"),
    set_prior("normal(0, 0.2)", nlpar = "e"),
    set_prior("normal(0, 0.2)", nlpar = "a"),
    set_prior("normal(0, 0.2)", nlpar = "b"),
    set_prior("normal(1, 0.2)", nlpar = "t"),
    set_prior("normal(0, 0.1)", nlpar = "te"),
    set_prior("normal(0, 0.1)", nlpar = "eb"),
    set_prior("normal(0, 0.1)", nlpar = "tb"),
    set_prior("student_t(3, 0, 0.2)", class = "sd", group = "participant", nlpar = "u"),
    set_prior("student_t(3, 0, 0.2)", class = "sd", group = "participant", nlpar = "t"),
    set_prior("student_t(3, 0, 0.2)", class = "sd", group = "participant", nlpar = "b"),
    set_prior("lkj(4)", class = "cor", group = "participant"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m02.rds"),
  seed = 20260205,
  backend = "cmdstanr",
  sample_prior = "yes"
)

m02

loo_m02 <- loo(m02)
loo_compare(loo_m00, loo_m01, loo_m02)

conditional_effects_m02 <- conditional_effects(m02)
conditional_effects_plots_m02 <- plot(conditional_effects(m02), points = TRUE)
conditional_effects_plots_m02b <- plot(conditional_effects(m02, method = "posterior_predict"), points = TRUE)

m02 |> 
  


conditional_effects_m02 |> 
  pluck(1) |> 
  as_tibble() |> 
  mutate(
    interval_length = exp(interval_length_log),
    estimate_exp__ = exp(estimate__),
    lower_exp__ = exp(lower__),
    upper_exp__ = exp(upper__)
  ) |> 
  ggplot(aes(interval_length, estimate_exp__)) +
  geom_smooth()

conditional_effects(
  m02,
  effects = "interval_length_log",
  re_formula = NULL,
  method = "posterior_predict",
  transform = exp
)




# m03 ---------------------------------------------------------------------

m03 <- brm(
  formula = bf(
    response_log ~ 
      0 + Intercept +
      beats_per_second + 
      interval_length_log + 
      experiment_contr +
      gender_contr +
      age_c +
      interval_length_log:experiment_contr +
      beats_per_second:experiment_contr +
      beats_per_second:interval_length_log +
      (beats_per_second + interval_length_log | participant)
  ),
  dt,
  family = gaussian(),
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
    set_prior("normal(0, 0.15)", class = "sd"),
    set_prior("lkj(4)", class = "cor"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m03.rds"),
  seed = 20260205,
  backend = "cmdstanr"
)


m03

loo_m03 <- loo(m03)
loo_compare(loo_m00, loo_m01, loo_m02, loo_m03)



pp_check(m03, ndraws = 20)
pp_check(m03, type = "scatter_avg", x = "interval_length")
pp_check(m03, type = "stat", stat = "sd")
mcmc_plot(m03)


conditional_effects_plots_m03 <- plot(conditional_effects(m03), points = TRUE)
conditional_effects_plots_m03b <- plot(conditional_effects(m03, method = "posterior_predict"), points = TRUE)

newdata_m03 <- expand_grid(
  beats_per_second = seq(min(dt$beats_per_second), max(dt$beats_per_second), by = 1),
  interval_length_log = quantile(dt$interval_length_log, c(0.25, 0.5, 0.75)),
  age_c = 0,
  gender_contr = c(-0.5, 0.5),
  experiment_contr = c(-0.5, 0.5)
)

posterior_epred_m03 <- 
  m03 |> 
  epred_draws(
    newdata = newdata_m03,
    re_formula = NA
  ) |> 
  ungroup()



posterior_epred_m03 |> 
  slice_sample(n = 1e4) |> 
  ggplot(aes(beats_per_second, exp(.epred), color = factor(interval_length_log))) +
  geom_point() +
  geom_smooth(method = "lm")


plot_predictions(m03, newdata = newdata_m03, condition = "beats_per_second", transform = "exp", type = "response") +
  scale_y_continuous(limits = c(2.5, 5.5))

plot_predictions(m03, newdata = newdata_m03, condition = "beats_per_second", type = "response")

plot_slopes(m03, variables = "beats_per_second", condition = "beats_per_second")


slopes(m03, variables = "beats_per_second", newdata = newdata_m03, re_formula = NA, transform = "exp")
avg_slopes(m03, variables = "beats_per_second", newdata = newdata_m03, re_formula = NA)



posterior_epred_m03 |> 
  group_by(beats_per_second) |> 
  summarise(response_epred = mean(exp(.epred)), .groups = "drop") |> 
  ggplot(aes(beats_per_second, response_epred)) +
  geom_point() +
  scale_y_continuous(limits = c(2.5, 5.5))


posterior_epred_m03  |>
  group_by(.draw, interval_length_log, age_c, gender_contr, experiment_contr) |>
  arrange(beats_per_second) |>
  mutate(delta_y = exp(.epred) - lag(exp(.epred)),
         delta_x = beats_per_second - lag(beats_per_second),
         slope = delta_y / delta_x) |>
  filter(!is.na(slope)) |>
  summarise(avg_slope = mean(slope), .groups = "drop")  |>
  summarise(
    mean = mean(avg_slope),
    median = median(avg_slope),
    q2.5 = quantile(avg_slope, 0.025),
    q97.5 = quantile(avg_slope, 0.975)
  )


# m04 ---------------------------------------------------------------------

m04 <- brm(
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
      (1 + beats_per_second + interval_length_log | participant)
  ),
  dt,
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
    set_prior("normal(0, 0.15)", class = "sd"),
    set_prior("lkj(4)", class = "cor"),
    set_prior("exponential(1)", class = "sigma")
  ),
  chains = 4,
  cores = 4,
  file = here("analysis/m04.rds"),
  seed = 20260205,
  backend = "cmdstanr"
)


m04

loo_m04 <- loo(m04)
loo_compare(loo_m00, loo_m01, loo_m02, loo_m04)



pp_check(m04, ndraws = 20)
pp_check(m04, type = "scatter_avg", x = "interval_length")

mcmc_plot(m04)


conditional_effects_plots_m04 <- plot(conditional_effects(m04), points = TRUE)
conditional_effects_plots_m04b <- plot(conditional_effects(m04, method = "posterior_predict"), points = TRUE)

newdata_m04 <- expand_grid(
  beats_per_second = seq(min(dt$beats_per_second), max(dt$beats_per_second), by = 1),
  interval_length_log = dt$interval_length_log |> unique(),
  age_c = 0,
  gender_contr = c(-0.5, 0.5),
  experiment_contr = c(-0.5, 0.5)
)

newdata_m04b <- 
  dt |> 
  distinct(beats_per_second, interval_length_log, experiment_contr, gender_contr) |> 
  mutate(age_c = 0)


newdata_m04c <- 
  bind_rows(
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

posterior_epred_m04 <- 
  m04 |> 
  epred_draws(
    newdata = newdata_m04c,
    re_formula = NA
  ) |> 
  ungroup()



avg_slopes_m04 <- avg_slopes(
  m04,
  variables = "beats_per_second",
  by = c("interval_length_log", "experiment_contr"),
  re_formula = NA,
  newdata = newdata_m04c
) |>
  as_tibble() |> 
  mutate(interval_length = exp(interval_length_log))


avg_slopes_m04b <- avg_slopes(
  m04,
  variables = "beats_per_second",
  by = c("interval_length_log", "experiment_contr"),
  re_formula = NA,
  newdata = newdata_m04b
) |>
  as_tibble() |> 
  mutate(interval_length = exp(interval_length_log))


dt2 <- posterior_epred_m04 |> 
  summarize(mean = mean(.epred), .by = c(beats_per_second, interval_length_log, experiment_contr))

dt2b <- dt2 |> 
  filter(beats_per_second == 1) |> 
  left_join(avg_slopes_m04, by = join_by(interval_length_log, experiment_contr))

dt2c <- posterior_epred_m04 |> 
  group_by(beats_per_second, interval_length_log, experiment_contr) |> 
  mean_hdi() |> 
  select(-c(starts_with("age_c"), starts_with("gender_contr")))


posterior_epred_m04 |> 
  slice_sample(n = 1e5) |> 
  ggplot(aes(beats_per_second, .epred, color = factor(round(exp(interval_length_log), 2)))) +
  # geom_jitter(width = 0, height = 0) +
  stat_lineribbon(alpha = 0.8) +
  facet_wrap(vars(experiment_contr), nrow = 2, labeller = as_labeller(c("-0.5" = "linear", "0.5" = "geometric"))) +
  labs(color = "interval length", y = "response") +
  geom_point(data = dt2, aes(beats_per_second, mean)) +
  geom_abline(
    data = dt2b,
    aes(intercept = mean, slope = estimate)
  ) +
  geom_point(
    data = dt2c,
    color = "red", alpha = 0.5,
    aes(beats_per_second, .epred)
  ) +
  geom_errorbar(
    data = dt2c,
    color = "red", alpha = 0.5,
    aes(x = beats_per_second, y = .epred, ymin = .epred.lower, ymax = .epred.upper)
  )



dt2 |> 
  group_by(interval_length_log, experiment_contr) |> 
  mutate(dydx = (mean - lag(mean)) / (beats_per_second - lag(beats_per_second))) |> 
  filter(beats_per_second == max(beats_per_second))
  ggplot() +
  geom_point(aes(beats_per_second, dydx, color = interval_length_log)) +
  facet_wrap(vars(experiment_contr), nrow = 2)



plot_predictions(m04, newdata = newdata_m04c, condition = "beats_per_second") +
  scale_y_continuous(limits = c(2.5, 5.5))

plot_predictions(m04, 
                 newdata = newdata_m04c, 
                 condition = c("beats_per_second", "interval_length_log", "experiment_contr"))

plot_predictions(m04, 
                 by = c("beats_per_second", "interval_length_log", "experiment_contr"))


plot_slopes(m04, variables = "beats_per_second", by = "interval_length_log", re_formula = NA)


slopes(m04, variables = "beats_per_second", newdata = newdata_m04b, re_formula = NA)



avg_slopes_m04 |>
  ggplot(aes(exp(interval_length_log), estimate)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(experiment_contr), nrow = 1)

avg_slopes_m04b |>
  ggplot(aes(exp(interval_length_log), estimate)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(experiment_contr), nrow = 1)

posterior_epred_m04 |> 
  group_by(beats_per_second) |> 
  summarise(response_epred = mean(.epred), .groups = "drop") |> 
  ggplot(aes(beats_per_second, response_epred)) +
  geom_point() +
  scale_y_continuous(limits = c(2.5, 5.5))


posterior_epred_m04  |>
  group_by(.draw, interval_length_log, age_c, gender_contr, experiment_contr) |>
  arrange(beats_per_second) |>
  mutate(delta_y = exp(.epred) - lag(exp(.epred)),
         delta_x = beats_per_second - lag(beats_per_second),
         slope = delta_y / delta_x) |>
  filter(!is.na(slope)) |>
  summarise(avg_slope = mean(slope), .groups = "drop")  |>
  summarise(
    mean = mean(avg_slope),
    median = median(avg_slope),
    q2.5 = quantile(avg_slope, 0.025),
    q97.5 = quantile(avg_slope, 0.975)
  )



# m05 ---------------------------------------------------------------------

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
  dt,
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


loo_m04 <- loo(m04)
loo_m05 <- loo(m05)

loo_compare(loo_m05, loo_m04)

pp_check(m05, ndraws = 30)

# fit_ratio <- brm(
#   formula = bf(
#     response / interval_length ~ 
#       beats_per_second + 
#       (beats_per_second || participant)
#   ),
#   dt,
#   family = gaussian(),
#   prior = c(
#     set_prior("normal(0, 0.1)", class = "Intercept"),
#     set_prior("normal(0, 0.2)", class = "b", coef = "beats_per_second"),
#     set_prior("student_t(3, 0, 0.15)", class = "sd")
#   )
# )



