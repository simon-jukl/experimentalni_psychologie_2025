library(tidyverse)
library(here)
library(brms)


data_test <- read_rds(here("DATA"))

# priors are still not set properly in all 3 models

fit_raw <- brm(
  formula = formula = bf(
    response ~ 
      tempo + 
      interval_length + 
      tempo:interval_length +
      (tempo + interval_length || participant),
    sigma ~ interval_length
  ),
  data,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.1)", class = "Intercept"),
    set_prior("normal(0, 0.2)", class = "b", coef = "tempo"),
    set_prior("normal(1, 0.1)", class = "b", coef = "interval_length"),
    set_prior("normal(0, 0.1)", class = "b", coef = "tempo:interval_length"),
    set_prior("student_t(3, 0, 0.15)", class = "sd"),
    set_prior("exponential(1)", class = "sigma")
  )
)
  

fit_ratio <- brm(
  formula = bf(
    response / interval_length ~ 
      tempo + 
      (tempo || participant)
  ),
  data,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.1)", class = "Intercept"),
    set_prior("normal(0, 0.2)", class = "b", coef = "tempo"),
    set_prior("student_t(3, 0, 0.15)", class = "sd")
  )
)


fit_log <- brm(
  formula = bf(
    response_log ~ 
      tempo + 
      interval_length + 
      tempo:interval_length +
      (tempo + interval_length || participant),
    sigma ~ interval_length
  ),
  data,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 0.1)", class = "Intercept"),
    set_prior("normal(0, 0.2)", class = "b", coef = "tempo"),
    set_prior("normal(1, 0.1)", class = "b", coef = "interval_length"),
    set_prior("normal(0, 0.1)", class = "b", coef = "tempo:interval_length"),
    set_prior("student_t(3, 0, 0.15)", class = "sd"),
    set_prior("exponential(1)", class = "sigma")
  )
)
