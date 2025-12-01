library(tidyverse)
library(lme4)


theme_set(theme_bw())
options(show.signif.stars = FALSE)

set.seed(1981758518)


# level 1
# 
# response ~ tempo
# effect same across subjects



tempo <- c(1.5, 2, 3, 4, 6, 8, 12, 16, 24, 32)
n_subj <- 1000
n_trials <- 100


# every Hz (bps) adds 0.05 s to response
tempo_effect <- 0.03
# noise (in seconds) around the mean effect
noise <- 0.1

# 3 seconds (= intercept)
interval <- 3


dt1 <- 
  map(1:n_subj, \(subj) {
    
    tibble(
      # same but randomly ordered trials for every subject
      tempo = rep_len(tempo, n_trials) |> sample(),
      # centering - 1 Hz baseline
      tempo_c = tempo - 1
    ) |> 
      mutate(
        id = subj,
        trial = row_number(),
        # interval as intercept (3 s)
        response = interval + tempo_effect * tempo_c + rnorm(n(), 0, noise)
      )
    
  }
  ) |> 
  list_rbind() |> 
  mutate(
    response_diff = response - interval
  ) |> 
  relocate(id, trial, tempo, tempo_c, response, response_diff)


dt1


dt1 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response)) +
  geom_point()

dt1 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response_diff)) +
  geom_point()


dt1 |> 
  # slice_sample(n = 5000) |> 
  ggplot(aes(response, fill = factor(tempo))) +
  geom_density()


fit1 <- lm(response ~ tempo_c, dt1)

summary(fit1)
# intercept - predicted response for trials with tempo 1 Hz
# tempo_c - difference in response between trials with 1 Hz difference


# level 2
# 
# response ~ tempo + interval
# effect same across subjects



tempo <- c(1.5, 2, 3, 4, 6, 8, 12, 16, 24, 32)
interval <- c(1.6, 2.2, 2.6, 3.2, 4.1, 5)
n_subj <- 1000

# all combinations of tempo and interval
trials <- bind_rows(expand_grid(tempo, interval), expand_grid(tempo, interval))

# every Hz (bps) adds 0.05 s to response
tempo_effect <- 0.05
# noise (in seconds) around the mean effect
noise <- 0.1



dt2 <- 
  map(1:n_subj, \(subj) {
    
    trials |> 
      # same but randomly ordered trials for every subject
      slice_sample(n = nrow(trials)) |> 
      mutate(
        id = subj,
        trial = row_number(),
        # centering - 1 Hz baseline
        tempo_c = tempo - 1,
        response = interval + tempo_effect * tempo_c + rnorm(n(), 0, noise)
      )
    
  }
  ) |> 
  list_rbind() |> 
  mutate(
    response_diff = response - interval
  ) |> 
  relocate(id, trial, interval, tempo, tempo_c, response, response_diff)


dt2


dt2 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response, color = factor(interval))) +
  geom_point() +
  geom_smooth(method = "lm")

dt2 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response, color = factor(tempo))) +
  geom_point()


dt2 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response_diff, color = factor(interval))) +
  geom_point()

dt2 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response_diff, color = factor(tempo))) +
  geom_point()


dt2 |> 
  # slice_sample(n = 5000) |> 
  ggplot(aes(response, fill = factor(tempo))) +
  geom_density(alpha = 0.5)


fit2 <- lm(response ~ tempo_c + interval, dt2)

summary(fit2)




# level 3
# 
# response ~ tempo + interval + tempo:interval
# effect same across subjects



tempo <- c(1.5, 2, 3, 4, 6, 8, 12, 16, 24, 32)
interval <- c(1.6, 2.2, 2.6, 3.2, 4.1, 5)
n_subj <- 1000

# all combinations of tempo and interval
trials <- bind_rows(expand_grid(tempo, interval), expand_grid(tempo, interval))

# every Hz (bps) adds 0.05 s to response
tempo_effect <- 0.04
# noise (in seconds) around the mean effect
noise <- 0.1
# interaction
# -0.02 makes it too strong
interaction <- -0.012



dt3 <- 
  map(1:n_subj, \(subj) {
    
    trials |> 
      # same but randomly ordered trials for every subject
      slice_sample(n = nrow(trials)) |> 
      mutate(
        id = subj,
        trial = row_number(),
        # centering - 1 Hz baseline
        tempo_c = tempo - 1,
        response = interval + tempo_effect * tempo_c + interaction * tempo_c * interval + rnorm(n(), 0, noise)
      )
    
  }
  ) |> 
  list_rbind() |> 
  mutate(
    response_diff = response - interval
  ) |> 
  relocate(id, trial, interval, tempo, tempo_c, response, response_diff)


dt3


dt3 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response, color = factor(interval))) +
  geom_point() +
  geom_smooth(method = "lm")

dt3 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response, color = factor(tempo))) +
  geom_point() +
  geom_smooth(method = "lm")


dt3 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response_diff, color = factor(interval))) +
  geom_point() +
  geom_smooth(method = "lm")

dt3 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response_diff, color = factor(tempo))) +
  geom_point() +
  geom_smooth(method = "lm")


dt3 |> 
  # slice_sample(n = 5000) |> 
  ggplot(aes(response, fill = factor(tempo))) +
  geom_density(alpha = 0.5)


fit3 <- lm(response ~ tempo_c + interval + tempo_c:interval, dt3)

summary(fit3)



# level 4
# 
# response ~ tempo + interval + time:interval + (1 + tempo + interval | id)
# multilevel model with effect of tempo and interval across subjects



tempo <- c(1.5, 2, 3, 4, 6, 8, 12, 16, 24, 32)
interval <- c(1.6, 2.2, 2.6, 3.2, 4.1, 5)
n_subj <- 1000

# all combinations of tempo and interval
trials <- bind_rows(expand_grid(tempo, interval), expand_grid(tempo, interval))

# every Hz (bps) adds 0.05 s to response
# 
mean_tempo_effect <- 0.05
# noise (in seconds) around the mean effect
noise <- 0.1
# interaction
# -0.02 makes it too strong
interaction <- -0.013



dt4 <- 
  map(1:n_subj, \(subj) {
    
    trials |> 
      # same but randomly ordered trials for every subject
      slice_sample(n = nrow(trials)) |> 
      mutate(
        id = subj,
        # random subject effect (diff from mean effect) 
        subj_tempo_effect = rnorm(1, 0, 0.02),
        subj_interval_effect = rnorm(1, 0, 0.01),
        trial = row_number(),
        # centering - 1 Hz baseline
        tempo_c = tempo - 1,
        response = (interval + subj_interval_effect) + (mean_tempo_effect + subj_tempo_effect) * tempo_c + interaction * tempo_c * interval + rnorm(n(), 0, noise)
      )
    
  }
  ) |> 
  list_rbind() |> 
  mutate(
    response_diff = response - interval
  ) |> 
  relocate(id, trial, interval, tempo, tempo_c, response, response_diff)


dt4


dt4 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response, color = factor(interval))) +
  geom_point() +
  geom_smooth(method = "lm")

dt4 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response, color = factor(tempo))) +
  geom_point() +
  geom_smooth(method = "lm")


dt4 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = tempo_c, y = response_diff, color = factor(interval))) +
  geom_point() +
  geom_smooth(method = "lm")

dt4 |> 
  slice_sample(n = 5000) |> 
  ggplot(aes(x = interval, y = response_diff, color = factor(tempo))) +
  geom_point() +
  geom_smooth(method = "lm")


dt4 |> 
  # slice_sample(n = 5000) |> 
  ggplot(aes(response, fill = factor(tempo))) +
  geom_density(alpha = 0.5)


fit4 <- lmer(response ~ tempo_c + interval + tempo_c:interval + (1 + tempo + interval | id), dt4)

summary(fit4)
