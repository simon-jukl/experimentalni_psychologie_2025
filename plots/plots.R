library(tidyverse)
library(here)
library(patchwork)
library(viridisLite)


dt <- read_rds(here("data_cleaning/data_full.rds"))

dt


theme_set(theme_minimal())


interval_lengths <- 
  dt |> 
  group_by(experiment) |> 
  distinct(interval_length) |> 
  ungroup() |> 
  mutate(
    x = interval_length,
    width = 0.12,
    xmin = x - width,
    xmax = x + width
  ) |> 
  arrange(x)

interval_lengths_linear <- subset(interval_lengths, experiment == "linear", x, drop = T)
interval_lengths_geometric <- subset(interval_lengths, experiment == "geometric", x, drop = T)


tempos <- 
  dt |> 
  group_by(experiment) |> 
  distinct(beats_per_second) |> 
  ungroup() |> 
  arrange(beats_per_second) |> 
  mutate(colour = viridis(n = n(), direction = -1))

tempo_colours_linear <- subset(tempos, experiment == "linear", colour, drop = T)
tempo_colours_geometric <- subset(tempos, experiment == "geometric", colour, drop = T)



ggplot(dt, aes(interval_length, response)) +
  geom_point()


dt |> 
  # filter(response < 20) |> 
  # slice_sample(n = 20, by = c(beats_per_second, interval_length), replace = F) |> 
  ggplot(aes(
    x = interval_length, 
    y = response,
    colour = factor(beats_per_second)
  )) +
  # geom_smooth(se = F, method = "lm") +
  geom_jitter(
    width = 0.1, 
    height = 0, 
    # alpha = 0.3, 
    shape = 1
  ) +
  geom_segment(data = interval_lengths, aes(
    x = xmin,
    xend = xmax,
    y = x
  ),
  colour = "red4", linewidth = 1) +
  scale_x_continuous(breaks = interval_lengths$x) +
  scale_colour_viridis_d(direction = -1)


dt |> 
  # filter(response < 20) |> 
  # slice_sample(n = 20, by = c(beats_per_second, interval_length), replace = F) |> 
  ggplot(aes(
    x = interval_length, 
    y = response,
    colour = factor(group)
  )) +
  geom_smooth(se = F, method = "lm") +
  scale_x_continuous(breaks = interval_lengths$x) +
  scale_colour_viridis_d(direction = -1) +
  facet_wrap(vars(experiment), nrow = 2)



p_lin1 <- dt |>
  filter(experiment == "linear") |>
  ggplot(aes(
    x = interval_length, 
    y = response,
    colour = factor(beats_per_second)
  )) +
  # geom_smooth(se = F, method = "lm") +
  geom_jitter(
    width = 0.1, 
    height = 0, 
    # alpha = 0.3, 
    shape = 1
  ) +
  geom_segment(data = interval_lengths |> filter(experiment == "linear"), aes(
    x = xmin,
    xend = xmax,
    y = x
  ),
  colour = "red4", linewidth = 1) +
  scale_x_continuous(breaks = interval_lengths_linear) +
  scale_colour_discrete(palette = tempo_colours_linear)


p_geo1 <- dt |> 
  filter(experiment == "geometric") |>
  ggplot(aes(
    x = interval_length, 
    y = response,
    colour = factor(beats_per_second)
  )) +
  # geom_smooth(se = F, method = "lm") +
  geom_jitter(
    width = 0.1, 
    height = 0, 
    # alpha = 0.3, 
    shape = 1
  ) +
  geom_segment(data = interval_lengths |> filter(experiment == "geometric"), aes(
    x = xmin,
    xend = xmax,
    y = x
  ),
  colour = "red4", linewidth = 1) +
  scale_x_continuous(breaks = interval_lengths_geometric) +
  scale_colour_discrete(palette = tempo_colours_geometric)

p_lin1 / p_geo1 + patchwork::plot_layout(axes = "collect")


