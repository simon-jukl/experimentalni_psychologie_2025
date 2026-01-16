library(tidyverse)
library(here)


# change for real data directory when ready
dt <- read_rds(here("data_cleaning_test/data_clean_test.rds"))


dt |> 
  ggplot(aes(x = interval_length, y = response, color = tempo, shape = group)) +
  scale_color_continuous(palette = c("blue", "red")) +
  geom_smooth(aes(group = group), method = "lm") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6)
