library(tidyverse)

n <- 1e5

tb <- bind_rows(
  tibble(g = rep(1, n), x = sample(c(1:4), n, replace = T)),
  tibble(g = rep(2, n), x = sample(c(4:7), n, replace = T)),
  tibble(g = rep(3, n), x = sample(c(7:10), n, replace = T))
)


tb

cor.test(tb$g, tb$x)

