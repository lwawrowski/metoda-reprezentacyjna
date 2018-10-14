library(readxl)
library(tidyverse)

# 5 - wieś, 4 - miasto, 3 - miejskowiejskie, 2 - wiejskie, 1 - miejskie

d <- read_xlsx("03_proba_losowa/dane_gus.xlsx")

d_exp <- d[rep(seq_len(nrow(d)), d$wartosc),1:4] %>%
  mutate(id=1:nrow(d_exp)) %>%
  mutate(id=sprintf("%06d",id))

save(d_exp, file="operat.RData")

# losowanie proste

d_sample <- d_exp %>%
  sample_frac(size = 0.04)

w <- nrow(d_exp)/nrow(d_sample)

d_sample <- d_sample %>%
  mutate(waga=w)

sum(d_sample$waga)

table(d_exp$wiek)
table(d_sample$wiek)

# losowanie złożone

d_sample2 <- d_exp %>%
  group_by(plec) %>%
  sample_frac(size = 0.04)