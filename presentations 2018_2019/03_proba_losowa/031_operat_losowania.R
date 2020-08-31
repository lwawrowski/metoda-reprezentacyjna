library(readxl)
library(tidyverse)

d <- read_xlsx("03_proba_losowa/dane_gus.xlsx")

operat <- d[rep(seq_len(nrow(d)), d$wartosc),1:4]

operat <- operat %>%
  filter(!(wiek %in% c("0-4", "5-9", "10-14")))

operat <- operat %>%
  mutate(id=1:nrow(operat)) %>%
  mutate(id=sprintf("%06d",id))

save(operat, file="03_proba_losowa/operat.RData")
