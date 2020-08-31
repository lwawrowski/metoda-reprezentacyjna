library(tidyverse)

load("operat.RData")

# losowanie proste

proba_prosta <- operat %>%
  sample____(size = ____)

w <- nrow(____)/nrow(____)

proba_prosta <- proba_prosta %>%
  mutate(waga=____)

sum(___)

# losowanie warstwowe

proba_plec <- operat %>%
  group_by(____) %>%
  sample____(size = ____)

liczebnosc_operat <- operat %>%
  count(____) %>%
  rename(operat=n)

liczebnosc_proba <- proba_plec %>%
  count(____) %>%
  rename(proba=n)

wagi <- inner_join(liczebnosc_operat, liczebnosc_proba) %>%
  mutate(waga=____)

proba_plec_wagi <- inner_join(proba_plec, wagi)

sum(____)