library(tidyverse)

load("03_proba_losowa/operat.RData")

# losowanie proste

proba_prosta <- operat %>%
  sample_frac(size = 0.04)

w <- nrow(operat)/nrow(proba_prosta)

proba_prosta <- proba_prosta %>%
  mutate(waga=w)

sum(proba_prosta$waga)

# losowanie warstwowe

proba_plec <- operat %>%
  group_by(plec) %>%
  sample_frac(size = 0.04)

liczebnosc_operat <- operat %>%
  count(plec) %>%
  rename(operat=n)

liczebnosc_proba <- proba_plec %>%
  count(plec) %>%
  rename(proba=n)

wagi <- inner_join(liczebnosc_operat, liczebnosc_proba) %>%
  mutate(waga=operat/proba,
         p=proba/operat)

proba_plec_wagi <- inner_join(proba_plec, wagi)

sum(proba_plec_wagi$waga)

# losowanie warstwowe - mw i wiek

proba_mw_wiek <- operat %>%
  group_by(mw,wiek) %>%
  sample_frac(size = 0.02)

liczebnosc_operat <- operat %>%
  count(mw,wiek) %>%
  rename(operat=n)

liczebnosc_proba <- proba_mw_wiek %>%
  count(mw,wiek) %>%
  rename(proba=n)

wagi <- inner_join(liczebnosc_operat, liczebnosc_proba) %>%
  mutate(waga=operat/proba)

proba_mw_wiek_wagi <- inner_join(proba_mw_wiek, wagi)

sum(proba_mw_wiek_wagi$waga)
