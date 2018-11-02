library(tidyverse)
library(survey)

load("dane.RData")

# struktura wieku

populacja %>%
  group_by(wiek) %>%
  count()

proba %>%
  group_by(wiek) %>%
  summarise(n_proba=sum(waga))

# braki

proba_braki %>%
  group_by(wiek) %>%
  summarise(n_proba_braki=sum(waga))

schemat <- svydesign(ids = ~id, strata = ~wiek, weights = ~waga, data=proba_braki)

# kalibracja w grupach wieku

pop_wiek <- colSums(model.matrix(~wiek,model.frame(~wiek,populacja)))
pop_wiek

kalibracja_wiek <- calibrate(design = schemat, formula = ~wiek, population = pop_wiek)

proba_braki <- proba_braki %>%
  ungroup() %>%
  mutate(waga_kal1=weights(kalibracja_wiek))

# sprawdzenie struktury

proba_braki %>%
  group_by(wiek) %>%
  summarise(n_proba_braki=sum(waga),
            n_proba_kal1=sum(waga_kal1))

# struktura płci

populacja %>%
  group_by(plec) %>%
  count()

proba %>%
  group_by(plec) %>%
  summarise(n_plec=sum(waga))

# braki

proba_braki %>%
  group_by(plec) %>%
  summarise(n_plec_braki=sum(waga),
            n_plec_kal1=sum(waga_kal1))

# kalibracja w grupach wieku i płci

pop_wiek_plec <- colSums(model.matrix(~wiek*plec,model.frame(~wiek*plec,populacja)))
pop_wiek_plec

kalibracja_wiek_plec <- calibrate(design = schemat, formula = ~wiek*plec, population = pop_wiek_plec)

proba_braki <- proba_braki %>%
  ungroup() %>%
  mutate(waga_kal2=weights(kalibracja_wiek_plec))

# sprawdzenie struktury

proba_braki %>%
  group_by(plec) %>%
  summarise(n_proba_braki=sum(waga),
            n_proba_kal1=sum(waga_kal1),
            n_proba_kal2=sum(waga_kal2))