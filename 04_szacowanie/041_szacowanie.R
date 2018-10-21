library(tidyverse)

# https://www.statisticshowto.datasciencecentral.com/finite-population-correction-factor/

load("03_proba_losowa/operat.RData")

# losowanie proste

proba_prosta <- operat %>%
  sample_frac(size = 0.004)

w <- nrow(operat)/nrow(proba_prosta)

proba_prosta <- proba_prosta %>%
  mutate(waga=w,
         pop=nrow(operat))

sum(proba_prosta$waga)

library(survey)

schemat <- svydesign(id = ~id, data = proba_prosta, fpc = ~pop)
summary(schemat)

schemat_wagi <- svydesign(id = ~id, data = proba_prosta, weights = ~waga)
summary(schemat_wagi)

waga1 <- unique(weights(schemat))
waga2 <- unique(weights(schemat_wagi))

svytotal(~mw, schemat) # inaczej oblicza błąd standardowy
svytotal(~mw, schemat_wagi)

proba_plec <- operat %>%
  group_by(plec) %>%
  sample_frac(size = 0.004) 

liczebnosc_operat <- operat %>%
  count(plec) %>%
  rename(pop=n)

proba_plec_pop <- inner_join(proba_plec, liczebnosc_operat)

schemat_plec <- svydesign(id = ~id, strata = ~plec, data = proba_plec_pop, fpc = ~pop)
summary(schemat_plec)

liczebnosc_proba <- proba_plec %>%
  count(plec) %>%
  rename(proba=n)

wagi <- inner_join(liczebnosc_operat, liczebnosc_proba) %>%
  mutate(waga=pop/proba,
         p=proba/pop)

proba_plec_wagi <- inner_join(proba_plec, wagi)

schemat_plec2 <- svydesign(id = ~id, strata = ~plec, data = proba_plec_wagi, weights = ~waga)
summary(schemat_plec2)

svytotal(~mw, schemat_plec)
svytotal(~mw, schemat_plec2)

waga1 <- unique(weights(schemat_plec))
waga2 <- unique(weights(schemat_plec2))

