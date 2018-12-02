library(tidyverse)
library(survey)

load("z04_losowanie/populacja.RData")

# losowanie proste

proba_prosta <- populacja %>%
  sample_frac(size = 0.001)

proba_prosta <- proba_prosta %>%
  mutate(n=nrow(populacja))

schemat_proba_prosta <- svydesign(ids = ~id, fpc = ~n, data=proba_prosta)
summary(schemat_proba_prosta)

wagi <- weights(schemat_proba_prosta)

# własności wag

sum(wagi)

proba_prosta <- proba_prosta %>%
  mutate(waga=wagi)

# struktura płci w populacji i probie

populacja %>% 
  group_by(plec) %>%
  count()

proba_prosta %>%
  group_by(plec) %>%
  summarise(suma=sum(waga))

# losowanie warstwowe

populacja <- populacja %>%
  mutate(warstwa1=plec)

proba_warstwa1 <- populacja %>%
  group_by(warstwa1) %>%
  sample_frac(0.001)

pop_warstwa1 <- populacja %>%
  group_by(warstwa1) %>%
  count()

proba_warstwa1_n <- left_join(proba_warstwa1, pop_warstwa1)

schemat_proba_warstwa1 <- svydesign(ids = ~id, strata = ~warstwa1, fpc = ~n, data=proba_warstwa1_n)
summary(schemat_proba_prosta)

wagi <- weights(schemat_proba_warstwa1)

# własności wag

sum(wagi)

proba_warstwa1_n <- proba_warstwa1_n %>%
  ungroup() %>%
  mutate(waga=wagi)

# struktura w populacji i próbie

populacja %>% 
  group_by(plec) %>%
  count()

proba_warstwa1_n %>%
  group_by(plec) %>%
  summarise(suma=sum(waga))

# inny przekrój

populacja %>% 
  group_by(mw) %>%
  count()

proba_warstwa1_n %>%
  group_by(mw) %>%
  summarise(suma=sum(waga))

# wiele zmiennych w warstwie

populacja <- populacja %>%
  mutate(warstwa2=paste0(mw,"_",plec))
