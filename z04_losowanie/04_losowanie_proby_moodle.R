library(tidyverse)
library(survey)

load("z04_losowanie/populacja.RData")

# losowanie proste

proba_prosta <- populacja %>%
  sample_frac(size = ____)

proba_prosta <- proba_prosta %>%
  mutate(n=nrow(populacja))

schemat_proba_prosta <- svydesign(ids = ____, fpc = ____, data=proba_prosta)

summary(schemat_proba_prosta)

wagi <- weights(schemat_proba_prosta)

# własności wag

sum(wagi)

proba_prosta <- proba_prosta %>%
  mutate(waga=wagi)

# struktura płci w populacji i probie

populacja %>% 
  group_by(____) %>%
  count()

proba_prosta %>%
  group_by(____) %>%
  summarise(suma=sum(waga))

# losowanie warstwowe

populacja <- populacja %>%
  mutate(warstwa1=plec)

proba_warstwa1 <- populacja %>%
  group_by(____) %>%
  sample_frac(____)

pop_warstwa1 <- populacja %>%
  group_by(____) %>%
  count()

proba_warstwa1_n <- left_join(proba_warstwa1, pop_warstwa1)

schemat_proba_warstwa1 <- svydesign(ids = ____, strata = ____, fpc = ____, data=proba_warstwa1_n)
summary(schemat_proba_prosta)

wagi <- weights(schemat_proba_warstwa1)

# własności wag

sum(wagi)

proba_warstwa1_n <- proba_warstwa1_n %>%
  ungroup() %>%
  mutate(waga=wagi)

# struktura w populacji i próbie

populacja %>% 
  group_by(____) %>%
  count()

proba_warstwa1_n %>%
  group_by(____) %>%
  summarise(suma=sum(waga))

# inny przekrój

populacja %>% 
  group_by(____) %>%
  count()

proba_warstwa1_n %>%
  group_by(____) %>%
  summarise(suma=sum(waga))

# wiele zmiennych w warstwie

populacja <- populacja %>%
  mutate(warstwa2=paste0(mw,"_",plec))
