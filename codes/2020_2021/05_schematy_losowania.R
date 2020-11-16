library(tidyverse)
library(sampling)

load("data/wybory.rda")

# losowanie proste

proba_prosta <- dane_czyste %>% 
  sample_n(100) %>% 
  mutate(prawd=n()/nrow(dane_czyste),
         waga=1/prawd)

# wagi w próbie sumują się do liczebności populacji
sum(proba_prosta$waga)

summary(proba_prosta$frekwencja)

# losowanie proporcjonalne

proba_prop <- dane_czyste %>% 
  filter(frekwencja > 0) %>% 
  sample_frac(0.00367, weight = 1/frekwencja)

summary(proba_prop$frekwencja)

dane_prop <- dane_czyste %>% 
  filter(frekwencja > 0) %>% 
  mutate(prawd=inclusionprobabilities(a = frekwencja, n = 100))

summary(dane_prop$prawd)

# prawdopodobieństwa w populacji sumują się do liczebności próby
sum(dane_prop$prawd)

proba_prop <- dane_prop %>% 
  sample_n(100, weight = prawd) %>% 
  mutate(waga=1/prawd)

summary(proba_prop$prawd)

summary(proba_prop$waga)

# w losowaniu proporcjonalnym wagi w próbie nie sumują się do liczebności populacji
sum(proba_prop$waga)

library(bdl)

ludnosc <- get_data_by_variable(varId = 72305, unitLevel = 6, year = 2019)

save(ludnosc, file = "data/ludnosc.rda")

load("data/ludnosc.rda")

ludnosc_teryt <- ludnosc %>% 
  mutate(kod_teryt=str_c(str_sub(id,3,4),str_sub(id,8,11))) %>% 
  mutate(typ_gminy=str_sub(id,12,12)) %>% 
  filter(!typ_gminy %in% c("4", "5")) %>% 
  select(kod_teryt, ludnosc=val)

wybory_ludnosc <- left_join(x = dane_czyste, y = ludnosc_teryt)

length(unique(dane_czyste$kod_teryt))
length(unique(ludnosc_teryt$kod_teryt))

summary(wybory_ludnosc$ludnosc)

braki_danych <- wybory_ludnosc %>% 
  filter(is.na(ludnosc))

wybory_ludnosc_bb <- wybory_ludnosc %>% 
  filter(!is.na(ludnosc)) %>% 
  mutate(ludnosc_kat4=cut(x = ludnosc, 
                          breaks = c(min(ludnosc), 8000, 17000, 60000, max(ludnosc)),
                          include.lowest = TRUE))

summary(wybory_ludnosc_bb$ludnosc)

length(unique(wybory_ludnosc_bb$ludnosc))

n_ludnosc <- wybory_ludnosc_bb %>% 
  count(ludnosc_kat4) %>% 
  mutate(n_proba = round(0.01*n))

proba_warstwa <- strata(data = wybory_ludnosc_bb, 
                        stratanames = "ludnosc_kat4", 
                        size = n_ludnosc$n_proba)

proba_warstwa_obwody <- getdata(wybory_ludnosc_bb, proba_warstwa)

sum(1/proba_warstwa_obwody$Prob)


