library(tidyverse)
library(sampling)

load("data/wybory.rda")

obwody <- readxl::read_excel("data/obwody_glosowania2020.xlsx") %>% 
  janitor::clean_names() %>% 
  select(nr_okw, kod_teryt=teryt_gminy, wyborcy, siedziba=pelna_siedziba, typ_obwodu, typ_obszaru) %>% 
  arrange(kod_teryt)

wyniki_obwody <- inner_join(dane_czyste, obwody) %>% 
  select(symbol_kontrolny:percent_glosow_niewaznych,percent_glosow_waznych:wyborcy) %>% 
  group_by_at(vars(symbol_kontrolny:rafal_kazimierz_trzaskowski)) %>% 
  summarise(wyborcy=round(mean(wyborcy))) %>% 
  ungroup() %>% 
  arrange(kod_teryt, numer_obwodu)

populacja <- wyniki_obwody %>% 
  filter(typ_obwodu == "stały")

woj_warstwa <- populacja %>% 
  distinct(wojewodztwo) %>% 
  mutate(woj_warstw=sprintf("%02d",1:nrow(.)))

set.seed(123)
frek_cuts <- classInt::classIntervals(populacja$frekwencja, n = 4, style = "kmeans")
pop_cuts <- classInt::classIntervals(populacja$wyborcy, n = 3, style = "quantile")

populacja_warstwy <- populacja %>% 
  inner_join(., woj_warstwa) %>% 
  mutate(frek_warstw = cut(frekwencja, frek_cuts$brks, c("1", "2", "3", "4"), include.lowest=T),
         wyb_warstw = cut(wyborcy, pop_cuts$brks, c("1", "2", "3"), include.lowest=T),
         roznica = andrzej_sebastian_duda-mean(andrzej_sebastian_duda),
         roznica_warstw = ifelse(roznica > -10 & roznica < 10, "1", "2"),
         warstwa1 = str_c(woj_warstw,wyb_warstw,frek_warstw),
         warstwa2 = str_c(woj_warstw,wyb_warstw,frek_warstw,roznica_warstw))


warstwa1_count <- populacja_warstwy %>% 
  count(warstwa1) %>% 
  mutate(n_los=ifelse(n < 100, 3, round(0.03*n)),
         n_los=ifelse(n_los > n, 1, n_los))

warstwa2_count <- populacja_warstwy %>% 
  count(warstwa2) %>% 
  mutate(warstwa2_small=ifelse(n < 10, "00000", warstwa2)) %>% 
  select(-n)

populacja_warstwy_small <- left_join(populacja_warstwy, warstwa2_count)

warstwa2_count_small <- populacja_warstwy_small %>% 
  count(warstwa2_small) %>%
  mutate(n_los=ifelse(n < 100, 2, round(0.03*n)),
         n_los=ifelse(n_los > n, 2, n_los))

# losowanie

set.seed(123)

populacja_warstwy1 <- populacja_warstwy %>% 
  arrange(warstwa1)

proba_warstwa1 <- sampling::strata(populacja_warstwy1, stratanames = "warstwa1", size = warstwa1_count$n_los)

proba1 <- getdata(populacja, proba_warstwa1) %>% 
  select(-ID_unit,-warstwa1) %>% 
  rename(prob=Prob, warstwa=Stratum) %>% 
  mutate(waga=1/prob)

populacja_warstwy2 <- populacja_warstwy_small %>% 
  arrange(warstwa2_small)

proba_warstwa2 <- sampling::strata(populacja_warstwy2, stratanames = "warstwa2_small", size = warstwa2_count_small$n_los)

proba2 <- getdata(populacja, proba_warstwa2) %>% 
  select(-ID_unit,-warstwa2_small) %>% 
  rename(prob=Prob, warstwa=Stratum) %>% 
  mutate(waga=1/prob)

save(populacja, proba1, proba2, file = "data/wybory_proba.rda")

