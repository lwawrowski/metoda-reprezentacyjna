library(tidyverse)
library(readxl)

# wczytanie danych
obwody <- read_xlsx("data/obwody_glosowania.xlsx")
# uporządkowanie nazw kolumn
obwody <- janitor::clean_names(obwody)

# wybór obwodów losowania z Poznania
obwody_poznan <- obwody %>% 
  filter(powiat == "Poznań", mieszkancy != 0)

# losowanie proste

# losowanie 30 obwodów
los_proste_n <- obwody_poznan %>%
  sample_n(30)

# losowanie 10% obwodów
los_proste_frac <- obwody_poznan %>% 
  sample_frac(0.1) %>%
  # mutate(prob=23/227)
  mutate(prob=n()/nrow(obwody_poznan), # obliczenie prawdopodobieństwa dostania się do próby
         waga=1/prob) # obliczenia wagi z próby

sum(los_proste_frac$waga) # suma wag = liczebność populacji

library(sampling)  

# losowania proste 1000 obwodów
los_proc <- obwody %>% 
  sample_n(1000)

# liczba potencjalnych wyborców w próbie
sum(los_proc$wyborcy)

# losowanie proporcjonalne
# wprost propocjonalnie do liczby wyborców
# większe prawdopodobieństwo mają obwody z dużą liczbą wyborców
los_proc <- obwody %>% 
  sample_n(1000, weight = wyborcy)

# liczba potencjalnych wyborców w próbie - większa niż w losowaniu prostym
sum(los_proc$wyborcy)

# odwrotnie propocjonalnie do liczby wyborców
# większe prawdopodobieństwo mają obwody z małą liczbą wyborców
los_proc <- obwody %>% 
  sample_n(1000, weight = 1/wyborcy)

# liczba potencjalnych wyborców w próbie - mniejsza niż w losowaniu prostym
sum(los_proc$wyborcy)

# prawdopodobieństwa dostania się do próby 
# próba 1000 obwodów proporcjonalnie do liczby wyborców
inclusionprobabilities(obwody$wyborcy, 1000)

# dodanie prawdopodobieństw do zbioru
obwody <- obwody %>%
  mutate(p_prop=inclusionprobabilities(wyborcy, 1000))

# przeprowadzenie losowania 
los_prop <- obwody %>%
  sample_n(1000, weight = wyborcy)

# w losowaniu propocjonalnym suma wag nie jest równa liczebności populacji
sum(1/los_prop$p_prop)

# losowanie warstwowe
# wyznaczenie liczebności próby w warstwach
n_sejm <- obwody %>% 
  count(numer_okregu_do_sejmu) %>% 
  mutate(n_proba=round(0.04*n))

sum(n_sejm$n_proba)

# zastosowanie losowania warstwowego
los_sejm <- strata(data = obwody, 
                   stratanames = "numer_okregu_do_sejmu",
                   size = n_sejm$n_proba)

# połączenie wylosowanych jednostek i danych oryginalnych
los_sejm_obwody <- getdata(obwody, los_sejm)

# suma wag = liczebność populacji
sum(1/los_sejm_obwody$Prob)

# zadanie
n_senat <- obwody %>% 
  count(numer_okregu_do_senatu) %>% 
  mutate(n_proba=round(0.018205*n))

sum(n_senat$n_proba)

los_senat <- strata(data = obwody, 
                   stratanames = "numer_okregu_do_senatu",
                   size = n_senat$n_proba)

los_senat_obwody <- getdata(obwody, los_senat)

# suma wag = liczebność populacji
sum(1/los_senat_obwody$Prob)

# losowanie zespołowe
# losowanie 10 gmin i wszystkich obwodów w tych gminach
los_zespol <- cluster(data = obwody, 
                      clustername = "gmina",
                      size = 10)

los_zespol_obwody <- getdata(obwody, los_zespol)

# w losowaniu zespołowym suma wag nie jest równa liczebności populacji
sum(1/los_zespol_obwody$Prob)

# losowanie systematyczne
# losowanie co k-tej jednostki w zależności od liczebności próby
los_syst <- obwody %>%
  mutate(prob=2000/nrow(.))

# wskazanie, które jednostki wylosować
UPsystematic(los_syst$prob)

los_syst <- los_syst %>% 
  mutate(w_probie=UPsystematic(prob)) %>%
  filter(w_probie == 1)

# suma wag = liczebność populacji
sum(1/los_syst$prob)
