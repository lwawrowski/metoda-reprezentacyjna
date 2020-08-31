library(tidyverse)
library(survey)

load("dane.RData")

# stworzenie warstwy

populacja <- populacja %>%
  mutate(warstwa1=paste0(degurba))

# losowanie próby 

proba1 <- populacja %>%
  group_by(warstwa1) %>%
  sample_frac(0.001)

# obliczenie wag

liczebnosc_warstw <- populacja %>%
  group_by(warstwa1) %>%
  count()

proba1 <- inner_join(proba1, liczebnosc_warstw, by = "warstwa1")

proba1_schemat <- svydesign(ids = ~id_gospodarstwa, strata = ~warstwa1, fpc = ~n, data = proba1)

wagi <- weights(proba1_schemat)

# dodanie wag do zbioru

proba1 <- proba1 %>%
  ungroup() %>%
  mutate(waga=wagi)

# dołączanie informacji o odpowiedziach

proba1_badanie <- merge(proba1, badanie, by = "id_gospodarstwa", all.x = T)

# usuwanie brakóW odpowiedzi

proba1_badanie_bez_brakow <- proba1_badanie %>%
  filter(!is.na(wiek)) %>%
  mutate(wyk_sr=ifelse(wyksztalcenie==2, 1, 0),
         mianownik=1)

proba1_bez_brakow_schemat <- svydesign(ids = ~id_gospodarstwa, weights = ~waga, strata = ~warstwa1,
                                       fpc = ~n, data = proba1_badanie_bez_brakow)

# kalibracja w ramach degurba

pop_total <- colSums(model.matrix(~degurba,model.frame(~degurba,populacja)))

proba1_kalibracja <- calibrate(design = proba1_bez_brakow_schemat, formula = ~degurba, population = pop_total)

# estymacja średniej liczby osób w gosp. 

srednia_wielkosc_gosp <- svyby(formula = ~liczba_osob, by = ~degurba, 
                               design = proba1_kalibracja, FUN = svymean)

# względny błąd oszacowania i przedziały ufności

q <- qnorm(1-0.1/2)

srednia_wielkosc_gosp <- srednia_wielkosc_gosp %>%
  mutate(cv=se/liczba_osob*100,
         dg=liczba_osob-q*se,
         gg=liczba_osob+q*se)


# estymacja odsetka średniego wykształcenia

wyk_sr_degurba <- svyby(formula = ~wyk_sr, denominator = ~mianownik, by = ~degurba, 
                            design = proba1_kalibracja, FUN = svyratio)

names(wyk_sr_degurba) <- c("degurba", "ods_wyk", "se")

# względny błąd oszacowania i przedziały ufności

q <- qnorm(1-0.1/2)

wyk_sr_degurba <- wyk_sr_degurba %>%
  mutate(cv=se/ods_wyk*100,
         dg=ods_wyk-q*se,
         gg=ods_wyk+q*se)

