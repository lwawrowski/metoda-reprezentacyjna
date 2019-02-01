library(tidyverse)
library(survey)

load("dane.RData")

# stworzenie warstwy

populacja <- populacja %>%
  mutate(warstwa1=paste0(woj,"_",degurba))

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
  mutate(bezr=ifelse(aktywnosc_ekon==2,1,0), 
         mianownik=ifelse(aktywnosc_ekon %in% c(1,2),1,0))

proba1_bez_brakow_schemat <- svydesign(ids = ~id_gospodarstwa, weights = ~waga, strata = ~warstwa1,
                                       fpc = ~n, data = proba1_badanie_bez_brakow)

# kalibracja w ramach województw

pop_total <- colSums(model.matrix(~degurba,model.frame(~degurba,populacja)))

proba1_kalibracja <- calibrate(design = proba1_bez_brakow_schemat, formula = ~degurba, population = pop_total)

# estymacja stopy bezrobocia

stopa_bezr_degurba <- svyby(formula = ~bezr, denominator = ~mianownik, by = ~degurba, 
                            design = proba1_kalibracja, FUN = svyratio)