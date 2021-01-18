library(tidyverse)

load("data/wybory_nonr.rda")

sum(proba1$waga)
sum(proba1_nonr$waga)

library(survey)

schemat1 <- svydesign(ids = ~1, strata = ~warstwa, weights = ~waga, data = proba1)
schemat1_nonr <- svydesign(ids = ~1, strata = ~warstwa, weights = ~waga, data = proba1_nonr)

sum(populacja$wyborcy)

svytotal(x = ~wyborcy, design = schemat1)

svytotal(x = ~wyborcy, design = schemat1_nonr)

# przekrój województwo

populacja %>% 
  count(wojewodztwo)

proba1 %>% 
  group_by(wojewodztwo) %>% 
  summarise(sum(waga))

proba1_nonr %>% 
  group_by(wojewodztwo) %>% 
  summarise(sum(waga))

# kalibracja w przekroju województw

mf <- model.frame(~wojewodztwo, populacja)
mm <- model.matrix(~wojewodztwo, mf)
pop_woj <- colSums(mm)
pop_woj

schemat1_kal1 <- calibrate(design = schemat1_nonr, formula = ~wojewodztwo, population = pop_woj)

proba1_nonr$waga_kal1 <- weights(schemat1_kal1)

sum(proba1_nonr$waga_kal1)

plot(proba1_nonr$waga, proba1_nonr$waga_kal1)

proba1_nonr %>% 
  group_by(wojewodztwo) %>% 
  summarise(sum(waga_kal1))

svytotal(x = ~wyborcy, design = schemat1_kal1)

# przekrój typ gminy

populacja %>% 
  count(typ_gminy)

proba1 %>% 
  group_by(typ_gminy) %>% 
  summarise(sum(waga))

proba1_nonr %>% 
  group_by(typ_gminy) %>% 
  summarise(sum(waga))

# kalibracja w przekroju województw i typu gminy

mf <- model.frame(~wojewodztwo+typ_gminy, populacja)
mm <- model.matrix(~wojewodztwo+typ_gminy, mf)
pop_woj_tg <- colSums(mm)
pop_woj_tg

schemat1_kal2 <- calibrate(design = schemat1_nonr, formula = ~wojewodztwo+typ_gminy, 
                           population = pop_woj_tg)

proba1_nonr$waga_kal2 <- weights(schemat1_kal2)

plot(proba1_nonr$waga, proba1_nonr$waga_kal2)

proba1_nonr %>% 
  group_by(typ_gminy) %>% 
  summarise(sum(waga_kal2))

proba1_nonr %>% 
  group_by(wojewodztwo) %>% 
  summarise(sum(waga_kal2))

svytotal(x = ~wyborcy, design = schemat1_kal2)
