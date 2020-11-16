library(tidyverse)

load("data/wybory.rda")

zad1 <- dane_czyste %>% 
  filter(gmina == "gm. Radków")

zad1 <- dane_czyste %>% 
  filter(kod_teryt == "261304")

length(unique(dane_czyste$gmina))
length(unique(dane_czyste$kod_teryt))

zad2 <- dane_czyste %>% 
  filter(frekwencja >= 62, wojewodztwo == "wielkopolskie")

zad2 <- dane_czyste %>% 
  filter(frekwencja >= 62 & wojewodztwo == "wielkopolskie")

zad2 <- dane_czyste %>% 
  filter(frekwencja >= 62) %>% 
  filter(wojewodztwo == "wielkopolskie")

zad3 <- dane_czyste %>% 
  filter(powiat == "wieruszowski") %>% 
  summarise(minimum = min(frekwencja))

zad3 <- dane_czyste %>% 
  filter(substr(kod_teryt,1,4) == "1018") %>% 
  summarise(minimum = min(frekwencja))

zad3 <- dane_czyste %>% 
  group_by(wojewodztwo, powiat) %>% 
  summarise(minimum = min(frekwencja))
