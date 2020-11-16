library(tidyverse)

load("data/wybory.rda")

# 1. Ile obwodów głosowania miało frekwencję powyżej 80%?

frekwencja80 <- dane_czyste %>%
  filter(frekwencja > 80)

# 2. Ile obwodów głosowania znajduje się w Poznaniu?

obw_poznan <- dane_czyste %>% 
  filter(powiat == "Poznań")

# 3. Ile jest obwodów według typu obszaru?

# wyświetla wynik w konsoli
dane_czyste %>% 
  count(typ_obszaru)

# tworzy obiekt w pamięci
count_typ_obszaru <- dane_czyste %>% 
  count(typ_obszaru)

# 4. Jaka była średnia frekwencja w województwach?

frekwencja_woj <- dane_czyste %>% 
  group_by(wojewodztwo) %>% 
  summarise(srednia_frekwencja=mean(frekwencja))

# 5. Gdzie była największa różnica pomiędzy kandydatami?

dane_roznica <- dane_czyste %>% 
  mutate(roznica=abs(andrzej_sebastian_duda - rafal_kazimierz_trzaskowski)) %>% 
  filter(roznica != 100) %>% 
  select(typ_obwodu, gmina, powiat, andrzej_sebastian_duda, rafal_kazimierz_trzaskowski, roznica) %>% 
  top_n(10, roznica) %>% 
  arrange(roznica)

# Jaki średni wynik uzyskał Andrzej Duda na statkach?

dane_czyste %>% 
  group_by(typ_obszaru) %>% 
  summarise(srednia=mean(andrzej_sebastian_duda, na.rm = TRUE))

dane_czyste %>% 
  filter(typ_obszaru == "statek") %>% 
  summarise(srednia=mean(andrzej_sebastian_duda, na.rm = TRUE))





