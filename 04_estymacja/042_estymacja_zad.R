library(tidyverse)
library(survey)

data("nhanes")

# liczebność populacji

sum(____)

# liczba psu i warstw

nhanes %>% 
  distinct(____)

nhanes %>% 
  distinct(____)

# rozkład wag

summary(____)
hist(____)

# dodajemy kolumnę z dochodem wygenerowanego z rozkładu normalnego

nhanes <- nhanes %>%
  mutate(income=____)

# definiujemy schemat losowania

schemat_warstwy <- svydesign(____)
summary(schemat_warstwy)

# wartość globalna dochodu
svytotal()

# wartość średnia dochodu
svymean()

# wartość średnia dochodu w grupach
svyby()

# zapisanie do zbioru i obliczenie względnego błędu oszacowania
income_race <- svyby()

income_race <- income_race %>%
  mutate(cv=____)