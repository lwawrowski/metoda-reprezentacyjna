library(tidyverse)
library(survey)

load("populacja.RData")

# losowanie warstwowe

populacja_warstwa <- populacja %>%
  mutate(warstwa=paste0(____,____))

proba2 <- populacja_warstwa %>%
  group_by(warstwa) %>%
  sample_frac(____)

proba2_wagi <- proba2 %>%
  group_by(warstwa) %>%
  count() %>%
  rename(n_proba=n)

populacja_wagi <- populacja_warstwa %>%
  group_by(warstwa) %>%
  count() %>%
  rename(n_pop=n)

wagi <- inner_join(proba2_wagi, populacja_wagi) %>%
  mutate(waga=____) 

proba2_wagi <- inner_join(proba2, wagi) %>%
  mutate(mianownik=____)

schemat1 <- svydesign(ids = ____, strata = ____, weights = ____, data=proba2_wagi)

wagi_schemat1 <- weights(____)
summary(wagi_schemat1)

schemat2 <- svydesign(ids = ____, strata = ____, fpc = ____, data=proba2_wagi)

wagi_schemat2 <- weights(____)
summary(wagi_schemat2)

# estymacja wartości globalnej liczby osób niepełnosprawnych

niep_total <- svytotal(x = ____, design = schemat1)
niep_total

niep_total <- svytotal(x = ____, design = schemat2)
niep_total

# wartość prawdziwa

sum(populacja$niepelnosprawnosc)

# 95% przedzial ufnosci

str(niep_total)

se_niep <- sqrt(attr(niep_total, "var"))
y_niep <- niep_total[1]
q_95 <- qnorm(1-____/2)

c(y_niep-q_95*se_niep, y_niep+q_95*se_niep)

# estymacja wartości globalnej liczby osób niepełnosprawnych w grupach wieku

niep_total_wiek <- svyby(formula = ____, by = ____, design = schemat2, FUN = ____)

niep_total_wiek <- niep_total_wiek %>%
  mutate(dolna_granica=____,
         gorna_granica=____)

niep_total_wiek_pop <- populacja %>%
  group_by(____) %>%
  summarise(niep_sum=sum(____))

niep_total_wiek_proba_pop <- inner_join(niep_total_wiek, niep_total_wiek_pop) %>%
  mutate(w_przedziale=ifelse(niep_sum >= dolna_granica & niep_sum <= gorna_granica, ____, ____))

# estymacja odsetka niepełnosprawnych

svyratio(numerator = ____, denominator = ____, design = schemat2)

# estymacja odsetka niepełnosprawnych w grupach wieku

svyby(formula = ____, denominator = ____, by = ____, design = schemat2, FUN = ____)
