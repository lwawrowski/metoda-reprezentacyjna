library(tidyverse)
library(survey)

load("05_estymacja/operat.RData")

populacja <- operat %>%
  mutate(niepelnosprawnosc=sample(c(1,0), nrow(operat), replace = T, c(0.17, 0.83)))

save(populacja, file="05_estymacja/populacja.RData")

load("05_estymacja/populacja.RData")

# losowanie proste

proba1 <- populacja %>%
  sample_frac(0.02)

schemat1 <- svydesign(ids = ~id, data = proba1)

wagi_schemat1 <- weights(schemat1)
summary(wagi_schemat1)

proba1 <- proba1 %>%
  mutate(pop=nrow(populacja))

schemat2 <- svydesign(ids = ~id, data = proba1, fpc = ~pop)

wagi_schemat2 <- weights(schemat2)
summary(wagi_schemat2)

# losowanie warstwowe

proba2 <- populacja %>%
  group_by(mw, wiek) %>%
  sample_frac(0.02)

proba2_wagi <- proba2 %>%
  group_by(mw, wiek) %>%
  count() %>%
  rename(n_proba=n)

populacja_wagi <- populacja %>%
  group_by(mw, wiek) %>%
  count() %>%
  rename(n_pop=n)

wagi <- inner_join(proba2_wagi, populacja_wagi) %>%
  mutate(waga=n_pop/n_proba) %>%
  select(-n_pop, -n_proba)

proba2_wagi <- inner_join(proba2, wagi) %>%
  mutate(mianownik=1)

schemat3 <- svydesign(ids = ~id, strata = ~mw+plec, weights = ~waga, data=proba2_wagi)

wagi_schemat3 <- weights(schemat3)
summary(wagi_schemat3)

# total

niep_total <- svytotal(x = ~niepelnosprawnosc, design = schemat3)
niep_total

sum(populacja$niepelnosprawnosc)

# 95% przedzial ufnosci

se <- sqrt(attr(niep_total, "var"))
y <- niep_total[1]
q <- qnorm(0.975)

c(y-q*se, y+q*se)

# total by

niep_total_wiek <- svyby(formula = ~niepelnosprawnosc, by = ~wiek, design = schemat3, FUN = svytotal)

# skonstuować przedziały ufności

# ratio

svyratio(numerator = ~niepelnosprawnosc, denominator = ~ mianownik, design = schemat3)

svyby(formula = ~niepelnosprawnosc, denominator = ~ mianownik, by = ~wiek, design = schemat3, FUN = svyratio)
