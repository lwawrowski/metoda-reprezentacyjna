library(tidyverse)
library(survey)

data("nhanes")

# liczebność populacji

sum(nhanes$WTMEC2YR)

# liczba psu i warstw

nhanes %>% 
  distinct(SDMVPSU)

nhanes %>% 
  distinct(SDMVSTRA)

# rozkład wag

summary(nhanes$WTMEC2YR)
hist(nhanes$WTMEC2YR)

# dodajemy kolumnę z dochodem

nhanes <- nhanes %>%
  mutate(income=rnorm(n=nrow(nhanes), 4000, 400),
         mianownik=1,
         licznik=sample(c(0,1), nrow(nhanes), replace = T))

# definiujemy schemat losowania

schemat_warstwy <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = nhanes, nest = TRUE)
summary(schemat_warstwy)

svytotal(x = ~income, design = schemat_warstwy)

svymean(x = ~income, design = schemat_warstwy)

svyby(formula = ~income, by =~race, design = schemat_warstwy, FUN = svymean)

income_race <- svyby(formula = ~income, by =~race, design = schemat_warstwy, FUN = svymean)

income_race <- income_race %>%
  mutate(cv=se/income*100)

svyratio(numerator = ~licznik, denominator = ~mianownik, design = schemat_warstwy)

svyby(formula = ~licznik, denominator = ~mianownik, by =~race, design = schemat_warstwy, FUN = svyratio)

nonweighted <- nhanes %>%
  count(race) %>%
  mutate(percent=n/sum(n)*100,
         race=factor(race, labels = c("Hispanic", "non-Hispanic white", "non-Hispanic black", "other"), ordered = T))
  
ggplot(nonweighted, aes(x=race, y=percent)) + 
  geom_bar(stat = "identity") + 
  xlab("") + ylab("Percent") +
  theme_light() +
  ylim(0,100) +
  ggsave("04_estymacja/nonweighted.png", height = 8, width = 6)

weighted <- svyby(formula = ~WTMEC2YR, by =~race, design = schemat_warstwy, FUN = svytotal)

weighted <- weighted %>%
  mutate(percent=WTMEC2YR/sum(a$WTMEC2YR)*100) %>%
  mutate(race=factor(race, labels = c("Hispanic", "non-Hispanic white", "non-Hispanic black", "other"), ordered = T))

ggplot(weighted, aes(x=race, y=percent)) + 
  geom_bar(stat = "identity") +
  xlab("") + ylab("Percent") +
  theme_light() +
  ylim(0,100) +
  ggsave("04_estymacja/weighted.png", height = 8, width = 6)


load("03_proba_losowa/operat.RData")

# losowanie proste

proba_prosta <- operat %>%
  sample_frac(size = 0.004)

w <- nrow(operat)/nrow(proba_prosta)

proba_prosta <- proba_prosta %>%
  mutate(waga=w,
         pop=nrow(operat))

sum(proba_prosta$waga)

library(survey)

schemat <- svydesign(id = ~id, data = proba_prosta, fpc = ~pop)
summary(schemat)

schemat_wagi <- svydesign(id = ~id, data = proba_prosta, weights = ~waga)
summary(schemat_wagi)

waga1 <- unique(weights(schemat))
waga2 <- unique(weights(schemat_wagi))

svytotal(~mw, schemat) # inaczej oblicza błąd standardowy
svytotal(~mw, schemat_wagi)

proba_plec <- operat %>%
  group_by(plec) %>%
  sample_frac(size = 0.004) 

liczebnosc_operat <- operat %>%
  count(plec) %>%
  rename(pop=n)

proba_plec_pop <- inner_join(proba_plec, liczebnosc_operat)

schemat_plec <- svydesign(id = ~id, strata = ~plec, data = proba_plec_pop, fpc = ~pop)
summary(schemat_plec)

liczebnosc_proba <- proba_plec %>%
  count(plec) %>%
  rename(proba=n)

wagi <- inner_join(liczebnosc_operat, liczebnosc_proba) %>%
  mutate(waga=pop/proba,
         p=proba/pop)

proba_plec_wagi <- inner_join(proba_plec, wagi)

schemat_plec2 <- svydesign(id = ~id, strata = ~plec, data = proba_plec_wagi, weights = ~waga)
summary(schemat_plec2)

svytotal(~mw, schemat_plec)
svytotal(~mw, schemat_plec2)

waga1 <- unique(weights(schemat_plec))
waga2 <- unique(weights(schemat_plec2))




