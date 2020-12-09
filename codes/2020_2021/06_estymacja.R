library(tidyverse)

load("data/wybory_proba.rda")

summary(proba1$waga)
summary(proba2$waga)

sum(proba1$waga)
sum(proba2$waga)

mean(proba1$andrzej_sebastian_duda)

mean(populacja$andrzej_sebastian_duda)

library(survey)

schemat1 <- svydesign(ids = ~1, strata = ~warstwa,
                      weights = ~waga, data = proba1)

schemat1 <- svydesign(ids = ~symbol_kontrolny, strata = ~warstwa,
                      weights = ~waga, data = proba1)

summary(schemat1)

svymean(x = ~andrzej_sebastian_duda, design = schemat1)

typ_gm_ad <- svyby(formula = ~andrzej_sebastian_duda, by = ~typ_gminy, 
                   design = schemat1, FUN = svymean)

schemat2 <- svydesign(ids = ~symbol_kontrolny, strata = ~warstwa,
                      weights = ~waga, data = proba2)

svyby(formula = ~rafal_kazimierz_trzaskowski, by = ~wojewodztwo, 
      design = schemat2, FUN = svymean)

# wartość globalna

proba1 <- proba1 %>% 
  mutate(glosujacy=round(wyborcy*frekwencja/100),
         glosujacyAD=round(glosujacy*andrzej_sebastian_duda/100),
         glosujacyRT=glosujacy-glosujacyAD)

sum(proba1$glosujacyAD)  
  
sum(proba1$glosujacyAD*proba1$waga)  

populacja <- populacja %>% 
  mutate(glosujacy=round(wyborcy*frekwencja/100),
         glosujacyAD=round(glosujacy*andrzej_sebastian_duda/100),
         glosujacyRT=glosujacy-glosujacyAD)

sum(populacja$glosujacyAD)

schemat1 <- svydesign(ids = ~symbol_kontrolny, strata = ~warstwa,
                      weights = ~waga, data = proba1)

svytotal(x = ~glosujacyAD, design = schemat1)
  
# wskaźnik

svyratio(numerator = ~glosujacyAD, denominator = ~glosujacy, design = schemat1)

# przedział ufności

confint(svytotal(x = ~glosujacyAD, design = schemat1))

confint(typ_gm_ad, level = 0.9)

# względny błąd oszacowania

cv(svytotal(x = ~glosujacyAD, design = schemat1))

cv(typ_gm_ad)  

proba2 <- proba2 %>% 
  mutate(glosujacy=round(wyborcy*frekwencja/100),
         glosujacyAD=round(glosujacy*andrzej_sebastian_duda/100),
         glosujacyRT=glosujacy-glosujacyAD)

schemat2 <- svydesign(ids = ~symbol_kontrolny, strata = ~warstwa,
                      weights = ~waga, data = proba2)

cv(svyby(formula = ~glosujacyRT, by = ~wojewodztwo, design = schemat2, FUN = svytotal))


