library(tidyverse)
library(survey)

# załadowanie zbioru danych
data("nhanes")

# zadanie - liczba osób z podwyższonym cholesterolem
sum(nhanes$HI_CHOL, na.rm = TRUE)
nhanes %>% count(HI_CHOL)

# w grupach płci
nhanes %>% count(RIAGENDR, HI_CHOL)

# liczebność populacji
sum(nhanes$WTMEC2YR)

# deklaracja schematu - w celu prawidłowego wyznaczenia wariancji
schemat <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, 
                     weights = ~WTMEC2YR, nest = TRUE,
                     data = nhanes)
summary(schemat)

# estymacja wartości globalnej ogółem
svytotal(x = ~HI_CHOL, design = schemat, na.rm = TRUE)

# estymacja wartości globalnej w grupach płci
svyby(formula = ~HI_CHOL, by = ~RIAGENDR, design = schemat,
      FUN = svytotal, na.rm = TRUE) %>% 
  mutate(cv=se/HI_CHOL) # obliczenie względnego błędu szacunku

# zadanie
# estymacja wartości globalnej w grupach wieku
wiek <- svyby(formula = ~HI_CHOL, by = ~agecat, design = schemat,
      FUN = svytotal, na.rm = TRUE) %>% 
  mutate(cv=se/HI_CHOL)

nhanes %>% count(agecat)

nhanes %>% count(agecat, HI_CHOL)

# API - zajęcia w dniu 4.11.2019

# załadowanie zbiorów danych
data(api)

# deklaracja schematu losowania
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, 
                  data=apistrat, fpc=~fpc)

# estymacja wartości globalnej ogółem - liczba uczniów biorących udział w teście kompetencji
svytotal(~api.stu, dstrat, na.rm=TRUE)

# wartość prawdziwa w populacji
sum(apipop$api.stu)

# estymacja wartości globalnej wg typu szkoły - liczba uczniów biorących udział w teście kompetencji
svyby(formula = ~api.stu, by = ~stype, design = dstrat,
      FUN = svytotal)

# estymacja średniego wyniku testu kompetencji ogółem
api00mean <- svymean(x = ~api00, design = dstrat)

# wartość prawdziwa w populacji
mean(apipop$api00)

# przedział ufności
confint(svymean(x = ~api00, design = dstrat))
confint(api00mean, level = 0.9)

# względny błąd oszacowania
cv(api00mean)

# estymacja średniego wyniku testu kompetencji wg typu szkoły
api00stype <- svyby(formula = ~api00, by = ~stype, 
                    design = dstrat, FUN = svymean)

# przedział ufności
confint(api00stype, level = 0.9)

# wartość prawdziwa w populacji
apipop %>%
  group_by(stype) %>%
  summarise(mean(api00))

# estymacja wskaźnika (odsetek uczniów biorących udział w teście) ogółem
svyratio(numerator = ~api.stu, denominator = ~enroll,
         design = dstrat)

# estymacja wskaźnika (odsetek uczniów biorących udział w teście) wg typu szkoły
svyby(formula = ~api.stu, denominator = ~enroll, 
      by = ~stype, design = dstrat,
      FUN = svyratio)
