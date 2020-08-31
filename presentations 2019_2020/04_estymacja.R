library(tidyverse)
library(survey)

data("nhanes")

sum(nhanes$WTMEC2YR)

warstwy <- nhanes %>%
  count(SDMVSTRA, SDMVPSU)

summary(nhanes)

nhanes %>%
  count(RIAGENDR, HI_CHOL)

schemat <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes) 
summary(schemat)

a <- svytotal(x = ~HI_CHOL, design = schemat, na.rm = T, deff = T)

b <- svyby(formula = ~HI_CHOL, by = ~RIAGENDR, design = schemat, FUN = svytotal, na.rm = T)

data(api)

cv(a)

confint(a, level = 0.9)
confint(b, level = 0.9)

deff(a)

# http://r-survey.r-forge.r-project.org/survey/survey-wss-2010.pdf


data(api)

sum(apisrs$pw)
sum(apistrat$pw)
sum(apiclus1$pw)
sum(apiclus2$pw)

dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

svytotal(x = ~api.stu, dstrat)

svyby(formula = ~api.stu, by = ~stype, design = dstrat, FUN = svytotal)

svymean(x = ~api00, design = dstrat)

mean(apipop$api00)


a <- svyby(formula = ~api00, by = ~stype, design = dstrat, FUN = svymean)

confint(a, level = 0.95)

apistrat %>%
  group_by(stype) %>%
  summarise(mean(api00))
