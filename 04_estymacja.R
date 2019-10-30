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

svytotal(x = ~HI_CHOL, design = schemat, na.rm = T)

svyby(formula = ~HI_CHOL, by = ~RIAGENDR, design = schemat, FUN = svytotal, na.rm = T)
