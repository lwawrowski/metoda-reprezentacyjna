library(tidyverse)
library(survey)

data(api)

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

apipop %>%
  count(stype)

apistrat %>%
  count(stype)

svytotal(~stype, dstrat)

# awards

apipop %>%
  count(awards)

apistrat %>%
  count(awards)

svytotal(~awards, dstrat)


# braki danych

apinonresp <- apistrat %>%
  sample_frac(0.8)

dnonresp <- svydesign(id=~1,strata=~stype, weights=~pw, data=apinonresp, fpc=~fpc)

svytotal(~stype, dnonresp)

mf <- model.frame(~stype, apipop)
mm <- model.matrix(~stype, mf)
pop_stype <- colSums(mm)

dcalib <- calibrate(design = dnonresp, formula = ~stype, population = pop_stype)

svytotal(~stype, dcalib)

apinonresp <- apinonresp %>%
  mutate(cw=weights(dcalib))

