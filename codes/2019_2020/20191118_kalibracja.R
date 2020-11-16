library(tidyverse)
library(survey)

# zadanie
data(api)

apipop %>% count(stype)

apistrat %>% count(stype)

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, 
                  data=apistrat, fpc=~fpc)

svytotal(~stype, dstrat)

# awards
apipop %>% count(awards)
svytotal(~awards, dstrat)

# braki danych

apinonresp <- apistrat %>%
  sample_frac(0.8)

sum(apistrat$pw)
sum(apinonresp$pw)

dnonresp<-svydesign(id=~1,strata=~stype, weights=~pw,
                    data=apinonresp, fpc=~fpc)

svytotal(~stype, dnonresp)

# kalibracja
mf <- model.frame(~stype, apipop)
mm <- model.matrix(~stype, mf)
pop_stype <- colSums(mm)
pop_stype

cnonresp <- calibrate(design = dnonresp, 
                      formula = ~stype,
                      population = pop_stype)

svytotal(~stype, cnonresp)

weights(cnonresp)

apinonresp <- apinonresp %>% 
  mutate(cw=weights(cnonresp))

sum(apinonresp$cw)

# porównanie oszacowań
svymean(~api00, dstrat)
svymean(~api00, dnonresp)
svymean(~api00, cnonresp)

svytotal(~enroll, dstrat)
svytotal(~enroll, dnonresp)
svytotal(~enroll, cnonresp)







