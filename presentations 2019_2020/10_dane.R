library(tidyverse)
library(haven)

pisa <- read_sas("data/cy07_msu_sch_qqq.sas7bdat")

save(pisa, file="data/pisa_school.RData")

pisa %>%
  count(CNT)

pisa_pl <- pisa %>%
  filter(CNT == "POL")
  
sum(pisa_pl$W_FSTUWT_SCH_SUM)
sum(pisa_pl$W_SCHGRNRABWT)

pisa_tch <- read_sas("data/cy07_msu_tch_qqq.sas7bdat")
save(pisa_tch, file="data/pisa_tch.RData")

pisa_students <- read_sas("data/cy07_msu_stu_qqq.sas7bdat")

pisa_cnt <- pisa_students %>% 
  count(CNT)

pisa_students_eu <- pisa_students %>% 
  filter()


cn <- apipop %>% 
  group_by(cname) %>% 
  summarise(suma=sum(enroll))

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
summary(dstrat)
svymean(~api00, dstrat)
cv(svyby(~api00, ~cname, dstrat, svymean))

apistrat %>% 
  count(cname)

a <- apipop %>% 
  group_by(cname) %>% 
  summarise(mean(api99))

# ESS

ess <- read_sav("data/ESS9PL.sav")
save(ess, file = "data/ess.RData")
sum(ess$dweight)

load("data/PISA2012.RData")

# PGSS

load("data/pgss2010.RData")

pgss_2010$weight_pop <- pgss_2010$weight/nrow(pgss_2010)*32674100
summary(pgss_2010$weight_pop)

save(pgss_2010, file = "data/pgss2010.rda")
