library(tidyverse)
library(readxl)

obwody <- read_xlsx("data/obwody_glosowania.xlsx")
obwody <- janitor::clean_names(obwody)

obwody_poznan <- obwody %>%
  filter(powiat == "Poznań") %>%
  filter(mieszkancy != 0)

losowanie_proste <- obwody_poznan %>%
  sample_n(30)

losowanie_proste <- obwody_poznan %>%
  sample_frac(0.1) %>%
  mutate(prob=23/227)

UPsystematic(losowanie_proste$prob)

filter(losowanie_proste, as.logical(UPsystematic(losowanie_proste$prob)))

library(sampling)

# dwumian newtona 10 po 4

mozliwe_proby <- writesample(4,10)

choose(100,30)

# 29372339821610843812848224

data(belgianmunicipalities)
pik=inclusionprobabilities(belgianmunicipalities$Tot04,200)
# the first-order inclusion probabilities for each municipality
data.frame(pik=pik,name=belgianmunicipalities$Commune)
# the inclusion probability sum is equal to the sample size
sum(pik)

# warstwowe

liczebnosci_warstwa <- rep(10, 41)

set.seed(123)
los_warstwa1 <- strata(data = obwody, stratanames = c("numer_okregu_do_sejmu"), size = liczebnosci_warstwa)
los_warstwa2 <- strata(data = obwody, stratanames = c("numer_okregu_do_sejmu"), size = liczebnosci_warstwa, pik = obwody$wyborcy)

obwody_warstwa1 <- getdata(obwody, los_warstwa1)
obwody_warstwa2 <- getdata(obwody, los_warstwa2)

sum(1/obwody_warstwa1$Prob)
sum(1/obwody_warstwa2$Prob)

obwody_warstwa1 %>%
  group_by(numer_okregu_do_sejmu) %>%
  summarise(sum(wyborcy))

obwody_warstwa2 %>%
  group_by(numer_okregu_do_sejmu) %>%
  summarise(sum(wyborcy))


# losowanie zespołowe - całe gminy

id_zespol <- cluster(data = obwody, clustername = c("gmina"), size = 10)

obwody_zespol <- getdata(obwody, id_zespol)

sejm <- obwody %>%
  count(numer_okregu_do_senatu) %>%
  mutate(proc=round(0.0182*n))

los_warstwa1 <- strata(data = obwody, stratanames = c("numer_okregu_do_senatu"), size = sejm$proc)

sum(1/los_warstwa1$Prob)
