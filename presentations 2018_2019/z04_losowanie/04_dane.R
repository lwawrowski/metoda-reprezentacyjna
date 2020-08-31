library(readxl)
library(tidyverse)

d <- read_xlsx("04_losowanie_zaoczne/dane_gus.xlsx")

populacja <- d[rep(seq_len(nrow(d)), d$wartosc),1:4]

populacja <- populacja %>%
  filter(!(wiek %in% c("0-4", "5-9", "10-14")))

populacja <- populacja %>%
  mutate(id=1:nrow(populacja)) %>%
  mutate(id=sprintf("%06d",id))

bezr <- sample(c(1,0), nrow(populacja), prob = c(0.05, 0.95), replace = T)
wydatki1 <- rnorm(n=324123, mean = 1000, 200)
wydatki2 <- rnorm(n=nrow(populacja)-324123, mean = 1400, 300)

wydatki <- c(wydatki1, wydatki2)

populacja$bezr <- bezr
populacja$wydatki <- round(abs(wydatki))

save(populacja, file="04_losowanie_zaoczne/populacja.RData")

proba <- populacja %>%
  group_by(wiek) %>%
  sample_frac(0.00131)

pop_wiek <- populacja %>%
  group_by(wiek) %>%
  count()

proba <- inner_join(proba, pop_wiek)

schemat <- survey::svydesign(ids = ~id, strata = ~wiek, fpc = ~n, data=proba)

proba <- proba %>%
  ungroup() %>%
  mutate(waga=as.numeric(weights(schemat))) %>%
  select(kod:id,waga,bezr,wydatki)

proba_id <- sample(1:nrow(proba), 0.2*nrow(proba))

proba_braki <- proba
proba_braki$bezr[proba_id] <- NA
proba_braki$wydatki[proba_id] <- NA

save(proba_braki, file="04_losowanie_zaoczne/proba_braki.RData")
