library(tidyverse)

load("data/wybory_proba.rda")

proba1_braki <- proba1

set.seed(123)
braki <- sample(1:nrow(proba1), 45)
proba1_braki$typ_gminy[braki] <- NA
braki <- sample(1:nrow(proba1), 90)
proba1_braki$frekwencja[braki] <- NA
braki <- sample(1:nrow(proba1), 65)
proba1_braki$wyborcy[braki] <- NA
braki <- sample(1:nrow(proba1), 25)
proba1_braki$andrzej_sebastian_duda[braki] <- NA
braki <- sample(1:nrow(proba1), 26)
proba1_braki$rafal_kazimierz_trzaskowski[braki] <- NA

summary(proba1_braki)

proba2_braki <- proba2

set.seed(125)
braki <- sample(1:nrow(proba2), 45)
proba2_braki$typ_gminy[braki] <- NA
braki <- sample(1:nrow(proba2), 90)
proba2_braki$frekwencja[braki] <- NA
braki <- sample(1:nrow(proba2), 65)
proba2_braki$wyborcy[braki] <- NA
braki <- sample(1:nrow(proba2), 25)
proba2_braki$andrzej_sebastian_duda[braki] <- NA
braki <- sample(1:nrow(proba2), 26)
proba2_braki$rafal_kazimierz_trzaskowski[braki] <- NA

summary(proba2_braki)

save(proba1, proba1_braki, proba2, proba2_braki, file = "data/wybory_braki.rda")