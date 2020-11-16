library(tidyverse)

load("data/pgss.RData")

pgss %>%
  count(q14a)

# usuwanie braków
is.na(5)
is.na(NA)
!is.na(NA)

complete.cases(pgss)

pgss_q34 <- pgss %>% 
  filter(!is.na(q34))

pgss_cc <- pgss %>% 
  filter(complete.cases(pgss))

# zastępowanie braków

imp_q21 <- median(pgss$q21, na.rm = TRUE)

pgss_q21 <- pgss %>% 
  mutate(q21=if_else(is.na(q21), imp_q21, q21))

summary(pgss$q21)
summary(pgss_q21$q21)

library(VIM)

# hot-deck
pgss_hd <- hotdeck(data = pgss, 
                   variable = "q21", 
                   ord_var = c("hompop", "q9age"),
                   domain_var = "voiev16")

summary(pgss_hd$q21)

# kNN
pgss_knn <- kNN(data = pgss, 
                variable = "q21", 
                dist_var = c("hompop", "q9age", "voiev16"))

summary(pgss_knn$q21)

plot(pgss_hd$q21, pgss_knn$q21)











