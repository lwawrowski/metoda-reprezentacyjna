library(tidyverse)
library(haven)

# http://www.ads.org.pl/opis-szczeg.php?id=91

pgss <- read_spss("13_projekt/P0091SAV.sav")

pgss_2010 <- pgss %>%
  filter(pgssyear==2010)

save(pgss_2010, file="13_projekt/pgss10.RData")

a <- rowSums(is.na(pgss_2010))

pgss_2010_zmienne <- pgss_2010 %>%
  select(recordid, weight, voiev16, region6, size, hompop, adults, q8, q9age, q14a, q16a, q21, q22g, q32, q34, q37, q61e, q61h, q61r, q87a, q87b, q87c, q87d, q87e,
         q104:q105, q107, q110, q111, q144a:q144o, in1f, in4e, in5e, in7a) %>%
  mutate_all(as.numeric) %>%
  mutate(q21=ifelse(q21>=998,NA,q21),
         q22g=ifelse(q22g>=99998,NA,q22g),
         q32=ifelse(q32>=99998,NA,q32),
         q34=ifelse(q34>=999998,NA,q34),
         in4e=ifelse(in4e>=900000,NA,in4e),
         in5e=ifelse(in5e>=900000,NA,in5e))

pgss <- pgss_2010_zmienne

save(pgss, file="13_projekt/pgss.RData")
