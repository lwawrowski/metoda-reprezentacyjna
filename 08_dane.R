library(tidyverse)

berkley <-
  UCBAdmissions %>% 
  as_tibble() %>% 
  pivot_wider(id_cols = c(Dept, Gender),
              names_from = Admit, 
              values_from = n) %>% 
  mutate(Total = Admitted + Rejected) %>%
  janitor::clean_names(.)

save(berkley, file = "data/berkley.RData")

berkley %>%
  group_by(gender) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(proc=admitted/total)

berkley %>%
  group_by(dept, gender) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(proc=admitted/total)
