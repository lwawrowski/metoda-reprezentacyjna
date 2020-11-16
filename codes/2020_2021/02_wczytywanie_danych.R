library(tidyverse)
library(readxl)

dane <- read_xlsx(path = "data/wybory2020.xlsx")

summary(dane)

mean(dane$Frekwencja)
mean(dane$`% głosów nieważnych`)

dane_czyste <- janitor::clean_names(dane)

summary(dane_czyste)

mean(dane_czyste$percent_glosow_niewaznych, na.rm = TRUE)

save(dane_czyste, file = "data/wybory.rda")
