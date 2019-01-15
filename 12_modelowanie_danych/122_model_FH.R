library(tidyverse)
library(readxl)

load("12_modelowanie_danych/zm_podreg.RData")

ht <- read_xlsx("12_modelowanie_danych/ht_2012.xlsx")

ht <- ht %>%
  mutate(arpr_ht_cv=arpr_ht_se/arpr_ht)

zm <- zm_podreg %>%
  select(ps_u_o_korz_z_pom, child_dep_ratio, ae_o_o_sbez, udzial_sam_rodz, mieszk_laz, zrod_roln, wyn_przec_wyn_brutto)

names(zm) <- c("pomoc_spoleczna", "wsk_zaleznosci_dzieci", "stopa_bezrobocia", "wsk_samotnych_rodzicow", "wsk_mieszkan_lazienka",
               "wsk_utrzym_roln", "przecietne_wynagrodzenie")

dane <- cbind(ht, zm)

load("12_modelowanie_danych/nbq_podreg.RData")

save(dane, podreg_nbq_w, file = "12_modelowanie_danych/dane.RData")
