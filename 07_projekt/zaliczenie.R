# Na podstawie danych o gospodarstwach w populacji generalnej, zaproponuj dwa sposoby losowania, które pozwolą uzyskać 
# oszacowania zmiennych .... w przekroju .... ze względnym błędem oszacowania nie przekraczającym .... %.

# zmienne
# liczba osób bezrobotnych
# liczba osób z wykształceniem
# liczba osób niepełnosprawnych
# średni dochód
# średni wiek
# średnia wielkość gospodarstwa
# stopa bezrobocia
# wskaźnik zatrudnienia
# odsetek niepełnosprawnych
# odsetek osób z wykształceniem

cecha1 <- c("liczba osób bezrobotnych", "liczba osób biernych", "liczba osób pracujących", 
            "liczba osób z wykształceniem podstawowym", "liczba osób z wykształceniem średnim", "liczba osób z wykształceniem wyższym",
            "liczba osób niepełnosprawnych",
            "średni dochód", "średni wiek", "średnia wielkość gospodarstwa")

cecha2 <- c("stopa bezrobocia", "wskaźnik zatrudnienia", "odsetek osób z wykształceniem podstawowym",
            "odsetek osób z wykształceniem średnim", "odsetek osób z wykształceniem wyższym", "odsetek osób niepełnosprawnych")

# przekroje
# województwa
# klasa miejscowości
# regiony
# stopień urbanizacji

przekroj <- c("województwa", "regionu", "klasy miejscowości", "stopnia urbanizacji")

# błąd oszacowania
# 2% - 10%

blad <- 2:10

# Krótka prezentacja powinna zawierać 
# 1. Zadanie projektowe
# 2. zastosowane schematy
# 3. Rozmiar próby i uzyskane wyniki

# Kryteria oceny:
# - osiągnięcie celu - 5 punktów
# - przeprowadzenie kalibracji - 2 punkt
# - sposób prezentacji wyników - 1 punkt

# - utworzenie dodatkowej warstwy spoza zbioru danych - 1 punkt
# - dodatkowy sposób losowania - 1 punkt
# - 3 najlepsze wyniki - 1 punkt


# wyloswanie próby - utworzenie warstw
# gospodarstwa domowe i charakterystyka głów gospodarstw
# wiek 18-67
# płeć

library(exams)

exams2html("07_projekt/zadanie.Rmd", n = 1, name = "test_html", dir = "07_projekt")

exams2moodle("07_projekt/zadanie.Rmd", n = 100, 
             name = "zadanie_projekt1",
             dir = "07_projekt",
             edir = "07_projekt",
             points = 0, 
             rule = "none")


library(tidyverse)
library(survey)
library(readxl)

degurba <- read_xlsx("projekt1/degurba.xlsx") %>%
  select(teryt, DEGURBA_CODE, region)

ludnosc <- read_xlsx("projekt1/ludnosc.xlsx") %>%
  select(teryt, klm, ludnosc) %>%
  group_by(teryt) %>%
  summarise(ludnosc=sum(ludnosc),
            mw=mean(klm)) %>%
  ungroup() %>%
  mutate(klm=if_else(ludnosc >= 500000, 6, if_else(ludnosc >= 200000 & ludnosc <= 499999 & mw == 2, 5, 
                                                   if_else(ludnosc >= 100000 & ludnosc <= 199999 & mw == 2, 4, 
            if_else(ludnosc >= 20000 & ludnosc <= 99999 & mw == 2, 3, if_else(ludnosc < 20000 & mw == 2, 2, 1))))),
         woj=substr(teryt,1,2))

table(ludnosc$mw, ludnosc$klm)

populacja <- inner_join(ludnosc, degurba) %>%
  mutate(sr=runif(2478, min = 2.5, max = 3.5),
         gosp=round(ludnosc/sr)) %>%
  select(teryt, region, woj, degurba=DEGURBA_CODE, klm, gosp)

gospodarstwa <- populacja[rep(seq_len(nrow(populacja)), populacja$gosp),1:5]

gospodarstwa$id_gospodarstwa <- 1:nrow(gospodarstwa)

save(gospodarstwa, file="projekt1/populacja.RData")

saveRDS(gospodarstwa, file="projekt1/populacja.rds")

# zmienne

gosp_cechy <- gospodarstwa %>%
  mutate(dochod = rchisq(nrow(gospodarstwa), 4))

proba <- gospodarstwa %>%
  group_by(woj, klm) %>%
  sample_frac(size = 0.002)

pop_woj <- gospodarstwa %>%
  group_by(woj) %>%
  count() %>%
  rename(woj_n=n)

pop_klm <- gospodarstwa %>%
  group_by(klm) %>%
  count() %>%
  rename(klm_n=n)


badanie <- inner_join(proba, gosp_cechy)
badanie_n <- inner_join(inner_join(badanie, pop_woj),pop_klm)

schemat <- survey::svydesign(id = ~ id_gospodarstwa, fpc = ~woj_n + klm_n, data=badanie_n, strata = ~woj+klm, nest = TRUE)

survey::svymean(~dochod, schemat)
a <- survey::svyby(~dochod, by = ~ woj, schemat, svymean)

a$se/a$dochod*100
