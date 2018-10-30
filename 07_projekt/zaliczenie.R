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

degurba <- read_xlsx("07_projekt/degurba.xlsx") %>%
  select(teryt, DEGURBA_CODE, region)

ludnosc <- read_xlsx("07_projekt/ludnosc.xlsx") %>%
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

save(gospodarstwa, file="07_projekt/populacja.RData")

load("07_projekt/populacja.RData")

gospodarstwa <- gospodarstwa %>%
  mutate(pow=substr(teryt,1,4))

pow_n  <- gospodarstwa %>%
  group_by(pow) %>%
  count()

prob <- read_xlsx("07_projekt/cechy_prob.xlsx")

niep <- NULL
wyk <- NULL
ae <- NULL
wiek <- NULL
gosp <- NULL

for(i in 1:nrow(prob)){
  niep_i <- sample(x = c(0,1), size = pow_n$n[i], replace = T, prob = as.numeric(prob[i,2:3]))
  niep <- c(niep, niep_i)
  
  wyk_i <- sample(x = 1:3, size = pow_n$n[i], replace = T, prob = as.numeric(prob[i,4:6]))
  wyk <- c(wyk, wyk_i)
  
  ae_i <- sample(x = 1:3, size = pow_n$n[i], replace = T, prob = as.numeric(prob[i,7:9]))
  ae <- c(ae, ae_i)
  
  wiek_i <- sample(x = 16:65, size = pow_n$n[i], replace = T, prob = as.numeric(prob[i,10:59]))
  wiek <- c(wiek, wiek_i)
  
  gosp_i <- sample(x = 1:5, size = pow_n$n[i], replace = T, prob = as.numeric(prob[i,60:64]))
  gosp <- c(gosp, gosp_i)
}

gospodarstwa <- gospodarstwa %>%
  mutate(niepelnosprawnosc=niep,
         wyksztalcenie=wyk,
         aktywnosc_ekon=ae,
         wiek=wiek,
         liczba_osob=gosp)

gospodarstwa <- gospodarstwa %>%
  select(-pow) %>%
  mutate(wiek=ifelse(wyksztalcenie == 3 & wiek <= 24, runif(1, min=25, max=65), wiek))

gospodarstwa <- gospodarstwa %>%
  mutate(wiek=ifelse(wyksztalcenie == 2 & wiek <= 18, runif(1, min=19, max=65), wiek))

dochod <- gospodarstwa %>%
  select(niepelnosprawnosc, wyksztalcenie, aktywnosc_ekon, wiek, liczba_osob) %>%
  mutate(wyz=ifelse(wyksztalcenie==3, 1, 0),
         bezr=ifelse(aktywnosc_ekon==2, 1,0)) %>%
  mutate(dochod=1905*niepelnosprawnosc-800*bezr+2200*wyz-103*wiek-55*liczba_osob+6095+rnorm(nrow(gospodarstwa), 1000,500))

dochod_minus <- dochod %>%
  filter(dochod <= 0)

summary(dochod$dochod)
hist(dochod$dochod)

gospodarstwa <- gospodarstwa %>%
  mutate(dochod=dochod$dochod)

gospodarstwa <- gospodarstwa %>%
  mutate(dochod=round(dochod,2))

gospodarstwa_badanie <- gospodarstwa %>%
  sample_frac(0.454) %>%
  select(id_gospodarstwa:dochod)

save(gospodarstwa_badanie, file="07_projekt/badanie.RData")

badanie <- gospodarstwa_badanie
populacja <- gospodarstwa

save(populacja, badanie, file="07_projekt/dane.RData")

load("07_projekt/dane.RData")

proba <- populacja %>%
  sample_frac(0.001)

proba_badanie <- merge(proba, badanie, by = "id_gospodarstwa", all.x = T)

proba_badanie <- proba_badanie %>%
  mutate(pop=nrow(populacja))

schemat <- svydesign(ids = ~id_gospodarstwa, data = proba_badanie, fpc = ~pop)

proba_badanie <- proba_badanie %>%
  mutate(waga=weights(schemat))

proba_badanie_cc <- proba_badanie %>%
  filter(!is.na(dochod))

schemat2 <- svydesign(ids = ~id_gospodarstwa, data = proba_badanie_cc, fpc = ~pop, weights = ~waga)

svytotal(x = ~niepelnosprawnosc, design = schemat, na.rm=T)
svytotal(x = ~niepelnosprawnosc, design = schemat2)

pop_total <- colSums(model.matrix(~woj*klm,model.frame(~woj*klm,populacja)))
pop_total

kalibracja <- calibrate(design = schemat2, formula = ~woj*klm, population = pop_total)

w <- weights(kalibracja)
summary(w)
sum(w)

plot(weights(schemat2), weights(kalibracja))
summary(weights(schemat2)/weights(kalibracja))

svytotal(x = ~niepelnosprawnosc, design = kalibracja)


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
