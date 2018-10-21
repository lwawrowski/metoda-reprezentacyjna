# Na podstawie danych o gospodarstwach w populacji generalnej, zaproponuj dwa sposoby losowania, które pozwolą uzyskać 
# oszacowania zmiennej .... w przekroju .... ze względnym błędem oszacowania nie przekraczającym .... %.

# zmienne
# liczba osób bezrobotnych
# liczba osób z wykształceniem
# liczba osób niepełnosprawnych
# średni dochód
# średni wiek
# stopa bezrobocia
# wskaźnik zatrudnienia
# odsetek niepełnosprawnych
# wykształcenie

# przekroje
# województwa
# klasa miejscowości
# regiony
# stopień urbanizacji
# liczba osób

# błąd oszacowania
# 2% - 10%

# Krótka prezentacja powinna zawierać 
# 1. Zadanie projektowe
# 2. zastosowane schematy
# 3. Rozmiar próby i uzyskane wyniki

# Kryteria oceny:
# - 3 sposoby losowania - 2 punkty
# - osiągnięcie celu - 1 punkt
# - przeprowadzenie kalibracji - 1 punkt
# - sposób prezentacji wyników - 1 punkt
# - najlepszy wynik - 1 punkt

# wyloswanie próby - utworzenie warstw
# gospodarstwa domowe i charakterystyka głów gospodarstw
# wiek 18-67
# płeć
library(tidyverse)
library(survey)

a <- emdi::eusilcA_smp

hist(rchisq(1000, 4))
