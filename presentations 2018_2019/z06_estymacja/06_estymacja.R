library(tidyverse)
library(survey)

data(api)

help(api)

schemat <- svydesign(id=~1, strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)


# wartość globalna --------------------------------------------------------

# wartość globalna uczących się języka angielskiego (zmienna ell)
svytotal(~ell, schemat)

# wartość prawdziwa
sum(apipop$ell)

# wartość średnia ---------------------------------------------------------

# wartość średnia API
svymean(~api00, schemat)

# wartość prawdziwa
mean(apipop$api00)

# estymacja w grupach -----------------------------------------------------

# według typu szkoły
svyby(formula = ~api00, by = ~stype, design = schemat, FUN = svymean)

# zapisanie do zbioru i obliczenie względnego błędu oszacowania
api_stype <- svyby(formula = ~api00, by = ~stype, design = schemat, FUN = svymean)

api_stype <- api_stype %>%
  mutate(cv=____)

# wartość prawdziwa

apipop %>%
  group_by(stype) %>%
  summarise_at("api00", "mean")

# Zadanie 1
# Oszacuj liczbę uczniów pierwszego roku (zmienna mobility) w przekroju typu szkoły oraz 
# oblicz względny błąd oszacowania.




# estymacja wskaźnika -----------------------------------------------------

# odsetek uczniów biorących udział w teście (api.stu/enroll)
svyratio(numerator = ~api.stu, denominator = ~enroll, design = schemat)

# według typu szkoły
svyby(formula = ~api.stu, denominator = ~enroll, by = ~stype, design = schemat, FUN = svyratio)

# przedział ufności -------------------------------------------------------

# dla wartości globalnej
ell_total <- svytotal(~ell, schemat)

se <- sqrt(attr(ell_total, "var"))
y <- ell_total[1]
q <- qnorm(0.975)

c(y-q*se, y+q*se)

# wg typu szkoły




# Zadanie
# Dla wcześniej oszacowanej liczby uczniów pierwszego roku w przekroju typu szkoły wyznacz 90% przedział ufności.

