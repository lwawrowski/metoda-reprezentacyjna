library(tidyverse)
library(survey)

data(api)

help(api)

schemat <- svydesign(id=~1, strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# wartość globalna --------------------------------------------------------

# wartość globalna uczących się języka angielskiego (zmienna ell)
svytotal(____)

# wartość prawdziwa
sum(____)

# wartość średnia ---------------------------------------------------------

# wartość średnia API
svymean(____)

# wartość prawdziwa
mean(____)

# estymacja w grupach -----------------------------------------------------

# według typu szkoły
svyby(____)

# zapisanie do zbioru i obliczenie względnego błędu oszacowania
api_stype <- ____

api_stype <- api_stype %>%
  mutate(cv=____)

# wartość prawdziwa

apipop %>%
  group_by(____) %>%
  summarise_at("api00", "mean")

# Zadanie 1
# Oszacuj liczbę uczniów pierwszego roku (zmienna mobility) w przekroju typu szkoły oraz 
# oblicz względny błąd oszacowania.




# estymacja wskaźnika -----------------------------------------------------

# odsetek uczniów biorących udział w teście (api.stu/enroll)
svyratio(____)

# według typu szkoły
svyby(____)

# przedział ufności -------------------------------------------------------

# dla wartości globalnej
ell_total <- svytotal(~ell, schemat)

se <- sqrt(attr(ell_total, "var"))
y <- ell_total[1]
q <- qnorm(0.975)

c(y-q*se, y+q*se)

# wg typu szkoły




# Zadanie 2
# Dla wcześniej oszacowanej liczby uczniów pierwszego roku w przekroju typu szkoły wyznacz 90% przedział ufności.

