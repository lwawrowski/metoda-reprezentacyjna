library(tidyverse)

load("data/wybory_braki.rda")

summary(proba1_braki)

12 == NA
is.na(12)

sum(complete.cases(proba1_braki))

proba1_bezbrakow <- proba1_braki %>% 
  filter(complete.cases(proba1_braki))

682/916

library(survey)

schemat1 <- svydesign(ids = ~1, strata = ~warstwa, 
                      weights = ~waga, data = proba1)

schemat1_braki <- svydesign(ids = ~1, strata = ~warstwa, 
                            weights = ~waga, data = proba1_braki)

svymean(x = ~frekwencja, design = schemat1)
svymean(x = ~frekwencja, design = schemat1_braki, na.rm = TRUE)

library(VIM)

aggr(proba1_braki)

proba1_hd <- hotdeck(data = proba1_braki, variable = "frekwencja", 
                     ord_var = c("wyborcy", "typ_obszaru"))

plot(proba1$frekwencja, proba1_hd$frekwencja)

schemat1_hd <- svydesign(ids = ~1, strata = ~warstwa, 
                         weights = ~waga, data = proba1_hd)

svymean(x = ~frekwencja, design = schemat1_hd)

proba1_knn <- kNN(data = proba1_braki, variable = "frekwencja",
                  dist_var = c("wyborcy", "typ_obszaru"), k = 5)

plot(proba1$frekwencja, proba1_knn$frekwencja)

schemat1_knn <- svydesign(ids = ~1, strata = ~warstwa, 
                         weights = ~waga, data = proba1_knn)

svymean(x = ~frekwencja, design = schemat1_knn)

proba1_reg <- regressionImp(formula = frekwencja ~ wyborcy + typ_obszaru,
                            data = proba1_braki)

summary(proba1_reg)

plot(proba1$frekwencja, proba1_reg$frekwencja)

schemat1_reg <- svydesign(ids = ~1, strata = ~warstwa, 
                          weights = ~waga, data = proba1_reg)

svymean(x = ~frekwencja, design = schemat1_reg, na.rm = TRUE)
