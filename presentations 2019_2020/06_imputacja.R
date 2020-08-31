library(tidyverse)

load("data/pgss.RData")

pgss <- pgss %>%
  mutate(waga=weight/1263*30000000)

sum(pgss$waga/30000000*1263)

pgss %>%
  count(in4e)

plot(pgss$in4e, pgss$in5e)

sum(is.na(pgss$q32))

names(sort(table(pgss$q14a), decreasing = T))[1]

hist(pgss$q21)

pgss_imp <- pgss %>%
  mutate(q21_imp=if_else(is.na(q21), median(q21, na.rm=T), q21))

summary(pgss_imp$q21)
summary(pgss_imp$q21_imp)

hist(pgss_imp$q21_imp)

library(VIM)

pgss2 <- hotdeck(pgss, "q14a")

pgss %>%
  filter(q14a!=98) %>% 
  ggplot(aes(x=as.factor(q14a))) +
  geom_bar()

marginplot()

pgss2 <- hotdeck(pgss, "in5e", c("hompop", "q9age"), "region6")

summary(pgss2$in5e)
summary(pgss$in5e)

pgss_sorted <- pgss %>%
  select(size, hompop, q32) %>% 
  arrange(size, hompop, q32)

pgss_sorted2 <- hotdeck(pgss_sorted, "q32", c("size", "hompop"))

summary(pgss)

pgss_knn <- kNN(pgss, variable = c("in5e"), dist_var = c("hompop", "q9age", "region6"), k = 5)

summary(pgss_knn$in5e)

plot(pgss2$in5e, pgss_knn$in5e)

# nie działa
pgss_reg <- regressionImp(formula = q32 ~ size + hompop + q9age, data = pgss)

marginplot(pgss[,c("q32", "q34")])
marginplot(pgss[,c("q14a", "q16a")])



marginplot(pgss[,c("q21","q14a")])

library(mice)

md.pairs(pgss[,c("q14a", "q16a")])

imp <- mice(pgss[,c("q14a", "q16a")])

imp$imp$q14a
