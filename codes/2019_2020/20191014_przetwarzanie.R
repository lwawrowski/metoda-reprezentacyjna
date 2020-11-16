library(tidyverse)

filmy <- read_csv(file = "data/movies.csv")
summary(filmy)

komedie <- filmy %>% 
  filter(genre == "Comedy") %>% 
  select(title, year)

filmy2012 <- filmy %>%
  filter(year == 2012)

filmy <- filmy %>% 
  mutate(age=2019-year)

filmy2012 %>% 
  count(genre)







