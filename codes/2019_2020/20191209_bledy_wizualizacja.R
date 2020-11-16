library(tidyverse)

load("data/berkley.RData")

berkley %>% 
  group_by(dept, gender) %>% 
  summarise(sum_adm=sum(admitted),
            sum_tot=sum(total)) %>% 
  mutate(p=sum_adm/sum_tot)

plot(berkley$admitted, berkley$rejected)

library(survey)

data(api)

hist(apipop$enroll)

# wykres punktowy
ggplot(data = apipop, aes(x = enroll, 
                          y = api.stu,
                          color = stype)) +
  geom_point() +
  xlab("Students enrolled") +
  ylab("Students tested") +
  scale_color_discrete(name = "School type") +
  xlim(0,4500) +
  ggtitle("Academic Performance Index") +
  theme_light() +
  ggsave("wykres.png")

# wykres słupkowy - dane jednostkowe
ggplot(data = apipop, aes(x = stype)) +
  geom_bar(fill = "yellow")

# schemat losowania
dstrat<-svydesign(id=~1,strata=~stype, 
                  weights=~pw, data=apistrat, 
                  fpc=~fpc)

# szacowanie 
api_stype <- svyby(formula = ~api00, 
                   by = ~stype,
                   design = dstrat, 
                   FUN = svymean)

# wykres słupkowy - dane z estymacji
ggplot(data = api_stype, aes(x = stype,
                             y = se)) +
  geom_col(fill = "green")

# szacowanie 
api_stype_aw <- svyby(formula = ~api00, 
                      by = ~stype+awards,
                      design = dstrat, 
                      FUN = svymean)

# wykres słupkowy - dane z estymacji
ggplot(data = api_stype_aw, aes(x = stype,
                                y = api00,
                                fill = awards)) +
  geom_col(position = "dodge")
















