library(tidyverse)
library(survey)

data(api)

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

no_rep <- seq(0,1000, by = 5)
no_rep[1] <- 2

se_rep5 <- numeric(length(no_rep))

for(i in 1:length(no_rep)){
  
  boot <- as.svrepdesign(dstrat, type = "bootstrap", replicates = no_rep[i])
  
  result <- svytotal(~api00, boot)
  
  se_rep5[i] <- SE(result)
  
}

plot(no_rep, se_rep)

api00tot <- svytotal(~api00, dstrat)

api00_boot <- data.frame(number=no_rep,
                         se1=se_rep,
                         se2=se_rep2,
                         se3=se_rep3,
                         se4=se_rep4,
                         se5=se_rep5)

api00_boot_long <- api00_boot %>%
  pivot_longer(se1:se5)

ggplot(api00_boot_long, aes(x=number, y=value)) + 
  geom_point() +
  geom_hline(yintercept = SE(api00tot), color = "red") +
  ylab("Błąd standardowy") +
  xlab("Liczba replikacji") +
  theme_light() +
  ggsave("img/bootstrap.png", width = 8, height = 5)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

bclus <- as.svrepdesign(dclus1, type = "bootstrap", replicates = 500)

svytotal(~api99, dclus1)
svytotal(~api99, boot_clus)
