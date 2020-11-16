library(survey)

data(api)

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, 
                  data=apistrat, fpc=~fpc)
svymean(~api00, dstrat)

bstrat <- as.svrepdesign(design = dstrat,
                         type = "bootstrap",
                         replicates = 500)
svymean(~api00, bstrat)

# zadanie
dclus1<-svydesign(id=~dnum, weights=~pw, 
                  data=apiclus1, fpc=~fpc)

svymean(~api99, dclus1)

bclus1 <- as.svrepdesign(design = dclus1,
                         type = "bootstrap",
                         replicates = 500)
svymean(~api99, bclus1)
