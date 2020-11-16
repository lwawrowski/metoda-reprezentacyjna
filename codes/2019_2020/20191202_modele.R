library(survey)

data(api)

dstrat<-svydesign(id=~1,strata=~stype, 
                  weights=~pw, data=apistrat, 
                  fpc=~fpc)

svytotal(x = ~api.stu, design = dstrat)

# model bez wyrazu wolnego

alfa <- svyratio(numerator = ~api.stu, 
                 denominator = ~enroll,
                 design = dstrat)

x <- sum(apipop$enroll, na.rm = TRUE)

predict(object = alfa, total = x)

plot(apistrat$api.stu, apistrat$enroll)

# ELL

svytotal(x = ~ell, design = dstrat)

alfa_ell <- svyratio(numerator = ~ell,
                 denominator = ~enroll,
                 design = dstrat)

predict(object = alfa_ell, total = x)

plot(apistrat$ell, apistrat$enroll)

# model z wyrazem wolnym

lm(api.stu ~ enroll, data = apistrat)

m <- svyglm(api.stu ~ enroll, design = dstrat)

x_df <- data.frame(enroll=x)

predict(object = m, newdata = x_df, total = nrow(apipop))

# ELL

m_ell <- svyglm(ell ~ enroll, design = dstrat)

predict(object = m_ell, newdata = x_df, total = nrow(apipop))










