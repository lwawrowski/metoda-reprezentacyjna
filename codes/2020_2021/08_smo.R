library(tidyverse)

load("data/stopa_bezr.rda")

mean(stopa_bezr_pow$y_ht_cv)

model <- lm(formula = y_ht ~ x1 + x2, data = stopa_bezr_pow)
summary(model)

library(emdi)

model_fh <- fh(fixed = y_ht ~ x1 + x2, vardir = "y_ht_var", combined_data = stopa_bezr_pow, 
               domains = "id", MSE = TRUE)
summary(model_fh)

stopa_bezr_pow_fh <- stopa_bezr_pow %>% 
  mutate(y_fh=model_fh$ind$FH,
         y_fh_se=sqrt(model_fh$MSE$FH),
         y_fh_cv=y_fh_se/y_fh,
         u=model_fh$model$random_effects[,1],
         gamma=model_fh$model$gamma$Gamma)

plot(stopa_bezr_pow_fh$y_ht, stopa_bezr_pow_fh$y_fh)

plot(stopa_bezr_pow_fh$y_ht_cv, stopa_bezr_pow_fh$y_fh_cv)

plot(stopa_bezr_pow_fh$gamma, stopa_bezr_pow_fh$y_ht_se)

