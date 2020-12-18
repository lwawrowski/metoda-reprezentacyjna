library(tidyverse)
library(bdl)
library(emdi)

nsp_stopa_bezr_y_ht <- get_data_by_variable(varId = 450231, unitLevel = 5) %>%
  select(id, name, y_ht=val) %>%
  mutate(y_ht=y_ht/100)

nsp_stopa_bezr_y_cv <- get_data_by_variable(varId = 450230, unitLevel = 5) %>%
  select(id, name, y_ht_cv=val) %>%
  mutate(y_ht_cv=y_ht_cv/100)

mnoznik <- 7

nsp_stopa_bezr_y <- inner_join(nsp_stopa_bezr_y_ht, nsp_stopa_bezr_y_cv) %>%
  mutate(y_ht_se=(y_ht*y_ht_cv)*mnoznik,
         y_ht_var=y_ht_se^2,
         y_ht_cv2=y_ht_se/y_ht)

# ludność w wieku nieprodukcyjnym na 100 osób w wieku produkcyjnym
x1 <- get_data_by_variable(varId = 60563, unitLevel = 5, year = 2011) %>% 
  select(id, name, x1=val)

# zasięg korzystania ze środowiskowej pomocy społecznej
x2 <- get_data_by_variable(varId = 458700, unitLevel = 5, year = 2011) %>% 
  select(id, name, x2=val)

# połączenie zbiorów danych
nsp_stopa_bezr <- inner_join(nsp_stopa_bezr_y, inner_join(x1, x2)) %>% 
  select(id, name, y_ht, y_ht_se, y_ht_cv=y_ht_cv2, y_ht_var, x1, x2) %>% 
  as.data.frame()

model <- lm(y_ht ~ x1 + x2, data = nsp_stopa_bezr)
summary(model)

model_fh <- fh(fixed = y_ht ~ x1 + x2, vardir = "y_ht_var", combined_data = nsp_stopa_bezr, domains = "id", MSE = TRUE)

nsp_stopa_bezr_fh <- nsp_stopa_bezr %>% 
  mutate(y_fh=model_fh$ind$FH,
         y_fh_se=sqrt(model_fh$MSE$FH),
         y_fh_cv=y_fh_se/y_fh,
         u=model_fh$model$random_effects[,1],
         gamma=model_fh$model$gamma$Gamma)

summary(nsp_stopa_bezr_fh)

plot(nsp_stopa_bezr_fh$y_ht, nsp_stopa_bezr_fh$y_fh)

plot(nsp_stopa_bezr_fh$y_ht_cv, nsp_stopa_bezr_fh$y_fh_cv)

plot(nsp_stopa_bezr_fh$gamma, nsp_stopa_bezr_fh$y_ht_se)
