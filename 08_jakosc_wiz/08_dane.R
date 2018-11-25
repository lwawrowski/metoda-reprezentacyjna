library(tidyverse)
library(survey)
library(lodown)

# http://asdfree.com/general-social-survey-gss.html

lodown( "gss" , output_dir = "08_jakosc_wiz")

gss <- readRDS("08_jakosc_wiz/2016.rds")

# gss_c <- readRDS("08_jakosc_wiz/gss 1972 2016 cross sectional cumulative data release 4 august 16 2018.rds")

gss_df <- gss %>%
  select(vpsu, vstrat, polviews, born, adults, hompop, race, region, age, sex, one, wtssall) %>%
  mutate(polviews = 
           factor( polviews ,
                   labels = c( "Extremely liberal" , "Liberal" ,
                               "Slightly liberal" , "Moderate, middle of the road" ,
                               "Slightly conservative" , "Conservative" ,
                               "Extremely conservative" )
           ) ,
         
         born_in_usa = ifelse( born %in% 1:2 , as.numeric( born == 1 ) , NA ) ,
         
         adults_in_hh = ifelse( adults > 8 , NA , adults ) ,
         
         persons_in_hh = ifelse( hompop > 11 , NA , hompop ) ,
         
         race = factor( race , labels = c( "white" , "black" , "other" ) ) ,
         
         region = 
           factor( region , 
                   labels = c( "New England" , "Middle Atlantic" ,
                               "East North Central" , "West North Central" ,
                               "South Atlantic" , "East South Central" ,
                               "West South Central" , "Mountain" , "Pacific" )
           ))

gss_design <- 
  svydesign( 
    ~ vpsu , 
    strata = ~ vstrat , 
    data = gss_df , 
    weights = ~ wtssall , 
    nest = TRUE 
  )  

summary(weights(gss_design))

svyby( ~ one , ~ region , gss_design , svytotal )