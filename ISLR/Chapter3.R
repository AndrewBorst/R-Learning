# ISLR Lab 3 Linear regression 
#install.packages('ISLR2')
library(ISLR2)
library(tidyverse)
library(tidymodels)
library(corrr)
library(GGally)
library(gt)

?Boston
glimpse(Boston)
correlate(Boston) %>% 
  focus(medv) %>% 
  filter(abs(medv) > 0.6)    


Boston %>%  
  select(medv, rm, lstat) %>% 
  ggpairs()
