# Fit oil linear regression model with brlusd and copper as predictors.
# Last Updated: 2/12/2022 

# Libraries and access to Quandl ---------------------------------------------------------
library(Quandl)
library(lubridate)
library(tidyverse)
library(BBmisc)
j <- jsonlite::read_json("GitIgnored_Config/config.json")

key <- j[[1]]

Quandl.api_key(key)

#source(file = "Shared/Common_functions.R")


# Get the data  -----------------------------------------------------------
startdate <-  "2010-09-01"

wti <-  Quandl("CHRIS/CME_CL1", collapse = "week", start_date=startdate)
wti$diff <-  c(wti$Settle[1:(nrow(wti)-1)] - wti$Settle[2:nrow(wti)],0)
wti$group <-  "wti"
#wtim = wtim[2:nrow(wtim),]
wtimonth <-  Quandl("CHRIS/CME_CL1", collapse = "month", start_date=startdate)
wtimonth$diff <-  c(wtimonth$Settle[1:(nrow(wtimonth)-1)] - wtimonth$Settle[2:nrow(wtimonth)],0)
wtiday <-  Quandl("CHRIS/CME_CL1", start_date=startdate)
wtiday$diff <-  c(wtiday$Settle[1:(nrow(wtiday)-1)] - wtiday$Settle[2:nrow(wtiday)],0)

copper  <-  Quandl("CHRIS/CME_HG1", collapse = "week", start_date=startdate)
copper$diff  <-  c(copper$Settle[1:(nrow(copper)-1)] - copper$Settle[2:nrow(copper)],0)
copper$group <-  "copusd"
#copm = copm[2:nrow(copm),]
coppermonth  <-  Quandl("CHRIS/CME_HG1", collapse = "month", start_date=startdate)
coppermonth$diff  <-  c(coppermonth$Settle[1:(nrow(coppermonth)-1)] - coppermonth$Settle[2:nrow(coppermonth)],0)
copperday  <-  Quandl("CHRIS/CME_HG1", start_date=startdate)
copperday$diff  <-  c(copperday$Settle[1:(nrow(copperday)-1)] - copperday$Settle[2:nrow(copperday)],0)

settleweek <- inner_join(wti, copper, by = "Date") %>% 
  select(Date, oil = Settle.x, copper = Settle.y) 
settlemonth <- inner_join(wtimonth, coppermonth, by = "Date") %>% 
  select(Date, oil = Settle.x, copper = Settle.y) 
settleday <- inner_join(wtiday, copperday, by = "Date") %>% 
  select(Date, oil = Settle.x, copper = Settle.y) 

cor(settlemonth$oil, settlemonth$copper)
cor(settleweek$oil, settleweek$copper)
cor(settleday$oil, settleday$copper)

diffweek <- inner_join(wti, copper, by = "Date") %>% 
  select(Date, oil = diff.x, copper = diff.y) 
diffmonth <- inner_join(wtimonth, coppermonth, by = "Date") %>% 
  select(Date, oil = diff.x, copper = diff.y) 
diffday <- inner_join(wtiday, copperday, by = "Date") %>% 
  select(Date, oil = diff.x, copper = diff.y) 

cor(diffday$oil, diffday$copper)
cor(diffweek$oil, diffweek$copper)
cor(diffmonth$oil, diffmonth$copper)

#Plot the data to look for multivariate outliers, non-linear relationships etc
diffmonth %>% ggplot(aes(copper,oil)) +
    geom_point() +
    geom_jitter() +
    geom_smooth() +
    facet_wrap(~year(Date))

mnorm  <-  dday %>%  
  mutate(oilnorm = normalize(oil), coppernorm = normalize(copper)) 

mnorm %>% 
  ggplot(aes(Date, oilnorm)) +
  geom_line() + 
  geom_point() +
  geom_line(data = mnorm, aes(Date,coppernorm), color = "red") +
  geom_point(data = mnorm, aes(Date,coppernorm), color = "red") 
# +
#   facet_wrap(~year(Date))
  
#fit oil model with copper as predictors (aka x-variable, explanatory variables)
oilmod = lm(oil ~ copper, data = m)
summary(oilmod)
#r^2 is 0.71 so around 70% of variation in the price of oil can be accounted for by brlusd and copper
#F-stat tests whether slope is zero 
oilpred = predict(oilmod)
gdata = melt(data.frame(data1$oil, wti$Date, oilpred),id="wti.Date")

ggplot(data=gdata,
       aes(x=wti.Date, y=value, colour=variable)) +
  geom_line(aes(linetype=variable)) +  
  geom_point()
# theme(panel.grid.minor = element_line(colour="blue", size=0.5)) + 
# scale_x_continuous(minor_breaks = seq(1, 10, 0.5))
# 
oilres = resid(oilmod)
plot(m$oil, oilres)
abline(0, 0)                  # the horizon
oilrst = rstandard(oilmod)
plot(m$oil,oilrst)
qqnorm(oilrst)
# 
# cor(data1$copper, data1$brlusd, method="pearson")
confint(oilmod, conf.level=0.95)
