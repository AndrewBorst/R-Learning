# A thought that diesel demand may be a leading indicator. 
# 
# Just glancing at the data between a retirement fund and diesel demand 
# leads me to rethink that notion and it is more likely that stocks lead
# demand. Further analysis would need to adjust diesel demand seasonally, 
# perhaps just lagging the numbers by one year after aggregating to the month. 

library(readr)
library(tidyverse)
library(lubridate)
library(quantmod)
library(GGally)

# getQuote("RFFTX")
# getSymbols("RFFTX")
# barChart(RFFTX)

X4_Week_Avg_U_S_Product_Supplied_of_Distillate_Fuel_Oil <- read_csv("Public_Datasets/4-Week_Avg_U.S._Product_Supplied_of_Distillate_Fuel_Oil.csv", 
                                                                    skip = 4)
RFFTX <- read_csv("Public_Datasets/RFFTX.csv")

head(X4_Week_Avg_U_S_Product_Supplied_of_Distillate_Fuel_Oil)
head(RFFTX)

diesel <- X4_Week_Avg_U_S_Product_Supplied_of_Distillate_Fuel_Oil %>% 
  select(Date = `Week of`, DieselSupplied = `4-Week Avg U.S. Product Supplied of Distillate Fuel Oil Thousand Barrels per Day`) 

diesel <- diesel %>% 
  mutate(Date = as.Date(parse_date_time(Date, orders = "mdy")))

fund  <-  RFFTX %>% 
    mutate(Date = Date + 4) %>% 
    select(Date, Close)

j <- diesel %>% 
  inner_join(fund) %>% 
  arrange(Date) %>% 
  mutate(dieselDiff = 100 * (DieselSupplied -first(DieselSupplied))/first(DieselSupplied)) %>% 
  mutate(fundDiff = 100 * (Close - first(Close))/ first(Close))
# mutate(dieselDiff = c(0,diff(j$DieselSupplied))) %>% 
#   mutate(fundDiff = c(0, diff(j$Close)))

j %>% filter(year(Date) %in% c(2018, 2019, 2020, 2021,2022)) %>% 
  ggplot(aes(Date)) +
  geom_line(aes(y=dieselDiff)) +
  geom_line(aes(y=fundDiff), colour = "blue") 

