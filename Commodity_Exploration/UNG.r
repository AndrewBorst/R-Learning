#Builds a matrix of lagged differences for oil copper corn brlusd and bdi
library(Quandl)
library(tidyverse)
library(lubridate)


Quandl.api_key("nfJf-LHSL3gtCpXmyuX2")

startdate='2020-01-01'

# Contract Month	NG
# Feb 2020 (G)	1/29/2020
# Mar 2020 (H)	2/26/2020
# Apr 2020 (J)	3/27/2020
# May 2020 (K)	4/28/2020
# Jun 2020 (M)	5/27/2020
# Jul 2020 (N)	6/26/2020
# Aug 2020 (Q)	7/29/2020
# Sep 2020 (U)	8/27/2020
# Oct 2020 (V)	9/28/2020
# Nov 2020 (X)	10/28/2020
# Dec 2020 (Z)	11/25/2020
# Jan 2021 (F)	12/29/2020
# Feb 2021 (G)	1/27/2021
# Mar 2021 (H)	2/24/2021
# Apr 2021 (J)	3/29/2021
# May 2021 (K)	4/28/2021
# Jun 2021 (M)	5/26/2021
# Jul 2021 (N)	6/28/2021
# Aug 2021 (Q)	7/28/2021
# Sep 2021 (U)	8/27/2021
# Oct 2021 (V)	9/28/2021
# Nov 2021 (X)	10/27/2021
# Dec 2021 (Z)	11/26/2021

UNG <- read_csv("ExplorationAndModels/UNG.csv")
UNG$Date <- as.Date(UNG$Date)
UNG$ungd1 <- c(0,diff(UNG$`Adj Close`))
v <- (UNG$`Adj Close` - lag(UNG$`Adj Close`)) / lag(UNG$`Adj Close`)
UNG$ungChange <- round(v, 3)

ng = as_tibble(Quandl("CHRIS/CME_NG1", collapse = "daily", start_date=startdate, order='asc'))
ng$ngd1 <- round(c(0,diff(ng$Settle)), 3)
v <- (ng$Settle - lag(ng$Settle)) / lag(ng$Settle) 
ng$ngChange <- round(v, 3)

ng2 = as_tibble(Quandl("CHRIS/CME_NG2", collapse = "daily", start_date=startdate, order='asc'))
ng2$ngd1 <- c(0,diff(ng2$Settle))
v <- (ng2$Settle - lag(ng2$Settle)) / lag(ng2$Settle) 
ng2$ng2Change <- round(v, 3)

View(UNG %>% filter(Date > '2020-12-31')  %>% 
  select(Date, ungChange) %>% 
  inner_join(select(ng, Date, ngChange), by="Date") %>% 
  inner_join(select(ng2, Date, ng2Change), by="Date")
)

d <- UNG %>% filter(Date > '2020-02-01' & day(Date) < 22)  %>% 
  select(Date, ungChange) %>% 
  inner_join(select(ng, Date, ngChange), by="Date") %>% 
  inner_join(select(ng2, Date, ng2Change), by="Date")

d$dayName <- weekdays(d$Date)
d$monthNo <- month(d$Date)

dmonth <- d %>% 
  group_by(monthNo) %>% 
  summarise(ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

ddayName <- d %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

# > filter(w, weekNo < 5) %>% summarise(sum(week1Diff), sum(week2Diff))
# sum(week1Diff)` `sum(week2Diff)`
# 0.017            0.013s

# rolling NG contract at EOM produces strong movement in UNG 

x1 <- d %>% filter(day(Date) <= 10)
x11 <- d %>% filter(day(Date) < 22 & day(Date) > 10)

x1dayName <- x1 %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

x11dayName <- x11 %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

x21 <- UNG %>% filter(day(Date) >= 21) %>% 
  select(Date, Close, ungChange) %>% 
  inner_join(select(ng, Date, Settle, ngChange), by="Date") %>%
  inner_join(select(ng2, Date, Settle, ng2Change), by="Date")

colSums(select(d, -(c(Date))), na.rm = 'true')
colSums(select(x1, -(c(Date, weekNo))), na.rm = 'true')
colSums(select(x11, -(c(Date, weekNo))), na.rm = 'true')

View(filter(x, day(Date) %in% c(25, 26, 27,28,29,30,31,1,2)) %>% mutate(Day1 = weekdays(Date)))
     
plot(d$ungChange, d$ngChange)

write.csv(d, file='R-Projects/ExplorationAndModels/ung_output.csv')
