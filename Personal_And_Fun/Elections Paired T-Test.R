# Reading through a blog post on creating a t-test in Power BI, I was interested 
# in recreating the test in R. The data used is quite different, here we are 
# doing the test on Indiana county 2016 and 2020 results for the President. 
# After some reflection, every election is different so doing a simple test
# between two election years.  

# https://datakuity.com/2022/02/07/paired-t-test-in-power-bi-using-dax/

library(readr)
library(tidyverse)
countypres_2000_2020 <- read_csv("Public_Datasets/countypres_2000-2020.csv")

county_percent <- countypres_2000_2020 %>% 
  filter(year == 2016 | year == 2020) %>% 
  filter(party == 'DEMOCRAT' | party == 'REPUBLICAN') %>% 
  filter(state == 'INDIANA') %>% 
  group_by(county_name, year, party) %>% 
  summarise(county_percent = candidatevotes / totalvotes) %>% 
  collect()
      
gain_loss_percent <-  county_percent %>% 
  pivot_wider(names_from = party, values_from = county_percent) %>% 
  pivot_wider(names_from = year, values_from = c(DEMOCRAT, REPUBLICAN)) %>% 
  mutate(democrat_percent_difference = round((DEMOCRAT_2020 - DEMOCRAT_2016),3) * 100,
         republican_percent_difference = round((REPUBLICAN_2020 - REPUBLICAN_2016),3) * 100) %>% 
  mutate(republican_gain_loss = (republican_percent_difference - democrat_percent_difference))

county_percent %>% filter(party == 'DEMOCRAT') %>% 
  ggplot(aes(x = year, y = round(county_percent, 3)* 100, group = year)) +
  geom_boxplot(fill = 'blue', outlier.shape = NA) +
  coord_cartesian(ylim=c(15, 40)) 


t.test(gain_loss_percent$DEMOCRAT_2016, gain_loss_percent$DEMOCRAT_2020, paired = TRUE, alternative =  "two.sided")
t.test(gain_loss_percent$REPUBLICAN_2016, gain_loss_percent$REPUBLICAN_2020, paired = TRUE, alternative =  "two.sided")


