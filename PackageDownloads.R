library(cranlogs)
library(tidyverse)
x <- cran_downloads(package = c("data.table", "dplyr", "shiny", "rmarkdown","Hmisc", "caret"), from = "2015-01-01", to = "2021-12-31")

nMonths <- x %>% 
  group_by(month = floor_date(date, "month"), package) %>%
  summarise(monthlySum = sum(count)) %>% 
  collect()

tail(nMonths)

nMonths %>% 
  ggplot(aes(month, as.integer(monthlySum), color = package)) +
  geom_line() + 
  xlab("") +
  ylab("") +
  ggtitle("Package Downloads (Monthly)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


