if(!(rlang::is_installed('ISLR2')))
{
  install.packages('ISLR2')
}
library(ISLR2)
library(tidyverse)


# Figure 1.1 --------------------------------------------------------------
Wage %>% ggplot(aes(x=age, y=wage)) +
  geom_point(colour="grey") +
  geom_smooth(se=FALSE) +
  # geom_smooth(method = "gam", color="red", fill="red") +
  # geom_smooth(method = "loess", color="yellow", fill="yellow") +
  # geom_smooth(method = "lm", color="blue", fill="blue") +
  # geom_smooth(method = "glm", color="green", fill="green") +
  labs(x = "Age", y = "Wage")

Wage %>% ggplot(aes(x=year, y=wage)) +
  geom_point(colour="grey") +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(breaks = c(2003,2004,2005,2006,2007,2008,2009),
                     labels = c("2003"="2003", "2004"="", "2005"="","2006"="2006",
                                "2007" ="", "2008"="", "2009"="2009")) +
  labs(x = "Year", y = "Wage") +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black",
    size = 1))

Wage %>% ggplot(aes(x=education, y=wage, fill=education)) +
  stat_boxplot(geom = "errorbar", width=0.3) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels=c(1, 2, 3, 4, 5)) +
  scale_y_continuous(breaks=c(50,100,200, 300)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "azure4"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")) + 
  labs(x="Education Level", y="Wage") 
 
 