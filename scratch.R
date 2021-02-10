library(dplyr)
library(ggplot2)
library(zoo)
colors = c('#F58C0A','#F0240F','#BF28D7','#ECF20D')
pos_mini_df %>%
  ggplot(aes(x=factor(week), y = positivity_for_week)) + 
  geom_col() +
  labs(title = "Orange County test postivity by week", 
       subtitle = paste("updated: ", date),
       x = "Week",
       y = "Test Positivity",
       fill = "Tier") +
  # tiers c(0,2,5,8,100)
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 2,
           alpha = .4, fill = colors[4])+
  annotate("rect", xmin = 0, xmax = Inf, ymin = 2, ymax = 5,
           alpha = .4, fill = colors[1])+
  annotate("rect", xmin = 0, xmax = Inf, ymin = 5, ymax = 8,
           alpha = .4, fill = colors[2])+
  annotate("rect", xmin = 0, xmax = Inf, ymin = 8, ymax = 25,
           alpha = .4, fill = colors[3])+
  theme_fivethirtyeight() + 
  ylim(c(0,25)) + 
  theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
  theme(legend.position = 'top') 
  

