###data 200 midterm
##formula 1 dataset analysis
##rory gardner

#10.02
#packages
library(ggplot2)
library(magrittr)
library(readr)
library(dplyr)

#f1 data
F1DriversDataset <- read_csv("Downloads/JMU Fall 2024/data 200/F1DriversDataset.csv")
f1 <- F1DriversDataset
View(f1)
colnames(f1)

#cleaning
#change championship years column name to match
colnames(f1) [13] <- "Championship_Years"

#visualization

##distinct winners by decade
#grouping distinct winners by decade
dist_bydec <- f1[f1$Race_Wins > 1,] %>% 
  group_by(Decade) %>% 
  summarize(ndrivers = n_distinct(Driver))
dist_bydec

#plot distinct winners by decade
plot_distbydec <- dist_bydec %>% 
  ggplot(aes(x = Decade, y = ndrivers)) +
  geom_bar(stat = "identity", color = "black", fill = "indianred") +
  labs(x = "Decade", y = "Count of Unique Race Winners", 
       title = "Count of Unique Race Winners by Decade", 
       caption = "Data Source: F1DriversDataset") + 
  scale_x_continuous(breaks = seq(min(dist_bydec$Decade), 
                                  max(dist_bydec$Decade), 
                                  by = 10)) +
  theme(axis.text = element_text(color = "dodgerblue", size = 10), 
        axis.text.x = element_text(face = "italic")) 
plot(plot_distbydec)

#boxplot distinct winners by decade
box_distbydec <- dist_bydec %>% 
  ggplot(aes(y = ndrivers)) + 
  geom_boxplot(fill = "slategray1", color = "skyblue4") + 
  labs(y = "Count of Unique Race Winners", 
       title = "Boxplot of Unique Race Winners per Decade", 
       subtitle = "Count of Unique Race Winners in Each Decade",
       caption = "Datasource: F1DriversDataset") + 
  ylim(0, 25) +
  theme_minimal()
plot(box_distbydec)


#average rates by decade
avg_bydec <- f1 %>% 
  group_by(Decade) %>% 
  summarise(
    avg_win_rate = mean(Win_Rate), 
    avg_pole_rate = mean(Pole_Rate), 
    avg_pod_rate = mean(Podium_Rate)
  )
avg_bydec

#plot average rates by decade
plot_avgbydec <- ggplot(avg_bydec, aes(x = Decade)) + 
  geom_line(aes(y = avg_win_rate, color = "Win Rate"), 
            linewidth = 1, alpha = 0.7) + 
  geom_line(aes(y = avg_pole_rate, color = "Pole Rate"), 
            linewidth = 1, alpha = 0.7) + 
  geom_line(aes(y = avg_pod_rate, color = "Podium Rate"), 
            linewidth = 1, alpha = 0.7) + 
  labs(title = "Average Performance Rates by Decade", 
       x = "Decade", 
       y = "Average Rate", 
       color = "Rate Measured", 
       caption = "Datasource: F1DriversDataset") + 
  scale_color_manual(values = c("Win Rate" = "coral", 
                                "Pole Rate" = "mediumseagreen", 
                                "Podium Rate" = "#DC143C")) +
  scale_x_continuous(breaks = seq(min(avg_bydec$Decade), 
                                  max(avg_bydec$Decade), 
                                  by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.06, by = 0.01), 
                     limits = c(0, 0.06)) +
  theme(axis.text = element_text(color = "dodgerblue", size = 10), 
        axis.text.x = element_text(face = "italic"), 
        legend.key.size = unit(1.5, "cm")) 
plot(plot_avgbydec)

