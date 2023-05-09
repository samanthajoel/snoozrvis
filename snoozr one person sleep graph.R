#snoozr Graph 1, May 8 2023

library(tidyverse)
library(ggplot2)
library(scales)

#Graph 1: Person Snapshot
#Visual output of a single person’s data that depicts their sleep/wake cycle across time
#The graph displays days on the X-axis, and hours (noon to noon) on the Y-axis
#Dark depicts sleeping, light depicts wakefulness

####Plotting it with one year data postpartum, wakefulness assumed####

graph1 <- function(data){

  ##Choose a color palette (sleep, wake, missing) based on brand colors
  colors <- c("#2f5170", "#f9f9fb","#80beaf")
  
graph1plot <- ggplot(data, aes(x = grouping_days, y = minutes_noon, color = awake)) +
  # Add points
  geom_point(size = 1.5, alpha=.8) +
  # Format the y-axis to show the time of day
  scale_y_continuous(limits=c(0, 1439), breaks=c(0, 120, 240, 360, 480, 600,720, 840, 960, 1080, 1200, 1320, 1439), 
                     labels=c("12PM", "2PM", "4PM", "6PM", "8PM", "10PM", "12AM", "2AM", "4AM", "6AM", "8AM", "10AM", "11AM")) +
  # Format the x-axis to display in one-month intervals
  scale_x_continuous(breaks = seq(0, max(data$grouping_days), by = 60),
                     labels = function(x) paste0((x/30))) +
  # Add x and y-axis labels
  labs(x = "Months Postpartum", y = "Time of day", color = NULL) +
  # Use the defined color palette
  scale_color_manual(values = colors) +
  # Set the plot theme
  theme_minimal() +
  # Remove gridlines
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "gray80"),
        legend.position = "top",
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "gray90", color = NA))

return(graph1plot)
}


#####trying it out on one-minute binned data for each participant####

TLdata <- read_csv("TL 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

KKdata <- read_csv("KK 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

MHdata <- read_csv("MH 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

#Plot and save as a jpeg for each

TLgraph1 <- graph1(TLdata)
ggsave("TL Graph 1, one year, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

KKgraph1 <- graph1(KKdata)
ggsave("KK Graph 1, one year, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

MHgraph1 <- graph1(MHdata)
ggsave("MH Graph 1, one year, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)


