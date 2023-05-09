#snoozr Graph 4, May 8 2023

library(tidyverse)
library(ggplot2)
library(scales)

#Graph 4: Longest Stretch (Heat Map)
#Longest consolidated sleep stretch each day plotted across all participants
#Shade indicates the percentage of participants who were experiencing their
#longest sleep stretch at the specific time indicated
#dark = 100% of participants were in their longest sleep stretch
#light = 0% of participants were in their longest sleep stretch

  
###Heat graph version shows the times of day when the longest sleep stretches vs. other
#sleep periods occurred

graph4heat <- function(data){
  
  ##Choose a color palette (longest stretch from 0% to 100%) based on brand colors
  #colors <- c("#f9f9fb", "#b3ddd0", "#2f5170")
  colors <- c("#f9f9fb", "#ee9c6d", "#2f5170")
  
  ####Plot it in the original format (time of day retained)###

heatplot <- ggplot(data, aes(x = date_overall, y = time_overall, color = percent_longest)) +
    # Add points
    geom_point(size = 1.5, alpha=.8) +
    # Format the y-axis to show the time of day
    scale_y_continuous(limits=c(0, 1439), breaks=c(0, 120, 240, 360, 480, 600,720, 840, 960, 1080, 1200, 1320, 1439), 
                       labels=c("12PM", "2PM", "4PM", "6PM", "8PM", "10PM", "12AM", "2AM", "4AM", "6AM", "8AM", "10AM", "11AM")) +
    # Format the x-axis to display in one-month intervals
    scale_x_continuous(breaks = seq(0, max(data$date_overall), by = 60),
                       labels = function(x) paste0((x/30))) +
    # Add x and y-axis labels
    labs(x = "Months Postpartum", y = "Time of day", color = "Percentage in Longest Sleep Stretch") +
  # Use a gradient color scale
  scale_color_gradientn(colors=colors) +
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
  
  return(heatplot)

}


#Try it out on merged data, save a jpeg

mergedata <- read_csv("Merged participant data for graphing, May 2023.csv")


graph4_3ps_heat <- graph4heat(mergedata)
ggsave("Graph 4 heat, three participants, with orange, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

