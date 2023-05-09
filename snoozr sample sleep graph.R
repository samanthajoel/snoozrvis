#snoozr Graph 3 code, May 8 2023

library(tidyverse)
library(ggplot2)
library(scales)

#Graph 3: Longest Stretch (Person-Level)
#Visual output of a single person’s longest consolidated sleep stretch each day
#A sleep stretch will be considered to be concluded when the person is awake
#for longer than 15 minutes

###Heat graph version shows the times of day when the longest sleep stretches vs. other
#sleep periods occurred

graph3heat <- function(data){

##Convert longest sleep stretch to a factor: Longest sleep stretch, sleeping, and awake
data$longfactor <- factor(if_else(data$longest==1, "long", data$long_awake), 
                           levels = c("long", "sleep", "wake"))

##Choose a color palette (longest stretch, sleep, wake)
#dark blue, greenblue, grey blue (Note, could replace greenblue with orange - #ee9c6d)
colors <- c("#2f5170", "#80beaf", "#f9f9fb")

####Plot it in the original format (time of day retained)###
heatplot <- ggplot(data, aes(x = grouping_days, y = minutes_noon, color = longfactor)) +
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
  # Use the defined color palette and relabel the categories
  scale_color_manual(values = colors, labels = c("Longest Sleep Stretch", "Shorter Sleep Periods", "Awake")) +
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


###Stacked version plots longest sleep stretch vs. other sleep periods in terms of
#total amount of minutes per day

graph3stacked <- function(data){

##Convert longest sleep stretch to a factor: Longest sleep stretch, sleeping, and awake
  data$longfactor <- factor(if_else(data$longest==1, "long", data$long_awake), 
                            levels = c("long", "sleep", "wake"))

  
##Choose a color palette (longest stretch, sleep, wake)
#dark blue, greenblue, grey blue (Note, could replace greenblue with orange - #ee9c6d)
  colors <- c("#2f5170", "#80beaf", "#f9f9fb")
  
##Creating a tidy summary of the data
summarydata <- data %>%
  group_by(grouping_days, longfactor) %>%
  summarize(n=n())

summarydata$n <- as.numeric(summarydata$n)

# Create a stacked bar chart
stackedplot <- ggplot(summarydata, aes(x = grouping_days, y = n, fill = longfactor)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # Format the y-axis to show the time of day
  scale_y_continuous(limits=c(0, 1440), breaks=c(0, 120, 240, 360, 480, 600,720, 840, 960, 1080, 1200, 1320, 1440), 
                     labels=c("0", "2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22", "24")) +
  # Format the x-axis to display in one-month intervals
  scale_x_continuous(breaks = seq(0, max(summarydata$grouping_days), by = 60),
                     labels = function(x) paste0((x/30))) +
  # Add x and y-axis labels
  labs(x = "Months Postpartum", y = "Hours per Day", fill = NULL) +
  # Use the defined color palette and relabel the categories
  scale_fill_manual(values = colors, labels = c("Longest Sleep Stretch", "Shorter Sleep Periods", "Awake")) +
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


return(stackedplot)

}

#
#####trying it out on one-minute binned data for each participant####

TLdata <- read_csv("TL 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

KKdata <- read_csv("KK 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

MHdata <- read_csv("MH 1-min bin data cleaned for graphing, May 8 2023.csv") %>%
  select(-1)

#Plot and save as a jpeg for each

TLgraph3heat <- graph3heat(TLdata)
ggsave("TL Graph 3, heat version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

TLgraph3stacked <- graph3stacked(TLdata)
ggsave("TL Graph 3, stacked version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)


KKgraph3heat <- graph3heat(KKdata)
ggsave("KK Graph 3, heat version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

KKgraph3stacked <- graph3stacked(KKdata)
ggsave("KK Graph 3, stacked version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)


MHgraph3heat <- graph3heat(MHdata)
ggsave("MH Graph 3, heat version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

MHgraph3stacked <- graph3stacked(MHdata)
ggsave("MH Graph 3, stacked version, May 8 2023.jpg", width = 10, height = 8, units = "in", dpi = 300)

