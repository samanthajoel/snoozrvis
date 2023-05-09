
######Merge and recode data from several participants######

##packages
library(tidyverse)

##read in cleaned 1-minute files
TLdata <- read_csv("TL 1-min bin data cleaned for graphing, May 8 2023.csv")
KKdata <- read_csv("KK 1-min bin data cleaned for graphing, May 8 2023.csv")
MHdata <- read_csv("MH 1-min bin data cleaned for graphing, May 8 2023.csv")


#Add a new, unique ID column to each based on its time and day,
#And add a suffix to all the rest of the columns
TLdata <- TLdata %>%
  mutate(graphID = paste0(grouping_days, "_", minutes_noon)) %>%
  rename_at(vars(-graphID), ~paste0(., "_TL"))

KKdata <- KKdata %>%
  mutate(graphID = paste0(grouping_days, "_", minutes_noon)) %>%
  rename_at(vars(-graphID), ~paste0(., "_KK"))

MHdata <- MHdata %>%
  mutate(graphID = paste0(grouping_days, "_", minutes_noon)) %>%
  rename_at(vars(-graphID), ~paste0(., "_MH"))

##merge into a single file using the graphID as a linking column
mergedata <- full_join(TLdata, KKdata, by = "graphID") %>%
  full_join(MHdata, by = "graphID") %>%
  #add two new variables capturing date and time for everyone
  separate(col = graphID, into = c("date_overall", "time_overall"), sep = "_") %>%
  #make new variables numeric
  mutate(time_overall = as.numeric(time_overall)) %>%
  mutate(date_overall = as.numeric(date_overall)) %>%
  #recreate graphID and move it along with the new variables to the front of the dataframe
  mutate(graphID = paste(date_overall, time_overall, sep = "_")) %>%
  select(graphID, date_overall,time_overall,everything())


####adding new graphing variables

mergedata <- mergedata %>%
  #calculate percentage of people asleep at each minute
  mutate(percent_asleep = rowMeans(select(., starts_with("awake_")) %>% 
                                     mutate(across(everything(), ~case_when(. == "sleep" ~ 1, . == "wake" ~ 0))), na.rm = TRUE) * 100) %>%
  mutate(percent_longest = rowMeans(select(., starts_with("longest_")) %>% 
                                     mutate(across(everything(), ~case_when(. == 1 ~ 1, . == 0 ~ 0))), na.rm = TRUE) * 100)

#add some text

write.csv(mergedata, "Merged participant data for graphing, May 2023.csv")
