#snoozr visualization code, May 8 2023

###This code includes functions for transferring 1-minute binned data into a suitable format for graphing.
#the longformat function converts the data to a long format where every row represents a minute
#the noonformat function represents a day as being from noon to noon
#the longstretch function adds columns representing a person's longest sleep stretch in a given day

#load packages
library(tidyverse)
library(ggplot2)
library(lubridate)


#########Converting 1-min bin data to long format for visualization###########

##Produces a dataframe with every minute of every recorded day represented as a row
#The sleep column indicates whether the person was asleep at a given time and day

longformat <- function(bindata){
  #Select only the first year of postpartum data
  bindata <- bindata %>%
    dplyr::filter(between(clean_day,0,365))
  
  ##Create a new factor variable for awake/asleep (observation)
  bindata$awake <- as.factor(bindata$sleepWake)
  
  ##Remove any missing cases
  bindata <- bindata %>% tidyr::drop_na(eventObsEndDateTime)
  
  ###pivot to wide format
  bindata_wide <- bindata %>%
    dplyr::select(time,date,clean_day,awake) %>%
    tidyr::pivot_wider(names_from = time, values_from = awake)
  
  ####Generate an empty dataframe that has one column for every minute of the day
  #Create a sequence of times for every minute of the day
  times <- seq(as.POSIXct("00:00:00", format = "%H:%M:%S"), 
               as.POSIXct("23:59:00", format = "%H:%M:%S"), by = "min")
  
  # Convert the times to character strings in %H:%M:%S format (same as real dataset)
  times_str <- format(times, format = "%HH %MM %SS")
  
  # Create a new data frame with the times as factor columns
  emptydf <- data.frame(matrix(ncol = length(times_str), nrow = 0))
  names(emptydf) <- times_str
  emptydf[] <- lapply(emptydf, factor)
  
  ###Read current data into empty dataframe
  graphdata <- dplyr::full_join(emptydf,bindata_wide)
  
  # reshape the data to long format
  longdata <- graphdata %>%
    tidyr::pivot_longer(
      cols = contains("H"),
      names_to = "time",
      values_to = "sleep",
      names_transform = list(time = ~gsub("H|S", "", .))
    )
  
  return(longdata)
}


##########Converting the format from 00H to 24H to noon to noon#########
#Graph will be more intuitive if nighttime (when people are asleep) is in the middle of the graph
#this function coverts the longdata table into one in which everything is moved forward by 12 hours,
#so that a 24-hour period begins and ends at noon
#"grouping_date" is the current date, moved forward by 12 hours so that the next date begins at noon.
#"grouping_days" is days postpartum, again moved forward by 12 hours so the next day begins at noon
#"minutes_noon" represents the time of day as minutes from noon.


noonformat <- function(longdata){
  # convert time from factor to HMS variable
  longdata$time <- lubridate::hms(longdata$time)
  
  ##Add new phase variable (morning or afternoon) tomorrow's date
  longdata <- longdata %>%
    dplyr::mutate(phase = ifelse(as.numeric(longdata$time) >= 43200, "afternoon", "morning"))
  
  # group the data by date and phase
  grouped_data <- longdata %>% 
    dplyr::group_by(date, phase) %>%
    #add "grouping" variables that consider afternoon data to be part of tomorrow
    #(each date begins at noon and continues to 11:59am the next day)
    dplyr::mutate(grouping_date = ifelse(phase == "morning",date, date + 1)) %>%
    dplyr::mutate(grouping_days = ifelse(phase == "morning",clean_day, clean_day + 1))
  
  # convert new date variable to date format
  grouped_data$grouping_date <- as.Date(grouped_data$grouping_date, origin="1970-01-01")
  
  #clean the time variable so it will convert correctly
  grouped_data$time <- as.character(grouped_data$time)
  grouped_data$time <- ifelse(grepl("M", grouped_data$time),
                              grouped_data$time, paste0("0M ", grouped_data$time))
  grouped_data$time <- ifelse(grepl("H", grouped_data$time),
                              grouped_data$time, paste0("0H ", grouped_data$time))
  
  #covert the time variable into minutes since midnight
  grouped_data$time <- lubridate::hms(grouped_data$time)
  grouped_data$minutes <- lubridate::hour(grouped_data$time) * 60 + 
    lubridate::minute(grouped_data$time)
  
  #Add 720 to each value of "minutes" to get minutes past noon
  grouped_data$minutes_noon <- ifelse(grouped_data$minutes < 720, 
                                      grouped_data$minutes + 720, 
                                      grouped_data$minutes - 720)
  
  #drop unneeded columns
  grouped_data <- subset(grouped_data, select=-c(date,time,phase,minutes))
  
  #Convert sleep to a factor with two levels (assume awake for missing data)
  grouped_data$awake <- factor(if_else(is.na(grouped_data$sleep), "wake", grouped_data$sleep), 
                               levels = c("sleep", "wake"))
  
  
  return(grouped_data)
}


#####Add longest stretch variables######
##This function calculates what a person's longest sleep stretch was on each day
#awake_long is a new sleep/wake column for which periods of wakefulness that are 15 minutes or less are
#recoded as sleep
#sleep_period is a new variable that counts up through the minutes in each sleep period


longstretch <- function(data){
  ##Add "period" variable that considers each switch from sleep or wake to be a new sleep/wake period
  data <- data %>%
    dplyr::group_by(grouping_date) %>%
    dplyr::mutate(period = cumsum(awake != lag(awake, default = first(awake))) + 1)
  
  #Add a "period count" variable that counts the total number of rows in a given period
  data <- data %>%
    dplyr::group_by(grouping_date, period) %>%
    dplyr::mutate(period_count = n()) %>%
    ##Add "long_awake" column that only counts wakefulness if the wake period is 15 minutes or longer
    dplyr::mutate(long_awake = ifelse(awake == "wake" & period_count > 14,"wake", "sleep")) %>%
    ungroup()
  
  ###Add "long period" variable that counts rows in each period based on new awake_long variable
  data <- data %>%
    dplyr::group_by(grouping_date) %>%
    dplyr::mutate(long_period = cumsum(long_awake != lag(long_awake, default = first(long_awake))) + 1) %>%
    ungroup()
  
  ##Add "sleep count" variable that counts the length of each sleep period
  #Awake periods have a sleep count of 0
  data <- data %>%
    dplyr::group_by(grouping_date, long_period) %>%
    dplyr::mutate(sleep_count = ifelse(long_awake == "sleep",n(),NA))%>%
    ungroup()
  
  ##Add new "longest" variable that indicates whether a row
  #belongs to the longest sleep stretch for a given day
  data <- data %>%
    dplyr::group_by(grouping_date) %>%
    dplyr::mutate(longest = case_when(sleep_count == max(sleep_count,na.rm=TRUE) ~ 1,
                               long_awake == "awake" ~0,
                               TRUE ~ 0))
  #drop unneeded columns
  data <- subset(data, select=-c(period,period_count,long_period))
  
 return(data) 
}
  

######Trying it out with existing data######

##reading in 1-minute bin data
TLdata <- read_csv("TL001.OneMinBin.csv")
KKdata <- read_csv("KK002.OneMinBin.csv")
MHdata <- read_csv("MH003.OneMinBin.csv")


##converting to graph format
TLlongdata <- longformat(TLdata)
TLgroupdata <- noonformat(TLlongdata)
TLstretchdata <- longstretch(TLgroupdata)

KKlongdata <- longformat(KKdata)
KKgroupdata <- noonformat(KKlongdata)
KKstretchdata <- longstretch(KKgroupdata)

MHlongdata <- longformat(MHdata)
MHgroupdata <- noonformat(MHlongdata)
MHstretchdata <- longstretch(MHgroupdata)

#write it out
write_csv(TLstretchdata, "TL 1-min bin data cleaned for graphing, May 8 2023.csv")
write_csv(KKstretchdata, "KK 1-min bin data cleaned for graphing, May 8 2023.csv")
write_csv(MHstretchdata, "MH 1-min bin data cleaned for graphing, May 8 2023.csv")



