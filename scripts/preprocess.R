###############################################################################
# preProcess : It reads the given raw input and transforms into an intermediate 
# format.
###############################################################################

## load the required packages 
library(stringr)
library(dplyr)
library(lubridate)

###################### Utility Functions ####################################

## Function Splits the given string using the seperator provided 
## Default seperator is "/"
split_data <- function(timing_String,seperator="/")
{
  return(as.list(str_trim(unlist(strsplit(timing_String,seperator)),side = "both")))
}

## Function splits the weekday amd timings of the restaurant
split_parts <- function(x) {
  
  m<- regexec("((((Mon|Tue|Wed|Thu|Fri|Sat|Sun), )?)(((Mon|Tue|Wed|Thu|Fri|Sat|Sun)-(Mon|Tue|Wed|Thu|Fri|Sat|Sun))?)(,? ?(Mon|Tue|Wed|Thu|Fri|Sat|Sun)?)) ((1?[012]|[0-9]):?([0-5]?[0-9]?) ([aApP][mM])) - ((1?[012]|[0-9]):?([0-5]?[0-9]?) ([aApP][mM]))",x)
  parts <- do.call(rbind,lapply(regmatches(x, m), `[`, c(2L,5L,7L,11L,12L,16L)))
  ##TODO Validation
  colnames(parts) <- c("weekdetails","firstWeek","interval","lastweekday","startTime","endTime")
  parts
}

## appends :00 where ever required. 
preprocessTime <- function(timing_String)
{
   time_part <-as.list(unlist(strsplit(timing_String," ")))
   if(!grepl(":",time_part[1]))
   {
     time_part <- str_c(time_part[1],":00 ",time_part[2])
   }
   else
     time_part <- timing_String
   return(time_part)
}

preprocessing <- function(row)
{
  row$startTime <-preprocessTime(as.character(row$startTime))
  row$endTime <- preprocessTime(as.character(row$endTime))
  return(row)
}

## Function populates the dataframe for the given weekday column with start time and end time
populaterow <- function(weekday,startTime,endTime,dummyData)
{
  indexList <- grep(weekday, colnames(dummyData))
  dummyData[1,indexList[1]] <- startTime
  dummyData[1,indexList[2]] <-endTime 
  return (dummyData)
} 

## initializes the dataframe
initDataFrame <- function()
{
  dummyData <- data.frame(restaurant=character(),MonStart =character(),MonEnd =character(),TueStart =character(),TueEnd =character(),
                          WedStart =character(),WedEnd =character(),ThuStart =character(),ThuEnd =character(),
                          FriStart =character(),FriEnd =character(),SatStart =character(),SatEnd =character(),
                          SunStart =character(),SunEnd =character(),stringsAsFactors = FALSE)
  return(dummyData)
}

## Function generated intermediate data in the form 
## Restaurant name : their opening and closing timing for each day of the week
generateIntermediateData <- function(restaurantDetails)
{
  processedRestaurantDetails <- initDataFrame()
  weekdayList <-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  info <- data.frame(stringsAsFactors = FALSE)
  for(i in seq(1,nrow(restaurantDetails)))
  {
    timings <- split_data(restaurantDetails[i,2])
    for(timing in timings)
    {
      result <-split_parts(timing)
      preprocessTime(result)
      processedData <-cbind(restaurantDetails[i,1],result)
      info <- rbind(info,processedData)
    }
  }
  colnames(info) <- c("Restaurant","weekdetails","firstWeek","interval","lastweekday","startTime","endTime")

  for(i in seq(1:nrow(restaurantDetails)))
  {
    restaurantName <- restaurantDetails[i,1]
    details <-info %>% filter(Restaurant ==restaurantName)
    dummyData <- initDataFrame()
    dummyData[1,1] <-restaurantName
        for(row in seq(1:nrow(details)))
        {
          row <-details[row,]
          row <-preprocessing(row)
          
          startTime <- row$startTime
          endTime <- row$endTime
          
          if(row$firstWeek!="")
            dummyData <-populaterow(row$firstWeek ,startTime,endTime,dummyData)

          if(row$interval!="")
          {
            inter <- unlist(str_split(row$interval,"-"))
            start_interval <-inter[1]
            end_interval <- inter[2]
            start_index <- which(start_interval ==weekdayList)
            end_index <- which(end_interval ==weekdayList)
            intervalList <- weekdayList[seq(start_index,end_index)]
            for(day in seq(1:length(intervalList)))
                dummyData <-populaterow(intervalList[day],startTime,endTime,dummyData)
          }
          if(row$lastweekday!="")
            dummyData <-populaterow(row$lastweekday,startTime,endTime,dummyData)
        }##end of for loop of details 
    processedRestaurantDetails <- bind_rows(processedRestaurantDetails,dummyData)
  }
  return(processedRestaurantDetails)
}

restaurantDetails <- read.csv("restaurant_hours.csv",header = FALSE,stringsAsFactors = FALSE)
colnames(restaurantDetails) <- c("restaurant name","timings")

processedRestaurantDetails <- generateIntermediateData(restaurantDetails)
write.csv(x = processedRestaurantDetails,file = "processed.csv",row.names = FALSE)


