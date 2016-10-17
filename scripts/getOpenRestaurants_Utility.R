library(lubridate)
library(dplyr)
library(stringr)

populate24HourTime <- function(inputTime)
{
  if(is.na(inputTime))
    return (0)
  time24hr <-format(strptime(inputTime, "%I:%M %p"), format="%H:%M")
  tempTime <-strptime(time24hr,"%H:%M")
  if(hour(tempTime)==0)
    time <-as.numeric(str_c("0.",minute(tempTime)))
  else 
  {
    dummyDate <-as.POSIXct(str_c(format(now(),"%Y-%m-%d")," ", as.character(time24hr)))
    time <- as.numeric(dummyDate - trunc(dummyDate, "days"))
  }
  return (time)
}

## Function returns the list of closed restaurant.
get_close_restaurants <- function(colStart,colEnd,data)
{
  openRestaurant <-data[data[ , grep(colStart, colnames(data) )] ==0,c("restaurant")]
  closeRestaurant <-data[data[ , grep(colEnd, colnames(data) )] ==0,c("restaurant")]
  closedRestaurants <- intersect(openRestaurant,closeRestaurant)
  return(closedRestaurants)
}

## Function returns the list of restaurants open past mid might
get_midNight_open_restaurants <- function(colStart,colEnd,data,time)
{
  midNight_open_restaurants <-data[data[ , grep(colStart, colnames(data) )] > data[ , grep(colEnd, colnames(data) )],c("restaurant",colStart,colEnd)] 
  midNight_open <- midNight_open_restaurants[midNight_open_restaurants[ , grep(colStart, colnames(midNight_open_restaurants) )] <=time | midNight_open_restaurants[ , grep(colEnd, colnames(midNight_open_restaurants) )] >=time,c("restaurant")]
  return(midNight_open)
}

## Function translate timestamp to numeric time
## Need to check a better way to do it 
populateData<- function(row)
{
  MonStart <-populate24HourTime(row$MonStart)
  MonEnd <-populate24HourTime(row$MonEnd)
  TueStart <-populate24HourTime(row$TueStart)
  TueEnd <-populate24HourTime(row$TueEnd)
  WedStart <-populate24HourTime(row$WedStart)
  WedEnd <-populate24HourTime(row$WedEnd)
  ThuStart <-populate24HourTime(row$ThuStart)
  ThuEnd <-populate24HourTime(row$ThuEnd)
  FriStart <-populate24HourTime(row$FriStart)
  FriEnd <-populate24HourTime(row$FriEnd)
  SatStart <-populate24HourTime(row$SatStart)
  SatEnd <-populate24HourTime(row$SatEnd)
  SunStart <-populate24HourTime(row$SunStart)
  SunEnd <-populate24HourTime(row$SunEnd)
  output <-cbind(row$restaurant,MonStart,MonEnd,TueStart,TueEnd,WedStart,WedEnd,ThuStart,ThuEnd,FriStart,FriEnd,SatStart,SatEnd,SunStart,SunEnd)
  return(output)
}

preprocess <- function()
{
  intermediateData <-read.csv("processed.csv",header = TRUE,stringsAsFactors = FALSE)
  data <- data.frame(stringsAsFactors = FALSE)
  
  for(i in seq(1,nrow(intermediateData)))
  {
    row <- intermediateData[i,]
    transformedRow <-populateData(row)
    data <- rbind(data,transformedRow)
  }
  colnames(data) <- colnames(intermediateData)
  weekdayList <-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  
  for(colname in weekdayList)
  {
    indexList <- grep(colname, colnames(data))
    data[,indexList[1]] <- as.numeric(as.character(data[,indexList[1]]))
    data[,indexList[2]] <-as.numeric(as.character(data[,indexList[2]]))
  }
  return(data)
}

get_open_restaurants <- function(input,data)
{
  input_date <- strptime(input, "%Y-%m-%d %I:%M %p")
  time <- format(input_date,format="%I:%M %p")
  time <- populate24HourTime(time)
  date <- format(input_date,format="%Y-%m-%d")
  weekDay <- wday(date, label=TRUE)
  weekDay <-substr(weekDay,1,3)
  colStart <- str_c(weekDay,"Start")
  colEnd <- str_c(weekDay,"End")
  
  ## find closed Restaurant 
  closedRestaurants <- get_close_restaurants(colStart,colEnd,data)
  
  ## find midnight open restaurants 
  midNight_open_restaurants <- get_midNight_open_restaurants(colStart,colEnd,data,time)
  
  openRestaurant <-data[data[ , grep(colStart, colnames(data) )] <=time,c("restaurant")]
  closeRestaurant <-data[data[ , grep(colEnd, colnames(data) )] >=time,c("restaurant")]
  
  restaurantList <-intersect(openRestaurant,closeRestaurant)
  restaurantList <-setdiff(restaurantList,closedRestaurants)
  restaurantList <- union(restaurantList,midNight_open_restaurants)
  return(restaurantList)
}