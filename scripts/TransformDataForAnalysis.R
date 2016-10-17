###############################################################################
# Analysis  : It reads the intermediate data generated from previous 
# step and and transform it into required data format for analysis.
# Also generated derived fields like totalDurationOpenWeekly , 
# totalDurationOpenWeekends daywiseDuration etc 
###############################################################################

## @knitr TransformDataForAnalysis

library(lubridate)
library(stringr)

## Converts period into hours.
convertDuration <- function(diff)
{
  hours(hour(diff)+ floor(minute(diff)/60)) + minutes(minute(diff)%%60)
}

deriveRelevantInformation <- function(data)
{
  for(index in seq(1:nrow(data)))
  {
    rowData <-deriveRelevantInformationPerRow(data[index,])
    processedRestaurant <- rbind(processedRestaurant,rowData)
  }
  return(processedRestaurant[-1,])
}

deriveRelevantInformationPerRow <- function(row)
{
  dummyData <- data.frame(MonDiff =period(1),TueDiff=period(1),WedDiff=period(1),ThuDiff=period(1),FriDiff=period(1),SatDiff=period(1),SunDiff=period(1),TotalDiff=period(1),WeekendDiff=period(1),WeekdayDiff=period(1))
  dummyData$MonDiff <-durationDifference(row$MonStart,row$MonEnd)
  dummyData$TueDiff <- durationDifference(row$TueStart,row$TueEnd)
  dummyData$WedDiff <- durationDifference(row$WedStart,row$WedEnd)
  dummyData$ThuDiff <- durationDifference(row$ThuStart,row$ThuEnd)
  dummyData$FriDiff <- durationDifference(row$FriStart,row$FriEnd)
  dummyData$SatDiff <- durationDifference(row$SatStart,row$SatEnd)
  dummyData$SunDiff <- durationDifference(row$SunStart,row$SunEnd)
  
  dummyData$TotalDiff <- with(dummyData,convertDuration(MonDiff+TueDiff+WedDiff+ThuDiff+FriDiff+SatDiff+SunDiff))
  dummyData$WeekendDiff <- with(dummyData,convertDuration(SatDiff+SunDiff))
  dummyData$WeekdayDiff <- with(dummyData,convertDuration(TotalDiff -WeekendDiff))
  return(dummyData)
}

## finds the difference between given startTime and endTime
## returns period
durationDifference <- function(startTime,endTime)
{
  start <- startTime
  end <- endTime
  if(is.na(start) & is.na(end))
    return(period(0))
  
  dt <-Sys.time()
  dateStr <-format(dt,"%Y-%m-%d")
  dformatstart <-str_c(dateStr," ",start)
  dformatend <-str_c(dateStr," ",end)
  startDate <-strptime(dformatstart,format = "%Y-%m-%d %I:%M %p")
  endDate <- strptime(dformatend,format = "%Y-%m-%d %I:%M %p")
  if(startDate >endDate)
  {
    nextday <-format(dt + ddays(1),"%Y-%m-%d")
    dformatstart <-str_c(dateStr," ",start)
    dformatend <-str_c(ymd(nextday)," ",end)
    start <-strptime(dformatstart,format = "%Y-%m-%d %I:%M %p")
    end <- strptime(dformatend,format = "%Y-%m-%d %I:%M %p")
    diff <-as.period(interval(start,end))

  }
  else
    diff <-as.period(interval(startDate,endDate))
  return(diff)
}

processedRestaurantDetails <-read.csv("processed.csv",header = TRUE,stringsAsFactors = FALSE)
processedRestaurant <- data.frame(MonDiff =period(1),TueDiff=period(1),WedDiff=period(1),ThuDiff=period(1),FriDiff=period(1),SatDiff=period(1),SunDiff=period(1),TotalDiff=period(1),WeekendDiff=period(1),WeekdayDiff=period(1))
finaldata <-deriveRelevantInformation(processedRestaurantDetails)
finaldata<-with(processedRestaurantDetails,cbind(restaurant,finaldata))


finaldata$MonDiffInHrs <- time_length(finaldata$MonDiff,unit="hours")
finaldata$TueDiffInHrs <- time_length(finaldata$TueDiff,unit="hours")
finaldata$WedDiffInHrs <- time_length(finaldata$WedDiff,unit="hours")
finaldata$ThuDiffInHrs <- time_length(finaldata$ThuDiff,unit="hours")
finaldata$FriDiffInHrs <- time_length(finaldata$FriDiff,unit="hours")
finaldata$SatDiffInHrs <- time_length(finaldata$SatDiff,unit="hours")
finaldata$SunDiffInHrs <- time_length(finaldata$SunDiff,unit="hours")

finaldata$TotalDiffInHrs <- time_length(finaldata$TotalDiff,unit="hours")
finaldata$WeekendDiffInHrs <- time_length(finaldata$WeekendDiff,unit="hours")
finaldata$WeekdayDiffInHrs <- time_length(finaldata$WeekdayDiff,unit="hours")
write.csv(x=finaldata,file = "TransformedOutput.csv",row.names = FALSE)



