## indentifying restaurants who server breakfast, lunch, Dinner or 
## any of there combination.
## @knitr BreakfastLunchDinnerOptions
library(dplyr)
library(stringr)
breakfastStartTime <-" 7:00 am"
breakfastEndTime <- " 10:30 am"
lunchStartTime <- " 12:00 pm"
lunchEndTime <- " 3:00 pm"
dinnerStartTime <- " 7:00 pm"
dinnerEndTime <- " 11:00 pm"
options <- c("Breakfast","Lunch","Dinner","BreakFast&Lunch","Breakfast&Dinner","Lunch&Dinner","ALL","Closed")
source("getOpenRestaurants_Utility.R")
data <- preprocess()
df <- with(data,data.frame(restaurant))


populateOptions <- function(input_date,df)
{
 date <- format(input_date,format="%Y-%m-%d")
 weekDay <- wday(date, label=TRUE)
 weekDay <-substr(weekDay,1,3)
 
 colName <- str_c(weekDay,"Options")
 
 breakfastStartlist <- get_open_restaurants(str_c(input_date,breakfastStartTime),data)
 breakfastEndlist <- get_open_restaurants(str_c(input_date,breakfastEndTime),data)
 breakfast <- union(breakfastStartlist,breakfastEndlist)

 lunchStartlist <- get_open_restaurants(str_c(input_date,lunchStartTime),data)
 lunchEndlist <-get_open_restaurants(str_c(input_date,lunchEndTime),data)
 lunch<- union(lunchStartlist,lunchEndlist)

 dinnerStartList <-get_open_restaurants(str_c(input_date,dinnerStartTime),data)
 dinnerEndList <- get_open_restaurants(str_c(input_date,dinnerEndTime),data)
 dinner <- union(dinnerStartList,dinnerEndList)

 all <- intersect(intersect(breakfast,lunch),dinner)

 breakfastLunch <- setdiff(intersect(breakfast,lunch),dinner)
 breakfastDinner <-setdiff(intersect(breakfast,dinner),lunch)
 lunchDinner <- setdiff(intersect(lunch,dinner),breakfast)

 df[which(df$restaurant %in% c(breakfast)),c(colName)] <- options[1]
 df[which(df$restaurant %in% c(lunch)),c(colName)] <- options[2]
 df[which(df$restaurant %in% c(dinner)),c(colName)] <- options[3]

 df[which(df$restaurant %in% c(breakfastLunch)),c(colName)] <- options[4]
 df[which(df$restaurant %in% c(breakfastDinner)),c(colName)] <- options[5]

 df[which(df$restaurant %in% c(lunchDinner)),c(colName)] <- options[6]
 df[which(df$restaurant %in% c(all)),c(colName)] <- options[7]
 df[which(is.na(df[c(colName)])),c(colName)]<- options[8]
 return(df)
}

df <- populateOptions("2016-8-15",df)
df <- populateOptions("2016-8-16",df)
df <- populateOptions("2016-8-17",df)
df <- populateOptions("2016-8-18",df)
df <- populateOptions("2016-8-19",df)
df <- populateOptions("2016-8-20",df)
df <- populateOptions("2016-8-21",df)

write.csv(x = df,file = "BreakFastLunchDinnerOptions.csv",row.names = FALSE)
