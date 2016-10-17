source("getOpenRestaurants_Utility.R")

data <- preprocess()

args = commandArgs(trailingOnly=TRUE)
input <- args[1]
inputTime <-strptime(input, "%Y-%m-%d %I:%M %p")
if(is.na(inputTime))
  stop("Invalid input format!! (format e.g. : 2016-10-18 9:00 am)")
open <- get_open_restaurants(input,data)

cat("**************************************************",fill = 1)
cat(str_c("For the given timestamp , number of open restaurants are ",length(open),"."),fill = 1)
cat("Open Restaurants are listed below:",fill = 1)
cat(open, fill=1)