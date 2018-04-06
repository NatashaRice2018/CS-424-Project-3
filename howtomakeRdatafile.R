require(datasets)


"read in the data"
allData<- read.table(file = "1950-2016_all_tornadoes.csv", header = TRUE,sep = ',') 
"Convert data to date-time format"
allData$date_time <- as.POSIXct(paste(allData$date, allData$time), format = "%Y-%m-%d %H:%M:%S" )
"get some other string info for data"
allData$day_string <- weekdays(allData$date_time)
allData$hour <-lubridate::hour(allData$date_time)
allData$month_abb <- month.abb[allData$mo]
allData$date <- date(allData$date_time)
allData$time_12hr <- format(strptime(allData$time, format='%H:%M:%S'), '%I:%M:%S %p')
allData$time_24hr <- allData$time


"!!!!!If any other vairables are needed for the structure add the lines here and create a new version of the .rds file!!!!!"

"create the rdata file"
saveRDS(allData, file = "tornadoes.rds")

"read in data with the below call"
my_data <- readRDS("tornadoes.rds")

"how to sort by time!"
"allData[ order(allData$time , decreasing = FALSE ),]"

"Convert from 24 hr to military"
"format(strptime(allData$time, format='%H:%M:%S'), '%I:%M:%S %p')"



