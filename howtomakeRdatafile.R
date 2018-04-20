require(datasets)


"read in the data"
allData<- read.table(file = "1950-2016_all_tornadoes.csv", header = TRUE,sep = ',') 
"Convert data to date-time format"
allData$date_time <- as.POSIXct(paste(allData$date, allData$time), format = "%Y-%m-%d %H:%M:%S" )
"get some other string info for data"
allData$day_string <- weekdays(allData$date_time)
allData$hour <-lubridate::hour(allData$date_time)
allData$month_abb <- month.abb[allData$mo]
allData$month_abb <- factor(allData$month_abb, levels = month.abb)
allData$date <- date(allData$date_time)
"allData$time_12hr <- format(strptime(allData$time, format='%H:%M:%S'), '%I:%M:%S %p')
allData$time_24hr <- allData$time"
"Want to factor magnitude so we get null values if applicapable"
allData$mag <- factor(allData$mag, levels = c(-9, 0, 1, 2, 3, 4, 5))

fips <- read.table('FIPS code',sep = ',')
names(fips)[1]<-'st'
names(fips)[3]<-'f1'
names(fips)[4]<-'county'
allData <- merge(allData,fips,by=c('st','f1'))


allData$loss_min <- allData$loss

allData$loss_min[allData$yr<2016 & allData$yr>=1996]<-allData$loss_min[allData$yr<2016 & allData$yr>=1996]*10^6
allData$loss_max <- allData$loss_min
allData$loss_min[allData$yr<1996 & allData$loss>0]<-5*10^(allData$loss_min[allData$yr<1996 & allData$loss >0]-1)
allData$loss_max[allData$yr<1996 & allData$loss>0]<-5*10^allData$loss_max[allData$yr<1996 & allData$loss >0]


"!!!!!If any other vairables are needed for the structure add the lines here and create a new version of the .rds file!!!!!"

"create the rdata file"
saveRDS(allData, file = "tornadoes.rds")

"read in data with the below call"
my_data <- readRDS("tornadoes.rds")

"how to sort by time!"
"allData[ order(allData$time , decreasing = FALSE ),]"
"allData <- allData[order(as_datetime(allData$time_12hr, format=%I:%M:%S %p)),]"

"Convert from 24 hr to military"
"format(strptime(allData$time, format='%H:%M:%S'), '%I:%M:%S %p')"




