require(datasets)



allData<- read.table(file = "1950-2016_all_tornadoes.csv", header = TRUE,sep = ',') 
saveRDS(allData, file = "tornadoes.rds")

"read in data with the below call"
my_data <- readRDS("tornadoes.rds")