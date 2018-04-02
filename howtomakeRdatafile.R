require(datasets)

allData <- read.table("1950-2016_all_tornadoes.csv")
saveRDS(allData, file = "tornadoes.rds")

"read in data with the below call"
my_data <- readRDS("tornadoes.rds")