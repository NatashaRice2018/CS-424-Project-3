library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(dplyr)
library(plotly)

"allows us to add missing values"
library(tidyverse)
"Allows us to piviot tables if needed"
library(reshape)


allData <- readRDS("tornadoes.rds")

ui <- dashboardPage(
  dashboardHeader
  (
    title = "You Spin Me Round",titleWidth=650
  ),
  dashboardSidebar
  (
    width= 650,
    sidebarMenu(
      menuItem("About", tabName="about"),
      menuItem("Part C", tabName="part_c")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",
              h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddharth Basu"),
              a("Link to project website", href="#")
      )
    )
  )
)

server <- function(input, output) {
  
  output$table_per_year<- DT::renderDataTable(
    DT::datatable({
      temp <- group_by(allData, yr, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(yr, mag) %>% group_by(yr) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      "get data into correct form"
      cast(temp2, yr ~ mag, mean, value = "count")
      
      
    },
    options = list(pageLength = 12))
  )
  
} 

shinyApp(ui = ui, server = server)