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

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

t<-c("24 hour","12 hour am/pm")

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
      menuItem("Part C", tabName="part_c"),
      menuItem("Time",
               box(
                 selectInput("Time", "12 hour am/pm time or 24 hour time ", choices=t, selected = '24 hour'), width=650
               )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",
              h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddharth Basu"),
              a("Link to project website", href="https://siddharth-basu.github.io/CS424_Project3_Website.io-/")
      ),
      tabItem("part_c",
              fluidRow(
                box(title = "Tornadoes by year", solidHeader = TRUE, status = "primary",width = 6,
                    radioButtons("table_by_year_view", "Choose one:",  inline = TRUE,
                                 choiceNames = list(
                                   "Numaric Values",
                                   "Percentages"
                                 ),
                                 choiceValues = list(
                                   "numb", "perc"
                                 )),
                    dataTableOutput("table_per_year")),
                
                box( title = "Tornados By Year", solidHeader = TRUE, status = "primary", width = 6,
                     plotOutput("stacked_bar_per_year")
                )
              ),
              fluidRow(
                box(title = "Tornadoes by Month", solidHeader = TRUE, status = "primary",width = 6,
                    radioButtons("table_by_month_view", "Choose one:",  inline = TRUE,
                                 choiceNames = list(
                                   "Numaric Values",
                                   "Percentages"
                                 ),
                                 choiceValues = list(
                                   "numb", "perc"
                                 )),
                    dataTableOutput("table_per_month")),
                
                box( title = "Tornados By Month", solidHeader = TRUE, status = "primary", width = 6,
                     plotOutput("stacked_bar_per_month")
                )
              ),
    
              
              fluidRow(
                box(title = "Tornadoes by hour", solidHeader = TRUE, status = "primary",width = 6,
                    radioButtons("table_by_hour_view", "Choose one:",  inline = TRUE,
                                 choiceNames = list(
                                   "Numaric Values",
                                   "Percentages"
                                 ),
                                 choiceValues = list(
                                   "numb", "perc"
                                 )),
                    dataTableOutput("table_per_hour")),
                
                box( title = "Tornados By Hour", solidHeader = TRUE, status = "primary", width = 6,
                     plotOutput("stacked_bar_per_hour")
                )
              ),
              fluidRow(
                box(title = "Injuries, fatalities and loss for each year",
                    solidHeader = TRUE, status = "primary",width = 8,dataTableOutput("inj_fat_loss_year")
                    
                ),
                box(title = "Injuries, fatalities and loss for each month",
                    solidHeader = TRUE, status = "primary",width = 8,dataTableOutput("inj_fat_loss_month")
                    
                ))
              
              
            )
      
    )
  )
)

server <- function(input, output) {
  
  switch_hour<- function(x){
    c <- x
    #ifelse(input$Time=="24 hour", c<-paste(c$hour,":00",sep=""), ifelse(c<12, paste(c,":00 AM",sep=""),paste(cc-12,":00 PM",sep = "")))
    if (input$Time !="24 hour"){
      c <- ifelse(c<12, paste(c,":00 AM",sep=""),paste(c-12,":00 PM",sep = ""))
      #code currently has a 0:00am - we need to change that to 12 am.
      c[c == "0:00 AM"] <- "12:00 AM"
      c[c == "0:00 PM"] <- "12:00 PM"
    } else {
      c<-paste(c,":00",sep="")
    }
    c
  }
  
  set_time_factor<-function(x)
  {
    time <- x
    if (input$Time =="24 hour"){
      temp <- c("0:00","1:00", "2:00", "3:00", "4:00", "5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00","13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00");
    }
    else{
      temp <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM");
    }
    time <- factor(time, levels = temp)
    
    time
  }
  
  output$table_per_year<- DT::renderDataTable(
    DT::datatable({
      temp <- allData %>% filter(st == "IL")
      temp <- group_by(temp, yr, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(yr, mag) %>% group_by(yr) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      "get data into correct form"
      temp3 <- cast(temp2, yr ~ mag, mean, value = "count")
      
      "calculate totals"
      temp3$year_total <-  rowSums(temp3[2:8])
      
      "calculate percent for each year"
      
      temp3$'-9 Percent' <- percent(temp3$`-9`/temp3$year_total)
      temp3$'0 Percent' <- percent(temp3$`0`/temp3$year_total)
      temp3$'1 Percent' <- percent(temp3$`1`/temp3$year_total)
      temp3$'2 Percent' <- percent(temp3$`2`/temp3$year_total)
      temp3$'3 Percent' <- percent(temp3$`3`/temp3$year_total)
      temp3$'4 Percent' <- percent(temp3$`4`/temp3$year_total)
      temp3$'5 Percent' <- percent(temp3$`5`/temp3$year_total)
      
      "move the total to last col"
      temp3 <- temp3%>%select(-year_total,year_total)
      
      "Remove Col user does not want to see"
      if(input$table_by_year_view == "numb")
      {
        finalChart <- temp3[-c(9:15)]
      }
      else
      {
        finalChart <- temp3[-c(2:8)]
      }
      
      finalChart
    },
    options = list(pageLength = 12))
  )
  
  output$stacked_bar_per_year<- renderPlot({
    temp <- allData %>% filter(st == "IL")
      temp <- group_by(temp, yr, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(yr, mag) %>% group_by(yr) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0

      
      ggplot(data=temp2, aes(x=yr, y=count, fill=mag)) +
        geom_bar(stat="identity") + scale_x_continuous(breaks=seq(1950,2016,5)) +
        scale_fill_brewer(palette = "Set1")
      
      
    }
  )
  
  output$table_per_month<- DT::renderDataTable(
    DT::datatable({
      temp <- allData %>% filter(st == "IL")
      temp <- group_by(temp, month_abb, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(month_abb, mag) %>% group_by(month_abb) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      "get data into correct form"
      temp3 <- cast(temp2, month_abb ~ mag, mean, value = "count")
      
      "calculate totals"
      temp3$month_total <-  rowSums(temp3[2:8])
      
      "calculate percent for each month"
      
      temp3$'-9 Percent' <- percent(temp3$`-9`/temp3$month_total)
      temp3$'0 Percent' <- percent(temp3$`0`/temp3$month_total)
      temp3$'1 Percent' <- percent(temp3$`1`/temp3$month_total)
      temp3$'2 Percent' <- percent(temp3$`2`/temp3$month_total)
      temp3$'3 Percent' <- percent(temp3$`3`/temp3$month_total)
      temp3$'4 Percent' <- percent(temp3$`4`/temp3$month_total)
      temp3$'5 Percent' <- percent(temp3$`5`/temp3$month_total)
      
      "move the total to last col"
      temp3 <- temp3%>%select(-month_total,month_total)
      
      "Remove Col user does not want to see"
      if(input$table_by_month_view == "numb")
      {
        finalChart <- temp3[-c(9:15)]
      }
      else
      {
        finalChart <- temp3[-c(2:8)]
      }
      
      finalChart
    },
    options = list(pageLength = 12))
  )
  
  output$stacked_bar_per_month<- renderPlot({
    temp <- allData %>% filter(st == "IL")
    temp <- group_by(temp, month_abb, mag) %>% summarise(count = n()) %>% group_by(mag)
    temp2 <- temp %>% complete(month_abb, mag) %>% group_by(month_abb) %>% fill(mag)
    "fill 0's in to dataset"
    temp2[is.na(temp2)] <- 0
    
    
    ggplot(data=temp2, aes(x=month_abb, y=count, fill=mag)) +
      geom_bar(stat="identity") + 
      scale_fill_brewer(palette = "Set1")
    
  }
  )
  
  output$table_per_hour<- DT::renderDataTable(
    DT::datatable({
      temp <- allData %>% filter(st == "IL")
      temp <- group_by(temp, hour, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(hour, mag) %>% group_by(hour) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      "get data into correct form"
      temp3 <- cast(temp2, hour ~ mag, mean, value = "count")
      
      "calculate totals"
      temp3$hour_total <-  rowSums(temp3[2:8])
      
      temp3$hour<-switch_hour(temp3$hour)
      #set a factor for time baised on what clock we are in
      temp3$hour <- set_time_factor(temp3$hour)
      
      "calculate percent for each hour"
      
      temp3$'-9 Percent' <- percent(temp3$`-9`/temp3$hour_total)
      temp3$'0 Percent' <- percent(temp3$`0`/temp3$hour_total)
      temp3$'1 Percent' <- percent(temp3$`1`/temp3$hour_total)
      temp3$'2 Percent' <- percent(temp3$`2`/temp3$hour_total)
      temp3$'3 Percent' <- percent(temp3$`3`/temp3$hour_total)
      temp3$'4 Percent' <- percent(temp3$`4`/temp3$hour_total)
      temp3$'5 Percent' <- percent(temp3$`5`/temp3$hour_total)
      
      "move the total to last col"
      temp3 <- temp3%>%select(-hour_total,hour_total)
      
      "Remove Col user does not want to see"
      if(input$table_by_hour_view == "numb")
      {
        finalChart <- temp3[-c(9:15)]
      }
      else
      {
        finalChart <- temp3[-c(2:8)]
      }
      
      finalChart
    },
    options = list(pageLength = 12))
  )
  
  
  output$stacked_bar_per_hour<- renderPlot({
    temp <- allData %>% filter(st == "IL")
    temp <- group_by(temp, hour, mag) %>% summarise(count = n()) %>% group_by(mag)
    temp2 <- temp %>% complete(hour, mag) %>% group_by(hour) %>% fill(mag)
    "fill 0's in to dataset"
    temp2[is.na(temp2)] <- 0
    
    temp2$hour<-switch_hour(temp2$hour)
    #set a factor for time baised on what clock we are in
    temp2$hour <- set_time_factor(temp2$hour)
    
    
    ggplot(data=temp2, aes(x=hour, y=count, fill=mag)) +
      geom_bar(stat="identity") + 
      scale_fill_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
    
  }
  )
  
  #table and chart showing the injuries, fatalities, loss per month summed over all years
  output$inj_fat_loss_year <- DT::renderDataTable(
    DT::datatable({
      n_inj_year <- aggregate(inj ~ yr, data = my_data, sum)
      n_fat_year <- aggregate(fat ~ yr, data = my_data, sum)
      n_loss_year <- aggregate(loss ~ yr, data = my_data, sum)
      inj_fat_loss_year <- merge(n_inj_year,n_fat_year)
      inj_fat_loss_year <- merge(inj_fat_loss_year,n_loss_year)
      
      inj_fat_loss_year <- as.data.frame(inj_fat_loss_year)
      inj_fat_loss_year
    }))
  # table and chart showing the injuries, fatalities, loss per hour of the day summed over all years
  output$inj_fat_loss_month <- DT::renderDataTable(
    DT::datatable({
      n_inj_month <- aggregate(inj ~ mo, data = my_data, sum)
      n_fat_month <- aggregate(fat ~ mo, data = my_data, sum)
      n_loss_month <- aggregate(loss ~ mo, data = my_data, sum)
      inj_fat_loss_month <- merge(n_inj_month,n_fat_month)
      inj_fat_loss_month <- merge(inj_fat_loss_month,n_loss_month)
      
      inj_fat_loss_month <- as.data.frame(inj_fat_loss_month)
      inj_fat_loss_month
    }))
  
  
} 

shinyApp(ui = ui, server = server)