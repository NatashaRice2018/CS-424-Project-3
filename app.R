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
"libary that lets us find distances between two LAT, Long"
library(geosphere)
library(measurements)


allData <- readRDS("tornadoes.rds")


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

t<-c("24 hour","12 hour am/pm")


                  #tabPanel("Tornados By Hour",
                           #tabBox(
                             #tabPanel("Table",
                             #),
                             #tabPanel("Chart",
                             #)
                  #)
                 #)

comp_state_full <- "Texas"

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
      menuItem("Number of Tornadoes", tabName="number_of_tornadoes"),
      menuItem("Injuries, Fatalities, Losses", tabName="injuries_fatalities_losses"),
      menuItem("Tornado Damage", tabName="tornado_damage"),
      menuItem("Time",
               box(
                 selectInput("Time", "12 hour am/pm time or 24 hour time ", choices=t, selected = '24 hour'), width=650
               )
      ),
      menuItem("Unit",
               box(
                 selectInput("Unit","Miles or Kilometers", choices=c("Miles","Kilometers"), selected = 'Miles'), width=650
               )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    
    tabItems(
      tabItem("about",
              h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddharth Basu"),
              a("Link to project website", href="https://siddharth-basu.github.io/CS424_Project3_Website.io-/")
      ),
      tabItem("number_of_tornadoes",
          fluidRow(width=12,
                tabBox(width=3,height=2150, title="Tornado Facts",
                  tabPanel("Tornados By Year", 
                     tabBox(width=12,
                       tabPanel("Table",
                        box(title = "Illinois Tornados By Year", solidHeader = TRUE, status = "primary",width = 6,
                            radioButtons("table_by_year_view", "Choose one:",  inline = TRUE,
                                         choiceNames = list(
                                           "Numaric Values",
                                           "Percentages"
                                         ),
                                         choiceValues = list(
                                           "numb", "perc"
                                         )),
                            dataTableOutput("table_per_year")),
                        box(title = paste(comp_state_full," Tornados By Year"), solidHeader = TRUE, status = "primary",width = 6,
                            radioButtons("table_by_year_view", "Choose one:",  inline = TRUE,
                                         choiceNames = list(
                                           "Numaric Values",
                                           "Percentages"
                                         ),
                                         choiceValues = list(
                                           "numb", "perc"
                                         )),
                            dataTableOutput("table_per_year_comp_state"))
                       ),
                       tabPanel("Chart",
                        fluidRow(
                          box(title = "Illinois Tornados By Year", solidHeader = TRUE, status = "primary", width = 12, height=925,
                               plotOutput("stacked_bar_per_year", height=850)
                          )
                        ),
                        fluidRow(
                          box( title = paste(comp_state_full," Tornados By Year"), solidHeader = TRUE, status = "primary", width = 12, height=925,
        
                               plotOutput("stacked_bar_per_year_comp_state", height=850)
                          )
                        )
                       )
                    )
                  ),
                  tabPanel("Tornados By Month",
                     tabBox(width=12,
                       tabPanel("Table",
                        box(title = "Illinois Tornadoes by Month", solidHeader = TRUE, status = "primary",width = 6,
                            radioButtons("table_by_month_view", "Choose one:",  inline = TRUE,
                                         choiceNames = list(
                                           "Numaric Values",
                                           "Percentages"
                                         ),
                                         choiceValues = list(
                                           "numb", "perc"
                                         )),
                            dataTableOutput("table_per_month")),
                        box(title = paste(comp_state_full, " Tornados by Month"), solidHeader = TRUE, status = "primary",width = 6,
                            radioButtons("table_by_month_view", "Choose one:",  inline = TRUE,
                                         choiceNames = list(
                                           "Numaric Values",
                                           "Percentages"
                                         ),
                                         choiceValues = list(
                                           "numb", "perc"
                                         )),
                            dataTableOutput("table_per_month_comp_state"))
                       ),
                       tabPanel("Chart",
                          fluidRow(
                            box( title = "Illinois Tornados By Month", solidHeader = TRUE, status = "primary", width = 12,
                                 plotOutput("stacked_bar_per_month")
                            )
                          ),
                          fluidRow(
                            box( title = paste(comp_state_full, " Tornados By Month"), solidHeader = TRUE, status = "primary", width = 12,
                                 plotOutput("stacked_bar_per_month_comp_state")
                            )
                          )
                       )
                     )
                 ),
                 
                  tabPanel("Tornados By Hour",
                           tabBox(width=12,
                             tabPanel("Table",
                                box(title = "Illinois Tornadoes by hour", solidHeader = TRUE, status = "primary",width = 6,
                                    radioButtons("table_by_hour_view", "Choose one:",  inline = TRUE,
                                                 choiceNames = list(
                                                   "Numaric Values",
                                                   "Percentages"
                                                 ),
                                                 choiceValues = list(
                                                   "numb", "perc"
                                                 )),
                                    dataTableOutput("table_per_hour")),
                                box(title = paste(comp_state_full, " Tornadoes by hour"), solidHeader = TRUE, status = "primary",width = 6,
                                    radioButtons("table_by_hour_view", "Choose one:",  inline = TRUE,
                                                 choiceNames = list(
                                                   "Numaric Values",
                                                   "Percentages"
                                                 ),
                                                 choiceValues = list(
                                                   "numb", "perc"
                                                 )),
                                    dataTableOutput("table_per_hour_comp_state"))
                             ),
                             tabPanel("Chart",
                                fluidRow(
                                  box( title = "Illinois Tornados By Hour", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("stacked_bar_per_hour")
                                  )
                                ),
                                fluidRow(
                                  box( title = paste(comp_state_full, " TX Tornados By Hour"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("stacked_bar_per_hour_comp_state")
                                  )
                                )
                             ) #end tab panel
                          ) # End tab box
                  )
               ), # End TabBox for Tornado Facts
               # Start Tab Box for Illinois Tornado Paths
               tabBox(width=3,
                  box(title = "Illinois Tornado Paths", solidHeader = TRUE, status = "primary", width = 12, height=2100,
                      leafletOutput("leaf", height=1900)
                  )
               ),
               # Start Tab Box for Texas Tornado Paths
               tabBox(width=3,
                  box(title = "Texas Tornado Paths", solidHeader = TRUE, status = "primary", width = 12, height=2100,
                      leafletOutput("leaf_comp_state", height=1900)
                  )
               ),
               #Start TabBox for Injuries, Fatalities, Losses
               tabBox(width=3,
                 tabPanel("Injuries, Fatalities, Losses By Year", 
                    tabBox(width=12,
                      tabPanel("Table",
                        box(title = "Illinois Injuries, Fatalities and Losses for each year",
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_year")
                        ),
                        box(title = paste(comp_state_full, " Injuries, fatalities and loss for each year"),
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_year_comp_state")
                        )
                      ),
                      tabPanel("Chart",
                        fluidRow(
                          box(title = "Illinois Injuries, fatalities and losses for each year",
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_year_line")
                          )
                        ),
                        fluidRow(
                          box(title = paste(comp_state_full, " Injuries, fatalities and losses for each year"),
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_year_line_comp_state")
                          )
                        )
                      )
                    ) #end tab box
                 ), # end tab panel
                 tabPanel("Injuries, Fatalities, Losses By Month", 
                    tabBox(width=12,
                      tabPanel("Table",
                        box(title = "Illinois Injuries, fatalities and loss for each month",
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_month")
                        ),
                        box(title = paste(comp_state_full, " Injuries, fatalities and loss for each month"),
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_month_comp_state")
                        )
                      ),
                      tabPanel("Chart",
                        fluidRow(
                          box(title = "Illinois Injuries, fatalities and loss for each month",
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_month_line")
                          )
                        ),
                        fluidRow(
                          box(title = paste(comp_state_full, " Injuries, fatalities and loss for each month"),
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_month_line_comp_state")
                          )
                        )
                      )
                  ) # end tab box in tab panel
                 ), # end tab panel
                 tabPanel("Injuries, Fatalities, Losses By Month", 
                    tabBox(width=12,
                      tabPanel("Table",
                        box(title = "Illinois Injuries, fatalities and loss for each hour",
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_hour")
                        ),
                        box(title = paste(comp_state_full," Injuries, fatalities and loss for each hour"),
                            solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("inj_fat_loss_hour_comp_state")
                        )
                      ),
                      tabPanel("Chart",
                        fluidRow(
                          box(title = "Illinois Injuries, fatalities and loss for each hour",
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_hour_line")
                          )
                        ),
                        fluidRow(
                          box(title = paste(comp_state_full, " Injuries, fatalities and loss for each hour"),
                              solidHeader = TRUE, status = "primary",width = 12,plotOutput("inj_fat_loss_hour_line_comp_state")
                          )
                        )
                      )
                    ) # end tab box in tab panel
                 ) # end tab panel
               ) # end tab box in fluid row
          ) #End FluidRow for main dashboard
            
      ),
      tabItem("injuries_fatalities_losses",
              fluidRow(
                box(title = "Most Hit Counties",
                    solidHeader = TRUE, status = "primary",width = 6,dataTableOutput("most_hit_counties")),
                box(title = "Most Hit Counties",
                    solidHeader = TRUE, status = "primary",width = 6,plotOutput("most_hit_counties_bar")
                    
                )
              )
            ),
      
      tabItem("tornado_damage",
              
              fluidRow(
                titlePanel("Destruction Parameters"),
                
                sidebarPanel(
                  
                  # Input: Simple integer interval ----
                  sliderInput("integer", "Fatalities:",
                              min = 0, max = 1000,
                              value = 500),
                  
                  # Input: Decimal interval with step value ----
                  sliderInput("decimal", "Injuries:",
                              min = 0, max = 1,
                              value = 0.5, step = 0.1),
                  
                  # Input: Specification of range within an interval ----
                  sliderInput("range", "Duration of Tornado:",
                              min = 1, max = 1000,
                              value = c(200,500)),
                  
                  # Input: Custom currency format for with basic animation ----
                  sliderInput("format", "Property Loss Cost:",
                              min = 0, max = 10000,
                              value = 0, step = 2500,
                              pre = "$", sep = ",",
                              animate = TRUE),
                  
                  # Input: Animation with custom interval (in ms) ----
                  # to control speed, plus looping
                  sliderInput("animation", "Looping Animation:",
                              min = 1, max = 2000,
                              value = 1, step = 10,
                              animate =
                                animationOptions(interval = 300, loop = TRUE))
                  
                ),   
                  
                box(title = "Top Destructive Tornados by Time and Power", solidHeader = TRUE, status = "primary", width = 6,
                    leafletOutput("topDestructive")
                )
              ), # end of fluid row
              fluidRow(
                box(title = "Tornadoes by distance", solidHeader = TRUE, status = "primary",width = 6,
                    radioButtons("table_by_dist_view", "Choose one:",  inline = TRUE,
                                 choiceNames = list(
                                   "Numaric Values",
                                   "Percentages"
                                 ),
                                 choiceValues = list(
                                   "numb", "perc"
                                 )),
                    dataTableOutput("table_per_dist")),
                
                
                box( title = "Tornados By Distance from Chicago", solidHeader = TRUE, status = "primary", width = 6,
                     plotOutput("stacked_bar_per_dist")
                )
              ) 
      ) #End tab item
    )
  )
)
  


server <- function(input, output) {
  
  comp_state_abbrev <- "TX"
  
  sliderValues <- reactive({
    
    print(input$integer)
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)
    
  })
  
  
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
  
  set_paths_by_mag<- function(mag_selected, map, table)
  {
    temp_0 = subset(table, mag == mag_selected)
    for(i in 1:nrow(temp_0)){
      "print(i)"
      map <- addPolylines(map, lat = as.numeric(temp_0[i, c("slat", "elat")]), 
                          lng = as.numeric(temp_0[i, c("slon", "elon")]))
    }
    
    map
  }
  
  
  #table showing the number of tornadoes that occurred in every year (1955, 1956, etc)
  table_per_year_func <- function (state_abbrev){
    return (
      DT::renderDataTable(
        DT::datatable({
          temp <- allData %>% filter(st == state_abbrev)
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
        options = list(pageLength = 24))
      )
     )
  }
  
  output$table_per_year<- table_per_year_func("IL")
  output$table_per_year_comp_state<- table_per_year_func(comp_state_abbrev)
  
  #bar chart showing the number of tornadoes that occurred in every year (1955, 1956, etc)
  stacked_bar_per_year_func <- function(state_abbrev) {
    return ( 
      renderPlot({
        temp <- allData %>% filter(st == state_abbrev)
        temp <- group_by(temp, yr, mag) %>% summarise(count = n()) %>% group_by(mag)
        temp2 <- temp %>% complete(yr, mag) %>% group_by(yr) %>% fill(mag)
        "fill 0's in to dataset"
        temp2[is.na(temp2)] <- 0
        
        ggplot(data=temp2, aes(x=yr, y=count, fill=mag)) +
          geom_bar(stat="identity") + scale_x_continuous(breaks=seq(1950,2016,5)) +
          scale_fill_brewer(palette = "Set1")
      })
    )
  }
  
  output$stacked_bar_per_year<- stacked_bar_per_year_func("IL")
  output$stacked_bar_per_year_comp_state<- stacked_bar_per_year_func(comp_state_abbrev)
  
  #table showing the number of tornadoes that occurred over all years for each month (Jan, Feb, etc)
  table_per_month_func <- function(state_abbrev){
    return (
      DT::renderDataTable(
      DT::datatable({
        temp <- allData %>% filter(st == state_abbrev)
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
    )
  }
  
  output$table_per_month<- table_per_month_func("IL")
  output$table_per_month_comp_state<- table_per_month_func(comp_state_abbrev)
  
  #bar chart showing the number of tornadoes that occurred over all years for each month (Jan, Feb, etc)
  stacked_bar_per_month_func <- function(state_abbrev){
    return (
      renderPlot({
      temp <- allData %>% filter(st == state_abbrev)
      temp <- group_by(temp, month_abb, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(month_abb, mag) %>% group_by(month_abb) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      
      ggplot(data=temp2, aes(x=month_abb, y=count, fill=mag)) +
        geom_bar(stat="identity") + 
        scale_fill_brewer(palette = "Set1")
      })
    )
  }
  
  output$stacked_bar_per_month<- stacked_bar_per_month_func("IL")
  output$stacked_bar_per_month_comp_state<- stacked_bar_per_month_func(comp_state_abbrev)
  
  #table showing the number of tornadoes that occurred over all years for each hour in 24-hour period
  table_per_hour_func <- function(state_abbrev){
    return (
      DT::renderDataTable(
        DT::datatable({
          temp <- allData %>% filter(st == state_abbrev)
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
        options = list(pageLength = 24))
      )
   )
  }
  
  output$table_per_hour<- table_per_hour_func("IL")
  output$table_per_hour_comp_state<- table_per_hour_func(comp_state_abbrev)
  
  #bar chart showing the number of tornadoes that occurred over all years for each hour in 24-hour period
  stacked_bar_per_hour_func <- function(state_abbrev){
    return (
      renderPlot({
        temp <- allData %>% filter(st == state_abbrev)
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
     })
    )
  }
  
  output$stacked_bar_per_hour<- stacked_bar_per_hour_func("IL")
  output$stacked_bar_per_hour_comp_state<- stacked_bar_per_hour_func(comp_state_abbrev)
  
  ### Injuries, Fatalities, Losses 
  #table showing the injuries, fatalities, loss  all years
  inj_fat_loss_year_func <- function(state_abbrev){
     return (
          DT::renderDataTable(
            DT::datatable({
              temp <- allData %>% filter(st == state_abbrev)
              n_inj_year <- aggregate(inj ~ yr, data = temp, sum)
              n_fat_year <- aggregate(fat ~ yr, data = temp, sum)
              n_loss_year_min <- aggregate(loss_min ~ yr, data = temp, sum)
              n_loss_year_max <- aggregate(loss_max ~ yr, data = temp, sum)
              
              inj_fat_loss_year <- merge(n_inj_year,n_fat_year)
              inj_fat_loss_year <- merge(inj_fat_loss_year,n_loss_year_min)
              inj_fat_loss_year <- merge(inj_fat_loss_year,n_loss_year_max)
              
              inj_fat_loss_year <- as.data.frame(inj_fat_loss_year)
              inj_fat_loss_year
            },
            options = list(pageLength = 24)
            )
          )
     )
   }
  
  output$inj_fat_loss_year <- inj_fat_loss_year_func("IL")
  output$inj_fat_loss_year_comp_state <- inj_fat_loss_year_func(comp_state_abbrev)
  
  #chart showing the injuries, fatalities, loss  all years
  inj_fat_loss_year_line_func <- function(state_abbrev){
    return (
      renderPlot({
        temp <- allData %>% filter(st == state_abbrev)
        n_inj_year <- aggregate(inj ~ yr, data = temp, sum)
        n_fat_year <- aggregate(fat ~ yr, data = temp, sum)
        n_loss_year_min <- aggregate(loss_min ~ yr, data = temp, sum)
        n_loss_year_max <- aggregate(loss_max ~ yr, data = temp, sum)
        
        inj_fat_loss_year <- merge(n_inj_year,n_fat_year)
        inj_fat_loss_year <- merge(inj_fat_loss_year,n_loss_year_min)
        inj_fat_loss_year <- merge(inj_fat_loss_year,n_loss_year_max)
        
        inj_fat_loss_year <- as.data.frame(inj_fat_loss_year)
        
        names(inj_fat_loss_year)[1]<-'Year'
        
        dat.m <- melt(inj_fat_loss_year, "Year")
        
        ggplot(dat.m, aes(Year, value, colour = variable)) + geom_line() +
          facet_wrap(~ variable, ncol = 1, scales = "free_y")
      })
    )
  }
  
  output$inj_fat_loss_year_line <- inj_fat_loss_year_line_func("IL")
  output$inj_fat_loss_year_line_comp_state <- inj_fat_loss_year_line_func("TX")
    
  # table showing the injuries, fatalities, loss per month summed over all years
  inj_fat_loss_month_func <- function(state_abbrev){
    return (
      DT::renderDataTable(
        DT::datatable({
          temp <- allData %>% filter(st == state_abbrev)
          n_inj_month <- aggregate(inj ~ month_abb, data = temp, sum)
          n_fat_month <- aggregate(fat ~ month_abb, data = temp, sum)
          n_loss_month_min <- aggregate(loss_min ~ month_abb, data = temp, sum)
          n_loss_month_max <- aggregate(loss_max ~ month_abb, data = temp, sum)
          
          inj_fat_loss_month <- merge(n_inj_month,n_fat_month)
          inj_fat_loss_month <- merge(inj_fat_loss_month,n_loss_month_min)
          inj_fat_loss_month <- merge(inj_fat_loss_month,n_loss_month_max)
          
          inj_fat_loss_month <- as.data.frame(inj_fat_loss_month)
          inj_fat_loss_month
        },
        options = list(pageLength = 12, order = list(list(1, 'asc')))
        )
      )
    )
  }
  
  output$inj_fat_loss_month <- inj_fat_loss_month_func("IL") 
  output$inj_fat_loss_month_comp_state <- inj_fat_loss_month_func(comp_state_abbrev)
  
  # chart showing the injuries, fatalities, loss per month summed over all years
  inj_fat_loss_month_line_func <- function(state_abbrev){
    return (
      renderPlot({
        temp <- allData %>% filter(st == state_abbrev)
        n_inj_month <- aggregate(inj ~ month_abb, data = temp, sum)
        n_fat_month <- aggregate(fat ~ month_abb, data = temp, sum)
        n_loss_month_min <- aggregate(loss_min ~ month_abb, data = temp, sum)
        n_loss_month_max <- aggregate(loss_max ~ month_abb, data = temp, sum)
        
        inj_fat_loss_month <- merge(n_inj_month,n_fat_month)
        inj_fat_loss_month <- merge(inj_fat_loss_month,n_loss_month_min)
        inj_fat_loss_month <- merge(inj_fat_loss_month,n_loss_month_max)
        
        inj_fat_loss_month <- as.data.frame(inj_fat_loss_month)
          
          names(inj_fat_loss_month)[1]<-'Month'
          inj_fat_loss_month<-inj_fat_loss_month[order(match(inj_fat_loss_month$Month, month.abb)), ]
          dat.m <- melt(inj_fat_loss_month, "Month")
          
          ggplot(dat.m, aes(Month, value, colour = variable)) + geom_line(aes(colour=variable,group=variable)) +
            facet_wrap(~ variable, ncol = 1, scales = "free_y")
      })
    )
  }
  
  output$inj_fat_loss_month_line <- inj_fat_loss_month_line_func("IL")
  output$inj_fat_loss_month_line_comp_state <- inj_fat_loss_month_line_func(comp_state_abbrev)
  
  # table showing the injuries, fatalities, loss per hour summed over all years
  inj_fat_loss_hour_func <- function(state_abbrev){
     return (
        DT::renderDataTable(
          DT::datatable({
            temp <- allData %>% filter(st == state_abbrev)
            n_inj_hour <- aggregate(inj ~ hour, data = temp, sum)
            n_fat_hour <- aggregate(fat ~ hour, data = temp, sum)
            n_loss_hour_min <- aggregate(loss_min ~ hour, data = temp, sum)
            n_loss_hour_max <- aggregate(loss_max ~ hour, data = temp, sum)
            
            inj_fat_loss_hour <- merge(n_inj_hour,n_fat_hour)
            inj_fat_loss_hour <- merge(inj_fat_loss_hour,n_loss_hour_min)
            inj_fat_loss_hour <- merge(inj_fat_loss_hour,n_loss_hour_max)
            
            inj_fat_loss_hour$hour<-switch_hour(inj_fat_loss_hour$hour)
            #set a factor for time baised on what clock we are in
            inj_fat_loss_hour$hour <- set_time_factor(inj_fat_loss_hour$hour)
            
            inj_fat_loss_hour <- as.data.frame(inj_fat_loss_hour)
            inj_fat_loss_hour
          },
          options = list(pageLength = 24)
          )
        )
     )
   }
  
  output$inj_fat_loss_hour <- inj_fat_loss_hour_func("IL")
  output$inj_fat_loss_hour_comp_state <- inj_fat_loss_hour_func(comp_state_abbrev)
  
  # chart showing the injuries, fatalities, loss per hour summed over all years
  inj_fat_loss_hour_line_func <- function(state_abbrev){
    return (
      renderPlot(
        {
          temp <- allData %>% filter(st == state_abbrev)
          n_inj_hour <- aggregate(inj ~ hour, data = temp, sum)
          n_fat_hour <- aggregate(fat ~ hour, data = temp, sum)
          n_loss_hour_min <- aggregate(loss_min ~ hour, data = temp, sum)
          n_loss_hour_max <- aggregate(loss_max ~ hour, data = temp, sum)
          
          inj_fat_loss_hour <- merge(n_inj_hour,n_fat_hour)
          inj_fat_loss_hour <- merge(inj_fat_loss_hour,n_loss_hour_min)
          inj_fat_loss_hour <- merge(inj_fat_loss_hour,n_loss_hour_max)
          
          inj_fat_loss_hour$hour<-switch_hour(inj_fat_loss_hour$hour)
          #set a factor for time baised on what clock we are in
          inj_fat_loss_hour$hour <- set_time_factor(inj_fat_loss_hour$hour)
          
          inj_fat_loss_hour <- as.data.frame(inj_fat_loss_hour)
          names(inj_fat_loss_hour)[1]<-'Hour'
          
          dat.m <- melt(inj_fat_loss_hour, "Hour")
          
          ggplot(dat.m, aes(Hour, value, colour = variable)) + geom_line(aes(colour=variable,group=variable)) +
            facet_wrap(~ variable, ncol = 1, scales = "free_y")
        }
      )
    )
  }
  
  output$inj_fat_loss_hour_line <- inj_fat_loss_hour_line_func("IL") 
  output$inj_fat_loss_hour_line_comp_state <- inj_fat_loss_hour_line_func(comp_state_abbrev) 
 
  #### Illinois-specific charts and tables
 
  # Calculates Tornados By Distance to Chicago (doesn't make sense for comparison for states) 
  output$table_per_dist<- DT::renderDataTable(
    DT::datatable({
      
      temp <- allData %>% filter(st == "IL", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
      latlong <- temp[, c("slon","slat")]
      temp$dist_From_chi <- distm( latlong, c(-87.63, 41.88), fun = distHaversine)
      
      
      "Convert data to the correct format"
      if(input$Unit == "Miles")
      {
        breaks_milimeters <- c(0, 161900, 250000, 320000, 400000, 480000, 580000 )
        breaks_miles <- conv_unit(breaks_milimeters , "m", "mi")
        temp$dist_From_chi = conv_unit(temp$dist_From_chi , "m", "mi")
        temp$groups = cut(temp$dist_From_chi, breaks_miles)
        label <- c("0 - 100", "100-150", "151 - 199", "200 - 250", "251 - 300", "300 - 360")
      }
      else
      {
        breaks_milimeters <- c(0, 161900, 250000, 320000, 400000, 480000, 580000 )
        breaks_Kmilimeters <- conv_unit(breaks_milimeters , "m", "km")
        temp$dist_From_chi = conv_unit(temp$dist_From_chi , "m", "km")
        temp$groups = cut(temp$dist_From_chi, breaks_Kmilimeters)
        label <- c("0 - 162", "162-250", "251 - 320", "321 - 400", "401 - 480", "481 - 580")
      }
      
      temp <- group_by(temp, groups, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(groups, mag) %>% group_by(groups) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0
      
      temp3 <- cast(temp2, groups ~ mag, mean, value = "count")
      
      "calculate totals"
      temp3$groups_total <-  rowSums(temp3[2:8])
      
      
      "calculate percent for each groups"
      
      temp3$'-9 Percent' <- percent(temp3$`-9`/temp3$groups_total)
      temp3$'0 Percent' <- percent(temp3$`0`/temp3$groups_total)
      temp3$'1 Percent' <- percent(temp3$`1`/temp3$groups_total)
      temp3$'2 Percent' <- percent(temp3$`2`/temp3$groups_total)
      temp3$'3 Percent' <- percent(temp3$`3`/temp3$groups_total)
      temp3$'4 Percent' <- percent(temp3$`4`/temp3$groups_total)
      temp3$'5 Percent' <- percent(temp3$`5`/temp3$groups_total)
      
      "move the total to last col"
      temp3 <- temp3%>%select(-groups_total,groups_total)
      
      "Remove Col user does not want to see"
      if(input$table_by_dist_view == "numb")
      {
        finalChart <- temp3[-c(9:15)]
      }
      else
      {
        finalChart <- temp3[-c(2:8)]
      }
      
      finalChart
      
      
    },
    options = list(pageLength = 24))
  )
  
  # Calculates Tornados By Distance to Chicago (doesn't make sense for comparison for states) 
  output$stacked_bar_per_dist<- renderPlot({
    temp <- allData %>% filter(st == "IL", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
    latlong <- temp[, c("slon","slat")]
    temp$dist_From_chi <- distm( latlong, c(-87.63, 41.88), fun = distHaversine)
    
    
    "Convert data to the correct format"
    if(input$Unit == "Miles")
    {
      breaks_milimeters <- c(0, 161900, 250000, 320000, 400000, 480000, 580000 )
      breaks_miles <- conv_unit(breaks_milimeters , "m", "mi")
      temp$dist_From_chi = conv_unit(temp$dist_From_chi , "m", "mi")
      temp$groups = cut(temp$dist_From_chi, breaks_miles)
      label <- c("0 - 100", "100-150", "151 - 199", "200 - 250", "251 - 300", "300 - 360")
    }
    else
    {
      breaks_milimeters <- c(0, 161900, 250000, 320000, 400000, 480000, 580000 )
      breaks_Kmilimeters <- conv_unit(breaks_milimeters , "m", "km")
      temp$dist_From_chi = conv_unit(temp$dist_From_chi , "m", "km")
      temp$groups = cut(temp$dist_From_chi, breaks_Kmilimeters)
      label <- c("0 - 162", "162-250", "251 - 320", "321 - 400", "401 - 480", "481 - 580")
    }
    
    temp <- group_by(temp, groups, mag) %>% summarise(count = n()) %>% group_by(mag)
    temp2 <- temp %>% complete(groups, mag) %>% group_by(groups) %>% fill(mag)
    "fill 0's in to dataset"
    temp2[is.na(temp2)] <- 0
    
    ggplot(data=temp2, aes(x=groups, y=count, fill=mag)) +
      geom_bar(stat="identity") + 
      scale_fill_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
      xlab("Distance") + ylab("Count") +
      scale_x_discrete(breaks= unique(temp2$groups),
                       labels= label)
  }
  )
  
  ## Table and chart showing which counties in Illinois were most hit by tornadoes summed over all years
  output$most_hit_counties <- DT::renderDataTable(
    DT::datatable({
      temp <- allData %>% filter(st == "IL")
      most_hit_counties <- group_by(temp,county) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
      most_hit_counties <- as.data.frame(most_hit_counties)
      most_hit_counties
    },
    options = list(pageLength = 12)
    )
  )
  
  output$most_hit_counties_bar <- renderPlot(
    {
      temp <- allData %>% filter(st == "IL", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
      temp2<-group_by(temp,county) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
      temp2$county <- factor(temp2$county,levels=temp2$county)
      ggplot(data=temp2, aes(x=county, y=count)) +
        geom_bar(stat="identity")
    }
  )
  
  output$leaf <- renderLeaflet({
    temp <- allData %>% filter(st == "IL", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
    map3 = leaflet(temp) %>% addTiles()
    "map3 %>% addMarkers(~elon, ~elat, popup = ~date_time)"
    map3 = set_paths_by_mag(0, map3, temp)
    map3 = set_paths_by_mag(1, map3, temp)
    map3 = set_paths_by_mag(2, map3, temp)
    map3 = set_paths_by_mag(3, map3, temp)
    map3 = set_paths_by_mag(4, map3, temp)
    map3 = set_paths_by_mag(5, map3, temp)
    map3
  })
  
  output$leaf_comp_state <- renderLeaflet({
    temp <- allData %>% filter(st == "TX", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
    map3 = leaflet(temp) %>% addTiles()
    "map3 %>% addMarkers(~elon, ~elat, popup = ~date_time)"
    map3 = set_paths_by_mag(0, map3, temp)
    map3 = set_paths_by_mag(1, map3, temp)
    map3 = set_paths_by_mag(2, map3, temp)
    map3 = set_paths_by_mag(3, map3, temp)
    map3 = set_paths_by_mag(4, map3, temp)
    map3 = set_paths_by_mag(5, map3, temp)
    map3
  })
  
  output$topDestructive <- renderLeaflet({
    temp <- allData %>% filter(st == "IL", elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
    
    "All options for map views"
    "http://leaflet-extras.github.io/leaflet-providers/preview/index.html"

    map3 = leaflet(temp) %>% addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain")  %>%
      addProviderTiles(providers$Stamen.Watercolor, group = "Watercolor")
    
    
    "create a list for all magnitude groups"
    l <- vector("list", nrow(temp))
    "map3 %>% addMarkers(~elon, ~elat, popup = ~date_time)"
    for(i in 1:nrow(temp)){
      "print(i)"
      map3 <- addPolylines(map3, lat = as.numeric(temp[i, c("slat", "elat")]), 
                          lng = as.numeric(temp[i, c("slon", "elon")]), group = paste("Magnitude ",temp[i,"mag"]))
      l[[i]] <- (paste("Magnitude ",temp[i,"mag"]))
    }
    map3 = map3 %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Terrain", "Watercolor" ),
        overlayGroups = unique(l),
        options = layersControlOptions(collapsed = FALSE)
      )
    map3
  })
  

  
} 

shinyApp(ui = ui, server = server)