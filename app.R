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
              )
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
      temp <- group_by(allData, yr, mag) %>% summarise(count = n()) %>% group_by(mag)
      temp2 <- temp %>% complete(yr, mag) %>% group_by(yr) %>% fill(mag)
      "fill 0's in to dataset"
      temp2[is.na(temp2)] <- 0

      
      ggplot(data=temp2, aes(x=yr, y=count, fill=mag)) +
        geom_bar(stat="identity") + scale_x_continuous(breaks=seq(1950,2016,5)) +
        scale_fill_brewer(palette = "Set1")
      
      
    }
  )
  
} 

shinyApp(ui = ui, server = server)