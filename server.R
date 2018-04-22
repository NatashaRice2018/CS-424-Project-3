library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

library(sp)

function(input, output, session) {
  output$state <- renderPrint({
    input$state
  })
  
  
  ## Interactive Map ###########################################
  filteredData <- reactive({ allData[ allData$wid >= input$width[1] & allData$wid <= input$width[2] &
                                        allData$len >= input$length[1] & allData$len <= input$length[2] &
                                        allData$loss >= input$loss[1] & allData$loss <= input$loss[2] &
                                        allData$fat >= input$fat[1] & allData$fat <= input$fat[2] &
                                        allData$inj >= input$injuries[1] & allData$inj <= input$injuries[2] &
                                        allData$yr >= input$year[1] & allData$yr <= input$year[2] &
                                        allData$mag %in% input$magnitude &
                                        allData$st == input$state, ]})
  
  output$countiesList <- renderPrint({
    temp <- filteredData()
    temp$county
    selectInput("selCounty", "Select County", unique(temp$county))
  })

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png"
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })


  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    filtered = filteredData()

    temp <- filtered %>% filter(elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)
    "filter by county if selected"
    if(input$county) {
      temp <- temp %>%  filter(county == input$selCounty)
      
      }
    if(nrow(temp) == 0)
    {
      map = leafletProxy("map", data = temp) %>%
        clearShapes() 
      return()
    }
    "Magnitudes are set by factor so they are unique"
    if(colorBy == "mag")
    {
      colorData <- temp[[colorBy]]
      pal <- colorFactor("Set1", colorData)
    }
    else{
      colorData <- temp[[colorBy]]
      pal <- colorBin("Set1", colorData, 7, pretty = TRUE)
    }
    if(sizeBy == "mag")
    {
      weightCalc <- (as.numeric(temp[[sizeBy]]) + 1 ) / max((as.numeric(temp[[sizeBy]]) + 1) )* 15
    }
    else{
      weightCalc <- (temp[[sizeBy]] +1 ) / max((temp[[sizeBy]] + 1 )) * 50
    }
    
    "temp <- illdata %>% filter(elat != 0.0, slat != 0.0, slon != 0.0, elon != 0.0)"
    id  <- rownames(temp)
    
    temp$id <- id
    
    flights_lines <- apply(temp, 1,function(x){
      points <- data.frame(lng=as.numeric(c(x["slon"], 
                                            x["elon"])),
                           lat=as.numeric(c(x["slat"], 
                                            x["elat"])))
      coordinates(points) <- c("lng","lat")
      Lines(Line(points),ID=x["id"])
    })

    flights_lines <- SpatialLinesDataFrame(SpatialLines(flights_lines),temp)
    
    map = leafletProxy("map", data = temp) %>%
      clearShapes() %>%
      addProviderTiles(input$map) %>%
      addPolylines(data=flights_lines, weight = weightCalc, color = pal(colorData))

    
    map = map  %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
      
  })

}
