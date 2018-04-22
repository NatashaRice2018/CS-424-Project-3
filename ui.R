library(leaflet)

# Choices for drop-downs
vars <- c(
  "Magnitude" = "mag",
  "Width" = "wid",
  "Loss" = "loss",
  "Injuries" = "inj",
  "Fatalities" = "fat",
  "Length" = "len"
)

mapView <- c(
  "Plain" = "OpenStreetMap.DE",
  "Plain (Black/White)" = "OpenStreetMap.BlackAndWhite",
  "Dark" = "CartoDB.DarkMatter",
  "Electricity" = "NASAGIBS.ViirsEarthAtNight2012",
  "Topoligical" = "OpenTopoMap",
  "Cities/Towns" = "Hydda.Full",
  "Realistic" = "Esri.WorldImagery",
  "Rivers" = "Esri.OceanBasemap",
  "Fun" = "Thunderforest.SpinalMap"
)


navbarPage("Superzip", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Tornado Views"),
        selectInput("map", "MAp Type", mapView, selected = "Plain"),
        selectInput("state", "State", unique(allData$st), selected = "IL"),
        illdata = allData %>% filter(st == verbatimTextOutput("state")),
        checkboxInput("county", "View by Couny", value = F),
        conditionalPanel("input.county", uiOutput("countiesList")),
        selectInput("color", "Color", vars, selected = "mag"),
        selectInput("size", "Size", vars, selected = "wid"),
        #filter all data by stte here
        
        sliderInput("width", "Width", min(illdata$wid), max(illdata$wid),
                    value = range(illdata$wid), step = 1.0
        ),
        sliderInput("length", "Length", min(illdata$len), max(illdata$len),
                    value = range(illdata$len), step = 1.0
        ),
        sliderInput("loss", "loss", min(illdata$loss), max(illdata$loss),
                    value = range(illdata$loss), step = 1.0
        ),
        sliderInput("fat", "Fatility", min(illdata$fat), max(illdata$fat),
                    value = range(illdata$fat), step = 1.0
        ),
        sliderInput("injuries", "Injuries", min(illdata$inj), max(illdata$inj),
                    value = range(illdata$inj), step = 10.0
        ),
        sliderInput("year", "Year", min(illdata$yr), max(illdata$yr),
                    value = range(illdata$yr), step = 1.0, sep = ""
        ),
        checkboxGroupInput("magnitude", 
                           h3("Magnitudes to Filter by"), 
                           choices = list("Magnitude 0" = 0, 
                                          "Magnitude 1" = 1, 
                                          "Magnitude 2" = 2, 
                                          "Magnitude 3" = 3,
                                          "Magnitude 4" = 4, 
                                          "Magnitude 5" = 5),
                           selected = c(0, 1, 2, 3, 4, 5))
        

      )
    )
  ),


  conditionalPanel("false", icon("crosshair"))
)
