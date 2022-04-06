library(ggvis)
library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)
library(viridis)
library(sf)
library(sp)
library(htmlwidgets)
library(readr)

if(FALSE) {
  library(RSQLite)
  library(dbplyr)
}

comp <- read_csv('comp.csv')

all_ecosystems <- comp

function(input, output, session) {
  
  
  # Filter the ecosystems, returns a data frame
  ecosystems <- reactive({
    difference <- input$diff * 1000000
    minCurAcres <- input$curAcres[1] * 1e6
    maxCurAcres <- input$curAcres[2] * 1e6
    minPer <- input$perChange[1]
    maxPer <- input$perChange[2]
    
    # Apply filters
    e <- all_ecosystems %>%
      filter(
        DIFF >= difference,
        ACRES_CURR >= minCurAcres,
        ACRES_CURR <= maxCurAcres,
        PER_CHANGE >= minPer,
        PER_CHANGE <= maxPer
      ) %>%
      arrange(ACRES_CURR)
    
    e <- as.data.frame(e)
    
    e
    
  })
  
  # Function for generating tooltip text
  tooltip <- function(x) {
    
    # Pick out the ecosystem with this name
    all_ecosystems <- isolate(ecosystems())
    ecosystem <- all_ecosystems[all_ecosystems$CLASSNAME == x$CLASSNAME, ]
    
    paste0("<div style = 'color:#000000'><b>", ecosystem$CLASSNAME, " </b><br>",
           format(ecosystem$ACRES_CURR, big.mark = ",", scientific = FALSE), " current acres,",
           "<br>", format(ecosystem$ACRES_HIST, big.mark = ",", scientific = FALSE), " historical acres,",
           "<br>", format(ecosystem$DIFF, big.mark = ",", scientific = FALSE), " difference in acres."
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    
    xvar <- prop("x", as.symbol("ACRES_CURR"))
    yvar <- prop("y", as.symbol("DIFF"))
    
    ecosystems %>%
      ggvis(x = xvar, y = yvar, fill = ~PER_CHANGE) %>%
      layer_points(size := 50, size.hover := 200,
                   fill.hover := 0.5,
                   key := ~CLASSNAME) %>%
      add_tooltip(tooltip, "hover") %>%
      scale_numeric("fill",domain = c(0, 100), range = c("white", "red")) %>%
      add_axis("x", title = "Current Acres") %>%
      add_axis("y", title = "Difference in Acres Current vs Historical",
               title_offset = 80) %>%
      add_legend("fill", title = "Percent Change") %>%
      set_options(width = "auto", height = "50%")
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_ecosystems <- renderText({ nrow(ecosystems()) })
  
  
  
  
  
  ################################################################
  
  
  
  
  #CREATING MAP
  
  leafMap <- st_read(dsn = 'leafMap.shp', stringsAsFactors = FALSE)
  
  bin <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  pal <- colorNumeric(palette = "viridis", domain = range(0,100), reverse = FALSE, na.color = "red")
  
  shinyMapData <- leafMap
  
  output$shinyMap <- renderLeaflet({
    
    
    # making label
    lab = sprintf("<strong>%s County</strong><br/>%g%% Natural <br/>%s Natural Acres", 
                  shinyMapData$NAME10, 
                  shinyMapData$PERCENT_N, 
                  format(shinyMapData$ACRES_N, big.mark = ",", scientific = FALSE)) %>%
      lapply(HTML)
    legendTitle = "Percent of Natural Acres"
    
    map <- leaflet(data = shinyMapData, options = leafletOptions(minZoom = 4, maxZoom = 9)) %>%
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
      setView(lng = -97.5,
              lat = 38,
              zoom = 5) %>%
      setMaxBounds(lng1 = -145,
                   lat1 = 55,
                   lng2 = -51,
                   lat2 = 18) %>%
      addPolygons(weight = 1,
                  smoothFactor = 0.02,
                  fillOpacity = 0.9,
                  color = ~pal(PERCENT_N),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = lab,
                  labelOptions =labelOptions(textsize = "15px")) %>%
      addLegend(pal = pal,
                values = ~PERCENT_N,
                bins = bin,
                opacity = 0.7,
                title = "Percent of Acres Natural",
                position = "bottomright")
  })
  
  observe({
    
    #temp variables to select data from
    selected <- input$colorBy
    
    #make the value lab exist
    # lab
    
    legendTitle = ""
    
    #rename selected columns and create selected labels
    if(selected == 1){
      #if they selected natural
      colnames(shinyMapData)[colnames(shinyMapData)=="PERCENT_N"] <- "PERCENT"
      lab = sprintf("<strong>%s County</strong><br/>%g%% Natural <br/>%s Natural Acres", 
                    shinyMapData$NAME10, 
                    shinyMapData$PERCENT, 
                    format(shinyMapData$ACRES_N, big.mark = ",", scientific = FALSE)) %>%
        lapply(HTML)
      legendTitle = "Percent of Natural Acres"
      
    } else if(selected == 2){
      #if they selected Agriculture
      colnames(shinyMapData)[colnames(shinyMapData)=="PERCENT_A"] <- "PERCENT"
      lab = sprintf("<strong>%s County</strong><br/>%g%% Agriculture <br/>%s Acres of Agriculture", 
                    shinyMapData$NAME10, 
                    shinyMapData$PERCENT, 
                    format(shinyMapData$ACRES_A, big.mark = ",", scientific = FALSE)) %>%
        lapply(HTML)
      legendTitle = "Percent of Agricultural Acres"
      
    } else{
      #if they selected urban
      colnames(shinyMapData)[colnames(shinyMapData)=="PERCENT_U"] <- "PERCENT"
      lab = sprintf("<strong>%s County</strong><br/>%g%% Urban <br/>%s Urban Acres", 
                    shinyMapData$NAME10, 
                    shinyMapData$PERCENT, 
                    format(shinyMapData$ACRES_U, big.mark = ",", scientific = FALSE)) %>%
        lapply(HTML)
      legendTitle = "Percent of Urban Acres"
    }
    
    
    
    
    
    leafletProxy("shinyMap", data = shinyMapData) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(weight = 1,
                  fillOpacity = 0.9,
                  color = ~pal(PERCENT),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = lab,
                  labelOptions =labelOptions(textsize = "15px")) %>%
      addLegend(pal = pal,
                values = ~PERCENT,
                bins = bin,
                opacity = 0.7,
                title = legendTitle,
                position = "bottomright")
  })
  
  
}