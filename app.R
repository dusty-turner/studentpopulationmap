if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages("httr")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")}
if("shinyAce" %in% rownames(installed.packages()) == FALSE) {install.packages("shinyAce")}
if("shinyAce" %in% rownames(installed.packages()) == FALSE) {install.packages("shinyAce")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("googlesheets" %in% rownames(installed.packages()) == FALSE) {install.packages("googlesheets")}
if("DT" %in% rownames(installed.packages()) == FALSE) {install.packages("DT")}
if("shinydashboard" %in% rownames(installed.packages()) == FALSE) {install.packages("shinydashboard")}

library(shiny)
library(tidyverse)
library(httr)
library(rvest)
library(leaflet)
library(shinyAce)
library(ggplot2)
library(googlesheets)
library(DT)
library(shinydashboard)

gmap <- gs_title("citymap")

ui <- dashboardPage(skin = "yellow",
                    # App title ----
                    dashboardHeader(title = "MA256 City Population Lookup", titleWidth = 350),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Table View", tabName = "poptable", icon = icon("table")),
                        menuItem("Map View", tabName = "showthemap", icon = icon("map-marker")),
                        menuItem("Information", tabName = "information", icon = icon("info-circle"), startExpanded = TRUE, 
                                 textInput("city", label = "City", value = "Kerrville"),
                                 textInput("state", label = "State", value = "Texas")),
                                 actionButton("Search", "Search"),
                                 tags$hr(),
                                 actionButton("Update", "Update")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "poptable",
                                tableOutput("citysummary"),
                                tableOutput("citytable"),
                                plotOutput("plot")),
                        tabItem(tabName = "showthemap",
                                tags$hr(),
                                leafletOutput("map")
                                # tableOutput("citytable")
                                )
                                    )
                                )
                        )

server <- function(input, output) {
  
  newEntry <- observeEvent(input$Search,{
    cleanedcity = gsub(" ", "%20", input$city)
    cleanedstate = input$state
    cleanedstate = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(cleanedstate), perl=TRUE)
    test = paste("http://nominatim.openstreetmap.org/search?city=", cleanedcity,
                 "&countrycodes=US&limit=9&format=json", sep="")
    r <- GET(test)
    mysearch = content(r)
    lat = map_chr(mysearch, "lat")
    long = map_chr(mysearch, "lon")
    state = map_chr(mysearch, "display_name")
    id = map_chr(mysearch, "place_id")
    citydf = data.frame(lat = as.numeric(as.character(lat)), long = as.numeric(as.character(long)), state = state, id = id)
    thiscity = citydf[grep(cleanedstate, citydf$state), ]
    
    if(is.na(as.numeric(thiscity[1,4]))==FALSE) {
      thiscity = thiscity[1,]
      lat1 = thiscity$lat
      long1 = thiscity$long
      a <- paste(
        "http://nominatim.openstreetmap.org/details.php?place_id="
        , as.character(thiscity$id)
        , sep="")
      scrape <- a %>%
        read_html() %>%
        html_nodes(css = "tr+ tr .line") %>%
        html_text()
      if(length(scrape >= 2)) {
        for (i in 1:length(scrape)) {
          a = str_detect(scrape[i], "population")
          if (a == TRUE) {
            break()
          }
        }
        pop= gsub("\\D","",scrape[i])
        # newLine <- isolate(c(input$city, input$state, pop))
        cleanedcity2 = input$city
        cleanedcity2 = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(cleanedcity2), perl=TRUE)
        helper = data.frame(city = cleanedcity2, state = cleanedstate, pop = pop, lat = as.numeric(as.character(lat1)), long = as.numeric(as.character(long1)))
        gmap %>% gs_add_row(input = helper)
        # isolate(values$df[nrow(values$df) + 1,] <- c(cleanedcity2, cleanedstate, pop, lat = lat1, long = long1))
        # isolate(string$df[1] = c("0"))

        } else { 
        # isolate(string$df="0")
      } }
    
  })
  
google <- eventReactive(input$Update==TRUE, {
  helper = gs_read(gmap)
})

output$citytable <- renderTable({
    google()
})

output$citysummary <- renderTable({
  google() %>%
    summarise(MeanPopulation = mean(Population, na.rm = TRUE), 
              MedianPopulation = median(Population, na.rm = TRUE),
              Mean10PCTTrim = mean(Population, trim = .05, na.rm = TRUE)
    )
})

output$plot = renderPlot({
  ggplot(data = google(), aes(y=0,x=Population)) +
    geom_point() +
    geom_point(aes(y=0,x=mean(Population, na.rm = TRUE), color = "red")) +
    geom_point(aes(y=0,x=median(Population, na.rm = TRUE), color = "green")) +
    geom_point(aes(y=0,x=mean(Population, na.rm = TRUE, trim = .05), color = "blue")) +
    ylab(NULL) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none")
  }, height = 100, width = 600
)
    
output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%  
    setView(lat=37, lng=-96, zoom = 4) 
})

observe({
 leafletProxy("map", data = google()) %>%
    clearMarkers() %>%
    addMarkers(lat = google()$Latitude, lng = google()$Longitude, popup = paste(google()$City, google()$State, "<br>Population:", google()$Population, sep = " ")) %>%
    addCircleMarkers(lat = mean(google()$Latitude), lng = mean(google()$Longitude), 
               popup = paste("The Mean Population:", round(mean(google()$Population,na.rm = TRUE), digits = 0), 
                             "<br>LAT:", 
                             round(mean(google()$Latitude,na.rm = TRUE), digits = 0),
                             "<br>LONG:", 
                             round(mean(google()$Longitude,na.rm = TRUE), digits = 0),
                             sep = " "),
               color = "green")
    
})

}

shinyApp(ui = ui, server = server, enableBookmarking = "server", options = list(launch.browser = TRUE))