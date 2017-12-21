library(shiny)
library(dplyr)
library(lubridate)
library(shinyAce)
library(ggplot2)  # for the diamonds dataset
library(googlesheets)
library(DT)
library(shinydashboard)
library(ggmap)
library(httr)
library(purrr)
library(dplyr)
library(rvest)
library(stringr)
library(mailR)
library(leaflet)


# citymap <- gs_new("citymap", input = head(iris, 3), trim = TRUE)

gmap <- gs_title("citymap")
# winner = data.frame(Game = c("Celebration Bowl", "New Orleans Bowl", "Cure Bowl", "Las Vegas Bowl","New Mexico Bowl","Camellia Bowl", "Cheribundi Tart Cherry Boca Raton Bowl", "DXL Frisco Bowl", "Bad Boy Mowers Gasparilla Bowl", "Bahamas Bowl", "Famous Idaho Potato Bowl", "Birmingham Bowl", "Lockheed Martin Armed Forces Bowl", "Dollar General Bowl", "Hawai'i Bowl", "Zaxby's Heart of Dallas Bowl", "Quick Lane Bowl", "Cactus Bowl", "Walk-On's Independence Bowl", "New Era Pinstripe Bowl", "Foster Farm's Bowl", "Academy Sports + Outdoors Texas Bowl"),
                    # Winner = c("North Carolina A&T", "Troy", "Georgia State", "Boise State", "Marshall", "Middle Tennessee", " "," "," ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "),
                    # Away = c("Grambling", "Troy", "Western Kentucky", "Boise State", "Marshall", "Middle Tennessee", "Akron", "Louisiana Tech", "Temple", "UAB", "Central Michigan", "Texas Tech", "San Diego State", "Appalachian State", "Fresno State", "Utah","Duke", "Kansas State","Southern Miss", "Iowa", "Arizona", "Texas"),
                    # Home = c("North Carolina A&T", "North Texas", "Georgia State", "Oregon", "Colorado State", "Arkansas State", "Florida Atlantic", "SMU", "Florida International", "Ohio", "Wyoming", "South Florida", "Army","Toledo", "Houston","West Virginia", "Northern Illinios", "UCLA", "Florida State", "Boston College", "Purdue", "Texas")
# )

# http://fontawesome.io/icons/
# Define UI for slider demo app ----
ui <- dashboardPage(skin = "yellow",
                    # App title ----
                    dashboardHeader(title = "MA256 City Population Lookup"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("showmap", tabName = "showmap", icon = icon("newspaper-o")),
                        menuItem("showthemap", tabName = "showthemap", icon = icon("newspaper-o")),
                        menuItem("Information", tabName = "information", icon = icon("dashboard"), startExpanded = TRUE, 
                                 textInput("city", label = "City", value = "Mason"),
                                 textInput("state", label = "State", value = "Texas")),
                                 actionButton("Search", "Search"),
                                 actionButton("Upload", "Upload")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "showmap",
                                tags$hr(),
                                tableOutput("citytable")),
                        tabItem(tabName = "showthemap",
                                leafletOutput("map")
                                # tableOutput("citytable")
                                )
                                    )
                                )
                        )





server <- function(input, output) {
  
  # get lat long
  # values <- reactiveValues()
  # emails = reactiveValues()
  
  # emails$df = data.frame(emailvector = numeric(0))
  # values$df <- data.frame(City = numeric(0), State = numeric(0), pop = numeric(0), lat = numeric(0), long = numeric(0))
  
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
  
google <- eventReactive(input$Upload==TRUE, {
  helper = gs_read(gmap)
})  

    output$citytable <- renderTable({
      google()
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%  
        addTiles(options = tileOptions(maxZoom = 28, maxNativeZoom = 19),
                 group = 'OSM')
    })
      
  ##############
  
  # observeEvent(input$Search, {
  #   output$showmap = renderPlot({
  #     ggmap(map) +
  #       geom_point(data = tablevalues(),
  #                  aes(x = as.numeric(long), y = as.numeric(lat), colour = log(1/Population), alpha = 10, size = 5),
  #                  shape = 20 ) +
  #       # scale_colour_gradient(low = "white") 
  #       theme(legend.position="none") 
  #     #guides(fill = FALSE,alpha = TRUE,size = FALSE)
  #   }, height = 700, width = 1000)
  #   

  # })
  
#   sendemailto = observeEvent(input$Submit, {
#     isolate(emails$df[nrow(emails$df) + 1,] <- c(emailvector = input$emailaddress))
#     # isolate(emails$df$emailvector = emails$df$emailvector[-input$delete,])
#     # colnames(emails$df) = c("Cadet_Emails")
#     return(emails$df)
#   })
#   
#   observeEvent(input$Submit, {
#     output$emaillist <- renderTable({
#       emails$df
#       showitdf = emails$df
#       colnames(showitdf) = c("Cadet Emails")
#       return(showitdf)
#     })
#   })
#   
#   observeEvent(input$Sendemaila, {
#     wd = paste("C:/Users/", input$first, ".", input$last, "/Downloads", sep = "")
#     setwd(wd)
#     for(i in 1:length(emails$df$emailvector)){
#       send.mail(from="dusty.turner@usma.edu",
#                 to=as.character(emails$df$emailvector[i]),
#                 subject = input$subject,
#                 body = "This is and email from CPT Dusty Turner",
#                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "dusty.s.turner", passwd = "stewardesses", ssl = TRUE),
#                 authenticate = TRUE,
#                 send = TRUE,
#                 attach.files = "Class Populations.csv",
#                 debug = FALSE)
#     }
#   })
#   
#   observeEvent(input$Sendemailb, {
#     wd = paste("//usmaedu/apollo/math/Userdirs/", input$dirslocation, sep = "")
#     setwd(wd)
#     for(i in 1:length(emails$df$emailvector)){
#       send.mail(from="dusty.turner@usma.edu",
#                 to=as.character(emails$df$emailvector[i]),
#                 subject = input$subject,
#                 body = "This is and email from CPT Dusty Turner",
#                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "dusty.s.turner", passwd = "stewardesses", ssl = TRUE),
#                 authenticate = TRUE,
#                 send = TRUE,
#                 attach.files = "Class Populations.csv",
#                 debug = FALSE)
#     }
#   })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server", options = list(launch.browser = TRUE))