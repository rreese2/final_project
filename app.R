library(tidyverse)
library(magrittr)
library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(shinyjs)
library(shinydashboard)
library(rgdal)
library(rgeos)

# datasets to read in 
path <- 'repo_clone/Data-Viz-2019-Fall-master/FinalProject/'

parks <- read.csv(paste(path, 'Parks_Locations_and_Features.csv', sep = ''))
public <- read.csv(paste(path, 'Public_Facilities.csv', sep = ''))
abandoned <- sf::st_read(paste(path, 'Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp', sep = ''), stringsAsFactors = FALSE)
abandoned_short <- abandoned[-18]
cityc <- sf::st_read(paste(path, 'City_Council_Districts/City_Council_Districts.shp', sep = ''), stringsAsFactors = FALSE)
census <- sf::st_read(paste(path, '2010_CensusData/2010_CensusData.shp', sep = ''), stringsAsFactors = FALSE)
parktype <- parks$Park_Type
lib <- public %>% filter(POPL_TYPE == "LIBRARY")
fire <- public %>% filter(POPL_TYPE == "FIRE STATION")
police <- public %>% filter(POPL_TYPE == "POLICE STATION")

pal <- colorFactor(palette = c('blue', 'black', 'cyan', 'green', 'purple', 'red', 'pink', 'yellow'), domain = parktype)


#read in sp version of abandoned for geospatial calc
abandoned_sp <- readOGR(
  dsn=paste(path, "Abandoned_Property_Parcels/",sep=''), 
  layer = "Abandoned_Property_Parcels", 
  stringsAsFactors = FALSE
)

#read in sp version of city_c for geospatial calc
cityc_sp <- readOGR(
  dsn=paste(path, "City_Council_Districts/",sep=''),
  layer = "City_Council_Districts", 
  stringsAsFactors = FALSE
)

#count how many adandoned properties are within the council district
aband_in_c_dist <- colSums(gContains(cityc_sp, abandoned_sp, byid = TRUE))
aband_in_c_dist_counts <- setNames(aband_in_c_dist, cityc_sp@data$Council_Me)

aband_in_c_dist_counts <- data.frame(aband_in_c_dist_counts)
names(aband_in_c_dist_counts) <- c('abandoned_properties')
aband_in_c_dist_counts %<>% rownames_to_column("Council_Me")

cityc <- full_join(cityc, aband_in_c_dist_counts, by = "Council_Me")


# dashboardpage is the outline of of our UI
ui <- dashboardPage(
  dashboardHeader(title = "Mayor Pete Dashboard"),
  dashboardSidebar(
    
    
    HTML('<h4>&nbsp;&nbsp;Notre Dame Data Science</h4>
         <br/>
         <h4>&nbsp;Colin Best</h4>
         <h4>&nbsp;Kirk Reese</h4>
         <h4>&nbsp;Spencer Poodiak-Parsons</h4>
         <h4>&nbsp;Tim Whelan</h4><br/>
         <h4>&nbsp;Class of 2020</h4><br/>
         <p>&nbsp; This Dashboard was created to enable comparison of needed city improvements by council district. The government official that uses it can tab between improvement types to see how districts compare. Click on districts to learn who the representative is, and to observe additional details. </p>
         
         ')
    
    ),
  dashboardBody(
    useShinyjs(),
    
    mainPanel(
      titlePanel("District Improvement Analytics"),
      
      tabsetPanel( type = "tabs",
                   # renders each of our leaflet maps that are created within the server 
                   tabPanel("Public Facilities",
                            leafletOutput("mymap")),
                            
                   tabPanel("Parks", 
                            leafletOutput("mymap1")),
                   
                   tabPanel("Abandoned Properties",
                            leafletOutput("mymap2")), 
                   
                   tabPanel("Data",
                            
                            # Input: Select a dataset ----
                            selectInput("dataset", "Choose a dataset:",
                                        choices = c("public", "parks", "abandoned")),
                            
                            # Include clarifying text ----
                            helpText("Please click Update View after selecting a new dataset"),
                            
                            # Input: actionButton() to defer the rendering of output ----
                            # until the user explicitly clicks the button (rather than
                            # doing it immediately when inputs change). This is useful if
                            # the computations required to render output are inordinately
                            # time-consuming.
                            actionButton("update", "Update View"), 
                            
                            
                            # Main panel for displaying outputs ----
                            
                            # Output: Header + summary of distribution ----
                            # h4("Summary"),
                            #  verbatimTextOutput("summary"),
                            
                            # Output: Header + table of distribution ----
                            h4("Observations"),
                            DT::dataTableOutput('view')
                            
                            
                            
                   )
                   
                   
                   
      )
    )
    # add more to body after this 
    
    
  )
)

server <-function(input, output) {
  observeEvent(input$showSidebar, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  output$mymap <- renderLeaflet({
    leaflet::leaflet()  %>%
      addTiles()  %>%
      setView(lng = -86.2739, lat = 41.6809, zoom = 11) %>%
      addMarkers(data = lib, popup = ~public$POPL_NAME, group  = "Library") %>%
      addMarkers(data = fire, popup = ~public$POPL_NAME, group = "Fire Station") %>%
      addMarkers(data = police, popup = ~public$POPL_NAME, group = "Police Station") %>%
      addPolygons(data = cityc, popup = ~cityc$Council_Me, color = "GREY", opacity = .9) %>%
      addProviderTiles(provider = "Esri",
                       group = "Terrain"
      ) %>%
      addProviderTiles(provider = "Esri.WorldImagery",
                       group = "Satellite"
      ) %>%
      addLayersControl(baseGroups = c("Streetmap", "Terrain", "Satellite"),
                       overlayGroups = c("Library", "Fire Station", "Police Station"),
                       options = layersControlOptions(collapsed = FALSE))
  
  
  })
  output$mymap1 <- renderLeaflet({
    leaflet::leaflet()  %>%
      addTiles()  %>%
      setView(lng = -86.2739, lat = 41.6809, zoom = 11) %>%
      addPolygons(data = cityc, popup = ~paste0("Council Member: ", Council_Me), fill = T, weight = 3, color = "GREY", group = "Council Districts", opacity = .9) %>%
      addCircleMarkers(data = parks, 
                       popup = ~paste0("Name: ",Park_Name, "<br/>Type: ", Park_Type, "<br/>Address: ", Address), 
                       color = ~pal(parktype), 
                       radius = 4, 
                       fillOpacity = 1
      ) %>%
      addLegend(position = "bottomleft",
                pal = pal, values = parktype,
                title = "Legend",
                opacity = 1
      ) %>%
      addProviderTiles(provider = "Esri",
                       group = "Terrain"
      ) %>%
      addProviderTiles(provider = "Esri.WorldImagery",
                       group = "Satellite"
      ) %>%
      addLayersControl(baseGroups = c("Streetmap", "Terrain", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  

  output$mymap2 <- renderLeaflet({
    leaflet::leaflet()  %>%
      addTiles()  %>%
      setView(lng = -86.2739, lat = 41.6809, zoom = 11) %>%
      addPolygons(data = abandoned, popup = ~Structures, color = "ORANGE", weight = 4, group='Properties') %>%
      addPolygons(data = cityc, popup = ~paste0("Council Member: ", Council_Me, "<br/>Abandoned Property Count: ", abandoned_properties), fill = T, weight = 3, color = "GREY", group = "Council Districts", opacity = .9) %>%
      addProviderTiles(provider = "Esri",
                       group = "Terrain"
      ) %>%
      addProviderTiles(provider = "Esri.WorldImagery",
                       group = "Satellite"
      ) %>%
      addLayersControl(baseGroups = c("Streetmap", "Terrain", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend(position = "bottomleft",
                colors = c('blue', 'orange'), labels = c('City Council Districts', 'Abandoned Property'),
                title = "Legend",
                opacity = .80)
  })
  
  
  # save this for new server funcitons 
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "public" = public, 
           "parks" = parks,
           "abandoned" = abandoned_short )
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  
  # -1 means no pagination; the 2nd element contains menu labels
  output$view <- DT::renderDataTable(
    DT::datatable(
      datasetInput(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
}

shinyApp(ui, server)

