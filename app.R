# Load libraries for app
library(shiny)
library(leaflet)
library(shinyWidgets)
library(dplyr)
library(DT)
library(shinythemes)
library(geojsonio)

# Define UI for app
ui <- fluidPage (theme=shinytheme("flatly"),
  
  # App title
  #titlePanel("Voter Registration in Montana"),
  
  navbarPage("Voter Registration in Montana",
  tabPanel("Planned VR Sites",
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # code to change slider bar display
      # fills on right, from selected value to max, rather than default fill of min to selection
      tags$head( tags$style( type = "text/css", '
      .irs-line-mid{
        background: #428bca ;
        border: 1px solid #428bca ;
      }
      .irs-line-right{
        background: #428bca ;
      }
      .irs-bar {
        background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
        border-top: 1px solid #CCC ;
        border-bottom: 1px solid #CCC ;
      }
      .irs-bar-edge {
        background: inherit ;
        border: inherit ;
      }

    '),
      
      # style for icons in organization check box input                      
      tags$style(".fmt {color:#EE9900}"),
      tags$style(".pirg {color:#41ABE2}"),
      tags$style(".mwv {color:#72AD00}"),
      tags$style(".wnv {color:#DA4521}")
      ), 
      
      
      checkboxGroupInput(inputId="Organizations",
                         label="Organizations",
                         choiceNames =
                           list(HTML(paste("Forward MT ",icon("map-marker",class="fmt",lib="font-awesome"))),
                              HTML(paste("Montana Women Vote ",icon("map-marker",class="mwv",lib="font-awesome"))),
                              HTML(paste("MontPIRG ",icon("map-marker",class="pirg",lib="font-awesome"))),
                              HTML(paste("Western Native Voice ",icon("map-marker",class="wnv",lib="font-awesome")))
                            ),
                         choiceValues = list("Forward MT",
                                   "Montana Women Vote",
                                   "MontPIRG",
                                   "Western Native Voice"),
                         selected = list("Forward MT",
                                         "Montana Women Vote",
                                         "MontPIRG",
                                         "Western Native Voice")
      ),
    
      
      # creates slider bar input for frequency
      sliderTextInput(inputId="Frequency",
                  label="Frequency of Use",
                  grid=TRUE,
                  choices = c("Annually","Monthly","Weekly","Continuously")
      ),
      
      # creates checkbox input for type of VR location
      checkboxGroupInput(inputId="Types",
                         label="Location Types",
                         choiceNames =
                           list(HTML(paste("Canvass ",icon("home",lib="font-awesome"))),
                                HTML(paste("Community Hot Spot ",icon("star",lib="font-awesome"))),
                                HTML(paste("Drop Box ",icon("fab fa-dropbox",lib="font-awesome"))),
                                HTML(paste("Event ",icon("calendar",lib="font-awesome"))),
                                HTML(paste("Office ",icon("briefcase",lib="font-awesome"))),
                                HTML(paste("School ",icon("graduation-cap",lib="font-awesome")))
                                ),
                         choiceValues = list("Canvass",
                                        "Community Hot Spot",
                                        "Drop Box",
                                        "Event",
                                        "Office",
                                        "School"),
                         selected = list("Canvass",
                                         "Community Hot Spot",
                                         "Drop Box",
                                         "Event",
                                         "Office",
                                         "School")
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Ouput: Tabset
      tabsetPanel(type='tabs',
        tabPanel("Map",leafletOutput(outputId="vrMap",height=550)),
        tabPanel("List of Locations",dataTableOutput(outputId="table"))
      )
  )
  )
  ),
  tabPanel("Registration Gaps",
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId="dataLayer",
                            label="Data to Visualize:",
                            choices=list("All Voters",
                                    "Young People",
                                     "Native Americans"))
             ),
             mainPanel(      # Ouput: Tabset
               tabsetPanel(type='tabs',
                           tabPanel("Map",leafletOutput(outputId="gapMap",height=550)),
                           tabPanel("Data",dataTableOutput(outputId="countyTable"))
                          )
                       )
           )
           
           
  ),id = "navbar"
  ),
  tags$footer("Created May 2019 by Michelle Vered for Montana Voices.")
)

# Define server logic for app
server <- function(input,output){
  
  # load in VR location data
  vrLocations <- read.csv("voter-reg-locations.csv")

  # Reactive expression to provide list of frequencies to subset to
  frequencyList <- reactive({
    if(input$Frequency=='Annually'){
      c("Annually","Monthly","Weekly","Continuously")
    }
    else if(input$Frequency=='Monthly'){
      c("Monthly","Weekly","Continuously")
    }
    else if(input$Frequency=='Weekly'){
      c("Weekly","Continuously")
    }
    else{
      c("Continuously")
    }
  })
  
  # Reactive expression for data subsetted to what the user selected
  filteredData <- reactive({
    vrLocations[
      which(vrLocations$Organization %in% input$Organizations
            & vrLocations$Type %in% input$Types
            & vrLocations$Frequency %in% frequencyList()
              ), ]
  })
  
  # set icon color depending on organization
  getIconColor <- function(vrLocations) {
    sapply(vrLocations$Organization, function(Organization) {
      if(Organization == 'Forward MT'){
        "orange"
      } else if(Organization =='MontPIRG'){
        "blue"
      }
      else if(Organization=='Montana Women Vote'){
        "green"
      }
      else if(Organization=='Western Native Voice'){
        "yellow" 
      }
      else{
        "black"
      } })
  }
  
  # set icon type depending on type of VR location
  getIconType <- function(vrLocations) {
    sapply(vrLocations$Type, function(Type) {
      if(Type == 'Canvass'){
        "home"
      } else if(Type =='Community Hot Spot'){
        "star"
      }
      else if(Type=='Drop Box'){
        "dropbox"
      }
      else if(Type=='Event'){
        "calendar" 
      }
      else if(Type=='Office'){
        "briefcase" 
      }
      else if(Type=='School'){
        "graduation-cap" 
      }
      else{
        "exclamation-circle"
      } })
  }
  
  # generate VR location icons
  vrLocationIcons <- reactive({
    awesomeIcons(
      icon = getIconType(filteredData()),
      iconColor='black',
      library='fa',
      markerColor = getIconColor(filteredData())
    )})
  
  # table output for VR locations
  output$table = renderDataTable({select(vrLocations,
                                         Name,
                                         Address,
                                         City,
                                         Zip,
                                         Organization,
                                         Type,
                                         Frequency)},
                                 options = list(order=list(0,'asc')),
                                 rownames=FALSE)
  
  # observer to dynamically update vrLocations in table
  observe({
    proxytable <- dataTableProxy("table")
    proxytable %>% replaceData({select(filteredData(),
            Name,
            Address,
            City,
            Zip,
            Organization,
            Type,
            Frequency)},
            rownames=FALSE)
  })
  
  # generate map output for vr locations
  output$vrMap <-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -109.4282, lat = 47.0625, zoom = 6)
  })
  
  # observer to dynamically update vrLocations being displayed on map
  observe({
    proxymap <- leafletProxy("vrMap", data=filteredData())
    proxymap %>% clearMarkers()
    proxymap %>%  addAwesomeMarkers(
                          ~Longitude,~Latitude, 
                          icon=vrLocationIcons(),
                          popup= paste("<b>",
                          filteredData()$Name,
                          "</b><br>",
                          filteredData()$Address,
                          "<br>",
                          filteredData()$City,
                          "MT ",
                          filteredData()$Zip))
      
  })
  
  # load in county level voter registration and population data
  counties <- geojsonio::geojson_read("county-data-simplified.geojson",what="sp")
  
  # colors for map polygons for chloropleth rendering
  pal <- colorNumeric("viridis",NULL)
  
  # generate map output for vr gaps
  output$gapMap <-renderLeaflet({
    leaflet(counties) %>%
      addTiles() %>%
      setView(lng = -109.4282, lat = 47.0625, zoom = 6) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,fillColor=~pal(Active))
  })
  
  # observer to dynamically update gap data being displayed on map
 #  observe({
 #  proxygapMap <- leafletProxy("gapMap")
#   proxygapMap %>% addPolygons(fillColor=~pal(Active))})

  # table output for county data
  output$countyTable = renderDataTable(select(as.data.frame(counties),
                                              County,
                                              Active,
                                              Inactive,
                                              RegA_U35,
                                              CVAP,
                                              CVAP_Native_LOW,
                                              CVAP_Native_HIGH,
                                              VAP_U35_LOW,
                                              VAP_U35_HIGH),
                                 server=FALSE,
                                 extensions = c("Buttons"),
                                 options = list(order=list(0,'asc'),
                                                dom='Bfrtip',
                                                buttons=c('copy','csv','excel','pdf','print')),
                                                colnames =c('County',
                                                            'Registered Active',
                                                            'Registered Inactive',
                                                            'Under 35 Registered Active',
                                                            'Citizen Voting-Age Population',
                                                            'CVAP Native American Lower Bound',
                                                            'CVAP Native American Upper Bound',
                                                            'CVAP Under 35 Lower Bound',
                                                            'CVAP Under 35 Upper Bound'),
                                 rownames=FALSE)
  
}

shinyApp(server=server, ui=ui)