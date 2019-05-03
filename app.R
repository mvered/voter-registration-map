# Load libraries for app
library(shiny)
library(leaflet)
library(shinyWidgets)
library(dplyr)
library(DT)

# Define UI for app
ui <- fluidPage (
  
  # App title
  titlePanel("Voter Registration in Montana"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      checkboxGroupInput(inputId="Organizations",
                         label="Organizations",
                         choices = list("Forward MT",
                                   "Montana Women Vote",
                                   "MontPIRG",
                                   "Western Native Voice"),
                         selected = list("Forward MT",
                                         "Montana Women Vote",
                                         "MontPIRG",
                                         "Western Native Voice")
      ),
      
      # code to change slider bar display
      # fills on right, from selected value to max, rather than default of min to selection
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

    ')), 
      
      # creates slider bar input for frequency
      sliderTextInput(inputId="Frequency",
                  label="Frequency of Use",
                  grid=TRUE,
                  choices = c("Annually","Monthly","Weekly","Continuously")
      ),
      
      # creates checkbox input for type of VR location
      checkboxGroupInput(inputId="Types",
                         label="Location Types",
                         choices = list("Canvass",
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
  
  # table output
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
}

shinyApp(server=server, ui=ui)