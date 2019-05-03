# Load libraries for app
library(shiny)
library(leaflet)
library(shinyWidgets)

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
      sliderTextInput(inputId="Frequency",
                  label="Frequency of Use",
                  grid=TRUE,
                  choices = c("Annually","Monthly","Weekly","Continuously")
        
      ),
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
        tabPanel("Placeholder",textOutput(outputId="test"))
      )
  )
  )
)

# Define server logic for app
server <- function(input,output){
  
  # load in VR location data
  vrLocations <- read.csv("voter-reg-locations.csv")
  
  # Reactive expression for data subsetted to what the user selected
  filteredData <- reactive({
    vrLocations[vrLocations$Organization %in% input$Organizations, ]
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
  
  # test
  output$test = renderPrint({filteredData()})
  
  # generate map output for vr locations
  output$vrMap <-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -109.4282, lat = 47.0625, zoom = 6)
  })
  
  # observer to dynamically update vrLocations being displayed
  observe({
    proxy <- leafletProxy("vrMap", data=filteredData())
    proxy %>% clearMarkers()
    proxy %>%  addAwesomeMarkers(
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