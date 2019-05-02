# Load libraries for app
library(shiny)
library(leaflet)

# Define UI for app
ui <- fluidPage (
  
  # App title
  titlePanel("Voter Registration in Montana"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Ouput: Tabset
      tabsetPanel(type='tabs',
        tabPanel("Map",leafletOutput(outputId="vrMap",height=550)),
        tabPanel("Placeholder")
      )
  )
  )
)

# Define server logic for app
server <- function(input,output){
  
  # load in VR location data
  vrLocations <- read.csv("voter-reg-locations.csv")
  
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
  vrLocationIcons <- awesomeIcons(
    icon = getIconType(vrLocations),
    iconColor='black',
    library='fa',
    markerColor = getIconColor(vrLocations)
  )
  
  # generate map output for vr locations
  output$vrMap <-renderLeaflet({
    leaflet() %>%
    addTiles() %>% 
    addAwesomeMarkers(data=vrLocations,
               ~Longitude,~Latitude, 
               icon=vrLocationIcons,
               popup= paste("<b>",
                            vrLocations$Name,
                            "</b><br>",
                            vrLocations$Address,
                            "<br>",
                            vrLocations$City,
                            "MT ",
                            vrLocations$Zip)
               )
  })
}

shinyApp(server=server, ui=ui)