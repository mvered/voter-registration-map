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
               selectInput(inputId="dataCategory",label="I want to learn more about...",
                           choices=list("All Registered/Unregistered Voters",
                                            "Native American Voter Registration",
                                            "Youth Voter Registration"),
                           selected="All"),
               radioButtons(inputId="dataLayer",
                            label="Show me:",
                            choiceValues = list("Active",
                                               "Share_CVAP_RegA",
                                               "Unreg_HIGH",
                                               "Share_Absentee",
                                               "CVAP_Native_HIGH",
                                               "CVAP_Share_Native_HIGH",
                                               "Unreg_Native_HIGH",
                                               "VAP_U35_HIGH",
                                               "VAP_Share_U35_HIGH",
                                               "Youth_RegA_Share",
                                               "Youth_RegI_Share",
                                               "Gap_Share_U35_HIGH",
                                               "NotRegA_U35_High",
                                               "Youth_Share_Absentee"),
                            choiceNames = list("Total Registered Voters",
                                               "Share of People Registered to Vote",
                                               "Unregistered or Inactive Citizen Voting-Age Population",
                                               "Share of Registered Voters on the Permanent Absentee List",
                                               "Total Native American Population",
                                               "Native American Share of Population",
                                               "Unregistered or Inactive Native Americans",
                                               "Total Youth Voting-Age Population",
                                               "Youth Share of Voting-Age Population",
                                               "Youth Share of Registered Active Voters",
                                               "Youth Share of Registered Inactive Voters",
                                               "Youth Voter Registration Gap",
                                               "Unregistered or Inactive Young People",
                                               "Share of Youth on the Permanent Absentee List"
                                               ),
                            selected = "Active")),
             mainPanel(      # Ouput: Tabset
               tabsetPanel(type='tabs',
                           tabPanel("Map",leafletOutput(outputId="gapMap",height=550)),
                           tabPanel("Data",dataTableOutput(outputId="countyTable")),
                           tabPanel("Notes on Data",
                                    HTML("<br><br><b><i>"),
                                    "Note 1: ",
                                    HTML("</i></b>"),
                                    "CVAP = Citizen Voting-Age Population",
                                    HTML("<br><br><b><i>"),
                                    "Note 2: ",
                                    HTML("</i></b>"),
                                    "Young People = Ages 18 to 34 (for the purposes of this analysis)",
                                    HTML("<br><br><b><i>"),
                                    "Note 3: ",
                                    HTML("</i></b>"),
                                    "Voter registration numbers (except for the number of registered Native American voters) come from voter file data provided by the Montana Secretary of State's  Office as of 5/1/2019. ",
                                    HTML("<br><br><b><i>"),
                                    "Note 4: ",
                                    HTML("</i></b>"),
                                    "Population estimates (i.e. estimates of CVAP) come from the American Community Survey from the US Census bureau. 
               Due to small sample sizes, for sub-populations such as young people or Native Americans, there is some uncertainty in these estimates. 
               Because this, each data layer showing population data for Native Americans or young people is presented using lower and upper bounds for the population size, rather than a single number.
                                    The upper and lower bounds represent a 90% confidence interval for the population estimate provided by the Census bureau.",
                                    HTML("<br><br><b><i>"),
                                    "Note 5: ",
                                    HTML("</i></b>"),
                                    "Estimating the number of registered and unregistered Native Americans is challenging because of limitations on the data. 
                                    These estimates, even when compared with population-level estimates of CVAP from the Census Bureau, contain a lot of uncertainty and should be taken
                                    as extremely rough estimates, rather than numbers presented with a strong degree of precision. 
                                    Unlike age, race is not listed on the voter file provided by the Secretary of State in Montana. In the absence of that information,
                                    the number of registered voters in the state who are Native American is estimated using Catalist race models. These models
                                    are known to be flawed. Comparing Catalist race models with self-reported race and ethncitity data collected by Montana Voices partners,
                                    only about half of self-identified Native Americans were modeled to be Native American by the Catalist models. Most of the people who self-identified as
                                    Native American but who were not identified that way by the Catalist race models were identified as white by the Catalist models.
                                    In order to try to be more realistic about the degree of uncertainty in these estimates, the number of Native Americans who are registered to vote in each county
                                    was calculated twice. First, using the Catalist models only. Second, using the self-reported data as a guide and doubling the number of 
                                    Native American registered voters predicted by the Catalist models. Both these numbers were then compared with the upper and lower bounds from the Census bureau's population
                                    estimates of the number of citizen voting-age Native Americans. Clicking on each county will display a range of estimates for the number of unregisted people who are Native American,
                                    from the lowest number to the highest number that were produced using those series of calculations.
                                    "
                                    
                                    
                                    )
                          )
                       )
           )
           
           
  ),id = "navbar"
  ),
  tags$footer("Created May 2019 by Michelle Vered for Montana Voices.")
)

# Define server logic for app
server <- function(input,output,session){
  
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
  
  # dynamically update choices for dataLayers
  observeEvent(input$dataCategory,{
    selectedChoiceNames <- { 
      if(input$dataCategory=='All Registered/Unregistered Voters'){
        list("Total Registered Voters",
             "Share of People Registered to Vote",
             "Unregistered or Inactive Citizen Voting-Age Population",
             "Share of Registered Voters on the Permanent Absentee List")
      }
      else if(input$dataCategory=='Native American Voter Registration'){
        list("Total Native American Population",
             "Native American Share of Population",
             "Unregistered or Inactive Native Americans")
      }
      else if(input$dataCategory=='Youth Voter Registration'){
      list("Total Youth Voting-Age Population",
           "Youth Share of Voting-Age Population",
           "Youth Share of Registered Active Voters",
           "Youth Share of Registered Inactive Voters",
           "Youth Voter Registration Gap",
           "Unregistered or Inactive Young People",
           "Share of Youth on the Permanent Absentee List")
      }
    }

    selectedChoiceValues <- { 
        if(input$dataCategory=='All Registered/Unregistered Voters'){
          list("Active",
               "Share_CVAP_RegA",
               "Unreg_HIGH",
               "Share_Absentee")
        }
        else if(input$dataCategory=='Native American Voter Registration'){
          list("CVAP_Native_HIGH",
               "CVAP_Share_Native_HIGH",
               "Unreg_Native_HIGH")
        }
        else if(input$dataCategory=='Youth Voter Registration'){
          list("VAP_U35_HIGH",
               "VAP_Share_U35_HIGH",
               "Youth_RegA_Share",
               "Youth_RegI_Share",
               "Gap_Share_U35_HIGH",
               "NotRegA_U35_High",
               "Youth_Share_Absentee")
        }
      }

    updateRadioButtons(session, inputId="dataLayer", choiceValues = selectedChoiceValues, choiceNames = selectedChoiceNames)
  })
  
  
  # load in county level voter registration and population data
  counties <- geojsonio::geojson_read("county-data-simplified.geojson",what="sp")
  
  # colors for map polygons for chloropleth rendering
  pal <- colorNumeric("Purples",NULL)
  
  # reactive map polygons
  countiesFiltered <- reactive({
    if(input$dataLayer=='Active'){
      counties[,c("Active","Inactive")]
    }
    else if(input$dataLayer=='CVAP'){
      counties[,c("CVAP","Total_Registered")]
    } 
    else if(input$dataLayer=='Share_CVAP_RegA'){
      counties[,c("Share_CVAP_RegA","CVAP")]
    }
    else if(input$dataLayer=='CVAP_Native_HIGH'){
      counties[,c("CVAP_Native_HIGH","CVAP_Native_LOW")]
    }
    else if(input$dataLayer=='CVAP_Share_Native_HIGH'){
      counties[,c("CVAP_Share_Native_HIGH","CVAP_Share_Native_LOW")]
    }
    else if(input$dataLayer=='VAP_U35_HIGH'){
      counties[,c("VAP_U35_HIGH","VAP_U35_LOW")]
    }
    else if(input$dataLayer=='VAP_Share_U35_HIGH'){
      counties[,c("VAP_Share_U35_HIGH","VAP_Share_U35_LOW")]
    }
    else if(input$dataLayer=='NotRegA_U35_High'){
      counties[,c("NotRegA_U35_High","NotRegA_U35_LOW")]
    }
    else if(input$dataLayer=='Unreg_HIGH'){
      counties[,c("Unreg_HIGH","Unreg_LOW")]
    }
    else if(input$dataLayer=='Share_Absentee'){
      counties[,c('Share_Absentee','Active','Inactive')]
    }
    else if(input$dataLayer=='Youth_RegA_Share'){
      counties[,c('Youth_RegA_Share','VAP_Share_U35_LOW','VAP_Share_U35_HIGH')]
    }
    else if(input$dataLayer=='Youth_RegI_Share'){
      counties[,c('Youth_RegI_Share','VAP_Share_U35_LOW','VAP_Share_U35_HIGH')]
    }
    else if(input$dataLayer=='Gap_Share_U35_HIGH'){
      counties[,c('Gap_Share_U35_HIGH','Gap_Share_U35_LOW')]
    }
    else if(input$dataLayer=='Youth_Share_Absentee'){
      counties[,c('Youth_Share_Absentee','Share_Absentee')]
    }
    else if(input$dataLayer=='Unreg_Native_HIGH'){
      counties[,c('Unreg_Native_HIGH','Unreg_Native_LOW')]
    }
    })
  
  # generate map output for vr gaps
  output$gapMap <-renderLeaflet({
    leaflet(counties) %>%
      addTiles() %>%
      setView(lng = -109.4282, lat = 47.0625, zoom = 6) %>%
      addPolygons(weight = 2,
                  opacity = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  fillColor=~pal(Active), 
                  highlight = highlightOptions(
                    weight = 5,
                    color="#666",
                    bringToFront=TRUE),
                  label=counties$County,
                  popup=paste("<b>",
                              counties$County,
                              " County</b>",
                              "<br><i>Registered Active: </i>",
                              formatC(countiesFiltered()[[1]],format="d",big.mark=","),
                              "<br><i>Registered Inactive: </i>",
                              formatC(countiesFiltered()[[2]],format="d",big.mark=","),
                              sep='')) %>%
      addLegend("bottomright", 
                    pal = pal, 
                    values = ~Active,
                    title = "Active",
                    opacity=1)  
  })
  
  # dynamic labels
  countyPopups <- reactive({ 
    if(input$dataLayer=='CVAP'){
      paste("<b>",
            counties$County,
            " County</b>",
            "<br><i>Total CVAP: </i>",
            countiesFiltered()[[1]],
            sep=''
      )
    }
    else if(input$dataLayer=='Active'){
      paste("<b>",
            counties$County,
            " County</b>",
            "<br><i>Registered Active: </i>",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            "<br><i>Registered Inactive: </i>",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            sep=''
      )
    } 
    else if(input$dataLayer=='Share_CVAP_RegA'){
      paste("<b>",
            counties$County,
            " County</b>",
            "<br>Est. ",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of citizens 18+ are registered active",
            sep=''
      )
    }
    else if(input$dataLayer=='CVAP_Native_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Between ",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            " to ",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            " Native American citizens 18+",
            sep='')
    }
    else if(input$dataLayer=='CVAP_Share_Native_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Between ",
            round((countiesFiltered()[[2]])*100,digits=0),
            "-",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of citizens 18+ are Native American",
            sep='')
    }
    else if(input$dataLayer=='VAP_U35_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Between ",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            " to ",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            " citizens 18 to 34",
            sep='')
    }
    else if(input$dataLayer=='VAP_Share_U35_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Between ",
            round((countiesFiltered()[[2]])*100,digits=0),
            "-",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of voting-age citizens are 18-34",
            sep='')
    }
    else if(input$dataLayer=='NotRegA_U35_High'){
      paste("<b>",
            counties$County,
            " County</b><br>Est. ",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            " to ",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            " unregistered/inactive 18-34 year-olds",
            sep='')
    }
    
    else if(input$dataLayer=='Unreg_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Est. ",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            " to ",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            " unregistered/inactive citizens 18+",
            sep='')
    }
    else if(input$dataLayer=='Share_Absentee'){
      paste("<b>",
            counties$County,
            " County</b><br> ",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of registered voters are on the permanent absentee list <br>",
            formatC(((countiesFiltered()[[2]])+(countiesFiltered()[[3]]))*countiesFiltered()[[1]],format="d",big.mark=","),
            " total registered absentee voters",
            sep='')
    }
    else if(input$dataLayer=='Youth_RegA_Share'){
      paste("<b>",
            counties$County,
            " County</b><br>",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of registered <i>active</i> voters are 18-34",
            "<br> By comparison, young people make up ",
            round((countiesFiltered()[[2]])*100,digits=0),
            "-",
            round((countiesFiltered()[[3]])*100,digits=0),
            "% of voting-age citizens",
            sep='')
    }
    else if(input$dataLayer=='Youth_RegI_Share'){
      paste("<b>",
            counties$County,
            " County</b><br>",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of registered <i>inactive</i> voters are 18-34",
            "<br> By comparison, young people make up ",
            round((countiesFiltered()[[2]])*100,digits=0),
            "-",
            round((countiesFiltered()[[3]])*100,digits=0),
            "% of voting-age citizens",
            sep='')
    }
    else if(input$dataLayer=='Gap_Share_U35_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>",
            round((countiesFiltered()[[2]])*100,digits=0),
            "-",
            round((countiesFiltered()[[1]])*100,digits=0),
            "percentage point youth voter registration gap",
            sep='')
    }
    else if(input$dataLayer=='Youth_Share_Absentee'){
      paste("<b>",
            counties$County,
            " County</b><br>",
            round((countiesFiltered()[[2]])*100,digits=0),
            "% of young registered voters are signed up to vote absentee",
            "<br>By comparison, ",
            round((countiesFiltered()[[1]])*100,digits=0),
            "% of all registered voters are signed up to vote absentee",
            sep='')
    }
    else if(input$dataLayer=='Unreg_Native_HIGH'){
      paste("<b>",
            counties$County,
            " County</b><br>Est. ",
            formatC(countiesFiltered()[[2]],format="d",big.mark=","),
            " to ",
            formatC(countiesFiltered()[[1]],format="d",big.mark=","),
            " unregistered/inactive Native American citizens 18+",
            sep='')
    }
    })
  
  #observer to dynamically update gap data being displayed on map
  observeEvent(input$dataLayer, {
    proxyGapMap <- leafletProxy("gapMap",data=countiesFiltered()) 
    proxyGapMap %>% clearControls()
    proxyGapMap %>% clearPopups()
    proxyGapMap %>% 
      addPolygons(weight = 2,
                  opacity = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  fillColor=~pal(countiesFiltered()[[1]]), 
                  highlight = highlightOptions(
                    weight = 5,
                    color="#666",
                    bringToFront=TRUE),
                  label=counties$County,
                  popup=countyPopups()
                             )         
                
    proxyGapMap %>% addLegend("bottomright", 
                  pal = pal, 
                  values = ~countiesFiltered()[[1]],
                  title = names(countiesFiltered()[1]))
    })

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