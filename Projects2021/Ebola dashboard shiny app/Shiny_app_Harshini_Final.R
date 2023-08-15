
# setup -------------------------------------------------------------------

library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(imager)
library(shiny) 
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(tmap)
library(maps)
library(mapproj)
library(ggplot2)

# data --------------------------------------------------------------------

##Ebola data
ebola <- read_csv('data/raw/Ebola_Virus_Outbreak.csv')


ebolatable <- ebola %>% ## data transformation
  select( ##variable selection
    City = location, #variable names transformation
    Country =country, 
    Recordno = recno,
    Cases = casecum2,
    Casescount = Cumulative_cases, 
    Casedate = date_,
    latitude = Ycoord,
    longitude = Xcoord
  )

df <- ebolatable 

# Shiny app UI ------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Ebola Dashboard"),##title of the dashboard
  dashboardSidebar( 
    
    dateInput(
      "date1", "Date:", format = "dd-mm-yyyy",##date input
      value = min(ebola$date_),
      min = min(ebola$date_),#date range
      max = max(ebola$date_)#date range
      
    ),
    
    
    selectInput(
      inputId = 'state_select',##country input
      label = 'Country',
      choices = c('Select Country',unique(ebola$country))
    ),
    
    conditionalPanel("input.state_select",##county input
                     selectInput(  inputId = 'city_select', label = 'City', c("Select city"="") )
    ) , 
    
    actionButton("mapbutton", "update map"),##buttons
    
    sidebarMenu( ##sidebarmenu features defined
      menuItem(text = "Home", tabName = "abouttab", icon=icon("clipboard") ),
      menuItem('Map',tabName = 'mapstab', icon = icon('map')  ),
      
      menuItem('Table', tabName = 'tablestab',  icon = icon('table') ),
      
      menuItem('Static Map1', tabName = 'tabmap1',  icon = icon('table') ),
      menuItem('Static Map2', tabName = 'tabmap2',  icon = icon('table') ),
      menuItem('Static Map3', tabName = 'tabmap3',  icon = icon('table')  )
      
      
      
    )
    
  ),
  dashboardBody(
    
    tabItems( #text data about the dashboard
      
      tabItem(
        tabName = 'abouttab',
        h2('About'),
        p("This Ebola dashboard is created for the users to understand how  ebola cases were spread across various counties of Guinea,Liberia and Sierra Leone during the Ebola outbreak of 2014-16 period." ),
        p("This dashboard contains an interactive map that helps users navigate through the total Ebola cases in each county,along with three static maps of Sierra Leone showing the users about the relation between Socioeconomic variables Gross National Income,Percentage of poor households and Mean education years of adult population with the Total Ebola cases in the county." ),
        p("This dashboard also contains a summary data table for the viewers to get a view of the socioeconomic variables  and total cases in all the three countries."), 
        p("Note- Please refer R console for Ebola case counts.Thank you for viewing!"),
        
      ),
      
      tabItem(
        tabName = 'mapstab', ##interactive map tab
        h2('Map'),
        leafletOutput(outputId = "ebolamap", height = 700) 
      ),
      
      tabItem(
        tabName = 'tabmap1',##static map1 tab
        h2('Static Map1'),
        tags$img(src='map1.png', height=500, width=500)#static map1 img insert
        
      ),
      
      tabItem(
        tabName = 'tabmap2',##static map2 tab
        h2('Static Map2'),
        tags$img(src='map2.png', height=500, width=500)#static map2 img insert
        
      ),
      tabItem(
        tabName = 'tabmap3',##static map3 tab
        h2('Static Map3'),
        tags$img(src='map3.png', height=500, width=500)#static map3 img insert
      ),
      
      
      tabItem(
        tabName = 'tabmap1',##summary table tab
        h2('Summary table'),
        dataTableOutput(outputId = 'summary_table')
      )
      
    ),
    
    #  leafletOutput("ebolamap"),
    
  )
)

# Shiny server ------------------------------------------------------------


server <- function(input, output, session){ ##server
  
  
  observe({ ##adding interactivity to the menu features of country & county
    locations <- if (is.null(input$state_select)) character(0) else {
      filter(ebola, country %in% input$state_select) %>%
        `$`('location') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$locations[input$locations %in% locations])
    updateSelectizeInput(session, "city_select", choices = locations,
                         selected = stillSelected, server = TRUE)
  })
  
  
  # Summary table:
  
  output$summary_table <-##adding summary table
    renderDataTable(
      summarized())
  
  ## create interactive map
  output$ebolamap <- renderLeaflet({
    
    leaflet(data= df,height = "100%") %>%##leaflet map
      addTiles() %>%
      addProviderTiles('Hydda.Full') %>% 
      addMarkers(lng = ~longitude, lat = ~latitude)  %>% 
      setView(lng = -10.81544453, lat = 6.698632754, zoom = 4)#setting view  
    
   
    
  })
  
  
  ## respond to the filtered data
  observeEvent(input$mapbutton, {
    
    df_filtered <- reactive({
      ebolatable %>% 
        filter( Country == input$state_select, City == input$city_select, Casedate == input$date1 )
    })
    
    leafletProxy(mapId = "ebolamap", data = df_filtered()) %>%
      clearMarkers() %>%   ## clear previous markers
      addMarkers(lng = ~longitude, lat = ~latitude)
    
    print(df_filtered())#output
    
  })
  
  
  
}

# App ---------------------------------------------------------------------

shinyApp(ui, server) #run the app

