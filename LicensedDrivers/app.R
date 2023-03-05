library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)


# Load the dataset for the app
data <- read.csv("licensed_drivers.csv")
states <- sort(unique(data$State))
# Merge the two data frames based on the "State" column
state_coor <- read.csv("StateGeo_lon_lat.csv")
merged <- merge(data, state_coor, by = "State")

# Create the "Lat" and "Lon" columns based on the "Latitude" and "Longitude" columns in the merged data frame
merged$Lat <- merged$Latitude
merged$Lon <- merged$Longitude

# Remove the "Latitude" and "Longitude" columns from the merged data frame
data <- merged[, !(names(merged) %in% c("Latitude", "Longitude"))]
# Define function to map states to regions
get_region <- function(state) {
  region <- case_when(
    state %in% c("Delaware", "Kentucky", "Maryland", "North Carolina", "Tennessee", "Virginia", "West Virginia") ~ "Mideast",
    state %in% c("Illinois", "Indiana", "Iowa", "Michigan", "Minnesota", "Missouri", "Ohio", "Wisconsin") ~ "Midwest",
    state %in% c("Colorado", "Kansas", "Montana", "Nebraska", "North Dakota", "South Dakota", "Utah", "Wyoming") ~ "Mountain-Prairie",
    state %in% c("Connecticut", "Maine", "Massachusets", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont") ~ "Northeast",
    state %in% c("Alaska", "Idaho", "Oregon", "Washington") ~ "Northwest",
    state %in% c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "Oklahoma", "Puerto Rico", "South Carolina", "Texas") ~ "Southeast",
    state %in% c("Arizona", "California", "Hawaii", "Nevada", "New Mexico") ~ "Southwest",
    TRUE ~ NA_character_
  )
  return(region)
}

# Apply the function to create a new Region column in data
data <- data %>% mutate(Region = get_region(State))

# Define UI for application that plots features of movies -----------
ui <- dashboardPage(
  
  # Application title -----------------------------------------------
  dashboardHeader(title = "Licensed Drivers Dashboard"),
  
  # Sidebar layout with a input and output definitions --------------
  dashboardSidebar(
    
    tags$style(HTML(".sidebar { height: calc(100vh - 50px) !important; overflow-y: auto; }")
    ),
    
    # Inputs: Select variables to plot ------------------------------
    # Select which gender group to display
    radioButtons(inputId = "selected_gender",
                 label = "Select Gender:",
                 choices = c("Female", "Male")),
    
    selectInput(inputId = "selected_region", 
                label = "Select Region:",
                choices = c("Mideast", "Midwest","Mountain-Prairie","Northeast","Southeast", "Southwest")),
    
    # Select the Year range
    sliderInput(inputId = "selected_year", 
                label = "Select Year Range", 
                min = 1994, max = 2018, 
                value = c(2000, 2010)),
    
    # Select which age group to display
    checkboxGroupInput(inputId = "selected_cohort",
                       label = "Select Age group:",
                       choices = sort(unique(data$Cohort))),
    
    # Show data table 
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),
    
    # Add a download button
    downloadButton("downloadData", "Download Data")
  ),
  
  # Output
  dashboardBody(
    # Value Boxes ----------------------------------------------
    fluidRow(
      valueBoxOutput("total_drivers", width = 4),
      valueBoxOutput("AAGR", width = 4),
      valueBoxOutput("senior_drivers", width = 4)
    ),
    
    tabBox(
      title = "Plots",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("bar Plot", plotlyOutput("barPlot", height = "400px")),
      tabPanel("Line Graph", plotlyOutput("linePlot", height = "400px")),
      tabPanel("Map", leafletOutput("map"))),
  
    # Add the data table
    fluidRow(
      box(
        title = "Licensed Drivers Data",
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput(outputId = "driversTable")
      )
    )
  )
)

# Define server function required to create the plots
server <- function(input, output) {
  # Filter the data based on the inputs
  region_subset <- reactive({ 
    filter(data, 
           Gender %in% input$selected_gender, 
           Cohort %in% input$selected_cohort, 
           Region %in% input$selected_region, 
           Year >= input$selected_year[1],
           Year <= input$selected_year[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(data = region_subset()) %>% 
      addTiles() %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addMarkers(lng = ~Lon, lat = ~Lat, popup = ~paste("State: ", State, "<br>Year: ", max(Year), "<br>Number of drivers: ",drivers_sum))
  })
  
  # Print data table if checked
  output$driversTable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = region_subset(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  
  output$linePlot <- renderPlotly({
    if (nrow(region_subset()) == 0) {
      return(NULL)
    }
    drivers_sum_region <- region_subset() %>% 
      group_by(Year, Cohort, Region) %>% 
      summarise(total_drivers_sum = sum(drivers_sum))
    
    ggplot(data = drivers_sum_region,aes(x=Year, y=total_drivers_sum, color=Cohort)) + 
      geom_line(aes(group = Cohort)) + 
      labs(title="Line Graph") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Drivers") + 
      geom_point() + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  output$barPlot <- renderPlotly({
    if (nrow(region_subset()) == 0) {
      return(NULL)
    }
    
    drivers_sum_region <- region_subset() %>% 
      group_by(Year, Cohort, Region) %>% 
      summarise(total_drivers_sum = sum(drivers_sum))
    
    ggplot(data = drivers_sum_region, aes(x = Year, y = total_drivers_sum, fill = Cohort)) +
      geom_bar(color = "black",position="stack", stat="identity") + 
      labs(title="Bar Plot") + 
      scale_fill_brewer(palette = "Set3") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Year", y = "Drivers") + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  # Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$selected_region, "_", input$selected_gender, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(region_subset(), file, row.names = FALSE)
    }
  )
  
  output$total_drivers <- renderValueBox({
    valueBox(
      paste0(format(sum(region_subset()$drivers_sum), big.mark = ",")), 
      "Total Drivers", 
      icon = icon("car"), 
      color = "blue"
    )
  })
  
  output$AAGR <- renderValueBox({
    # get the selected year from an input control called `selected_year`
    selected_year <- input$selected_year
    
    # calculate the AAGR for the selected year
    data_summed <- region_subset() %>% group_by(Year) %>% summarise(drivers_sum = sum(drivers_sum))
    aagr <- (data_summed$drivers_sum[length(data_summed$drivers_sum)] / data_summed$drivers_sum[1])^(1/length(data_summed$drivers_sum)) - 1
    
    # initialize as 0
    if (is.null(input$selected_cohort)) {
      aagr <- 0
    }
    formatted_aagr <- sprintf("%.2f%%", aagr*100)
    valueBox(
      paste0(sprintf("%.2f%%", aagr*100)),
      "AAGR for Selected Year",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Total number of senior drivers
  output$senior_drivers <- renderValueBox({
    senior_cohort <- c("60-64", "65-69", "70-74", "75-79","80-84","85+")
    valueBox(
      paste0(format(sum(filter(region_subset(), Cohort %in% senior_cohort)$drivers_sum), big.mark = ",")), 
      "Senior Drivers(>60 years old)", 
      color = "maroon"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
