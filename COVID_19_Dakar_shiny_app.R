# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)          
library(dplyr)       
library(RColorBrewer)
library(shinythemes)
library(ggplot2)  

load("dakar_covid_negh_commune.RData")

# Ensure months are in proper order from January to December
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
df$Month_Name <- factor(df$Month_Name, levels = month_order)  # Order months
df_animated$Month_Name <- factor(df_animated$Month_Name, levels = month_order)  # Order months

# Convert df_animated to an sf object (spatial points)
df_animated_sf <- st_as_sf(df_animated, coords = c("Longitude", "Latitude"), crs = 32628)  # EPSG:32628 (UTM zone 28N)
df_animated_sf <- st_transform(df_animated_sf, crs = 4326)  # Transform to WGS 84

# CSS 
custom_css <- "
  .title-panel {
    text-align: center;
    font-family: 'Arial', sans-serif;
    font-weight: bold;
    color: #2C3E50;
    margin-bottom: 20px;
  }
  .sidebar-panel {
    background-color: #f8f9fa;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }
  .main-panel {
    padding: 15px;
  }
  body {
    background-color: #ECF0F1;
  }
  .custom-play-button {
    background-color: #007BFF;
    color: white;
    padding: 5px 10px;
    border: none;
    border-radius: 5px;
    font-weight: bold;
    cursor: pointer;
  }
  .custom-play-button:hover {
    background-color: #0056b3;
  }
"

# UI definition 
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Choose a theme from shinythemes
  tags$head(
    tags$style(HTML(custom_css))  # Add custom CSS
  ),
  
  # Title Panel
  div(class = "title-panel", h3("Tracking the Outbreak of COVID-19 in Dakar, Senegal"),
      h4("Observed Records of COVID-19 Infected Cases"),
      h4("(May 2020 - May 2021)") 
  ),
  
  # Animated map controls
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-panel", 
          p("Use the controls below to explore monthly variations in reported COVID-19 infection cases across individual neighbourhoods in Dakar."),
          p("Select a year and navigate through months to view changes over time."),
          radioButtons("top_year", "Select Year for Animated Map", choices = c("2020", "2021"), selected = "2021"),
          sliderInput("month_slider", "Select or Play through Months", min = 1, max = 12, value = 1, step = 1,
                      animate = animationOptions(interval = 2000, loop = FALSE, playButton = icon("play", class = "custom-play-button"), pauseButton = icon("pause"))),  # Add custom play button
          width = 2 
      )
    ),
    
    # Animated Map
    mainPanel(
      div(class = "main-panel",  # Add class for styling
          leafletOutput("animated_observed_map", height = 500)
      ),
      width = 6 
    )
  ),
  
  #==================================================
  hr(),  # Separate sections
  
  # Subtitle for comparison section
  div(class = "title-panel", h3("Comparison of Observed and Predicted COVID-19 Prevalence"),
      h4("Prevalence of COVID-19 per 10,000 Population") 
  ), 
  
  # For observed and predicted maps
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-panel",  
          p("Prevalence represents the number of observed or predicted COVID-19 cases per 10,000 individuals within each commune on a monthly basis."),
          p("This provides a standardized measure of infection intensity across communes."),
          radioButtons("year", "Select Year", choices = c("2020", "2021"), selected = "2021"),
          selectInput("month", "Select Month", choices = month_order),
          width = 3,
          plotOutput("histogram_plot", height = 400)
      )
    ),
    
    mainPanel(
      div(class = "main-panel",  # Add class for styling
          fluidRow(
            column(6, leafletOutput("observed_map", height = 600)),
            column(6, leafletOutput("predicted_map", height = 600))
          ),
          width = 9
      )
    )
  )
)

#===============================================
# Server definition
server <- function(input, output, session) {
  
  # Filter data based on selected year and month (Animated)
  filtered_data_animated <- reactive({
    df_animated_sf %>%
      filter(Year == as.numeric(input$top_year), Month_Name == month_order[input$month_slider])  
  })
  
  # Spatial join with new shapefile for the animated observed map
  joined_data_animated <- reactive({
    data <- filtered_data_animated()
    if (nrow(data) > 0) {
      st_join(dakar_boun_shapefile, data, join = st_intersects)
    } else {
      NULL  # Return NULL if no data
    }
  })
  
  # Compute the common range for the legend scale across observed cases for the animated map
  common_range_animated <- reactive({
    spatial_data <- joined_data_animated()
    if (!is.null(spatial_data) && nrow(spatial_data) > 0) {
      c(min(spatial_data$Observed_cases, na.rm = TRUE),
        max(spatial_data$Observed_cases, na.rm = TRUE))
    } else {
      c(0, 1)  # Default range if no data
    }
  })
  
  #==================================================
  # Render animated observed cases map
  output$animated_observed_map <- renderLeaflet({
    spatial_data <- joined_data_animated()
    month_selected <- month_order[input$month_slider]
    year_selected <- as.numeric(input$top_year)
    
    # Define logic for displaying the "No data" popup
    if ((year_selected == 2020 && month_selected %in% c("Jan", "Feb", "Mar", "Apr")) || 
        (year_selected == 2021 && month_selected %in% c("Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -17.444, lat = 14.6928, zoom = 20) %>%
        addPopups(-17.444, 14.6928, "No data available for this month.", options = popupOptions(closeOnClick = TRUE))
    } else {
      if (!is.null(spatial_data) && nrow(spatial_data) > 0) {
        # Categorize observed cases
        spatial_data$Observed_cat <- cut(spatial_data$Observed_cases,
                                         breaks = c(-Inf, 2, 5, 10, Inf),
                                         labels = c("0–2", "3–5", "6–10", "11 and above"),
                                         right = TRUE)
        
        # Define a manual color palette for the categories
        category_colors <- c("0–2" = "#ffffcc", 
                             "3–5" = "#fed976", 
                             "6–10" = "#fd8d3c", 
                             "11 and above" = "#e31a1c")
        
        observed_palette <- colorFactor(palette = category_colors, domain = spatial_data$Observed_cat)
        
        leaflet(spatial_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~observed_palette(Observed_cat),
            color = "#BDBDC3",
            weight = 1,
            fillOpacity = 0.7,
            highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
            label = ~paste0("Observed Cases: ", round(Observed_cases, 2), 
                            " (", Observed_cat, ")"),
            popup = ~paste0("<strong>Observed Cases: </strong>", round(Observed_cases, 2))
          ) %>%
          addLegend(
            pal = observed_palette,
            values = spatial_data$Observed_cat,
            title = "Observed Cases (per month)",
            position = "bottomright"
          )
      } else {
        leaflet() %>%
          addTiles() %>%
          setView(lng = -17.444, lat = 14.6928, zoom = 20) %>%
          addPopups(-17.444, 14.6928, "No data available for this month.", options = popupOptions(closeOnClick = TRUE))
      }
    }
  })
  
  
  #===============================================
  
  # Filter data based on selected year and month for the bottom maps
  filtered_data <- reactive({
    df %>%
      filter(Year == as.numeric(input$year), Month_Name == input$month)  # Filter by both Year and Month
  })
  
  # Perform spatial join with original shapefile
  joined_data <- reactive({
    left_join(dakar_shapefile, filtered_data(), by = c("com_ids" = "com_code"))
  })
  
  # Compute the common range for the legend scale across both observed and predicted cases
  common_range <- reactive({
    spatial_data <- joined_data()
    c(min(c(spatial_data$Obs_prev, spatial_data$Pred_prev), na.rm = TRUE),
      max(c(spatial_data$Obs_prev, spatial_data$Pred_prev), na.rm = TRUE))
  })
  
  # Color palette for mapping using a common range
  create_palette <- function(values) {
    palette <- colorNumeric(palette = "YlOrRd", domain = common_range(), na.color = "grey")
    return(palette)
  }
  
  #=======================================
  
  # Render histogram plot
  output$histogram_plot <- renderPlot({
    # Filter for the months from May 2020 to August 2021
    filtered_hist_data <- df %>%
      filter((Year == 2020 & Month_Name %in% c("May", "Jun", "July", "Aug", "Sept", "Oct", "Nov", "Dec")) | 
               (Year == 2021 & Month_Name %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July"))) %>%
      group_by(Year, Month_Name) %>%
      summarise(Aggregated_Obs = sum(Obs_prev, na.rm = TRUE)) %>%
      ungroup()
    
    # Create a combined month name for x-axis
    filtered_hist_data <- filtered_hist_data %>%
      mutate(Month_Year = factor(paste(Month_Name, Year), 
                                 levels = c("May 2020", "Jun 2020", "July 2020", "Aug 2020", 
                                            "Sept 2020", "Oct 2020", "Nov 2020", "Dec 2020",
                                            "Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021",
                                            "May 2021", "Jun 2021", "July 2021")))
    
    # Create the histogram
    ggplot(filtered_hist_data, aes(x = Month_Year, y = Aggregated_Obs, fill = as.factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#FF9999", "#66B3FF"), labels = c("2020", "2021")) +
      labs(x = "Months (Year)", 
           y = "Aggregated Observed Prevalence", fill = "Year", 
           title = "Commune Level Aggregated Observed Prevalence \nper 10,000 Population") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))  
  })
  
  # Plot observed cases map
  output$observed_map <- renderLeaflet({
    spatial_data <- joined_data()
    observed_palette <- create_palette(spatial_data$Obs_prev)  # Use common range
    
    leaflet(spatial_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~observed_palette(Obs_prev),
        color = "#BDBDC3",
        weight = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
        label = ~paste0("Observed: ", round(Obs_prev, 2)),
        popup = ~paste0("<strong>Commune: </strong>", Comm_name, "<br><strong>Observed: </strong>", round(Obs_prev, 2))
      ) %>%
      addLegend(
        pal = observed_palette,
        values = common_range(),
        title = "Observed Prevalance",
        position = "bottomright"
      )
  })
  
  # Plot predicted cases map
  output$predicted_map <- renderLeaflet({
    spatial_data <- joined_data()
    predicted_palette <- create_palette(spatial_data$Pred_prev)  # Use common range
    
    leaflet(spatial_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~predicted_palette(Pred_prev),
        color = "#BDBDC3",
        weight = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
        label = ~paste0("Predicted: ", round(Pred_prev, 2)),
        popup = ~paste0("<strong>Commune: </strong>", Comm_name, "<br><strong>Predicted: </strong>", round(Pred_prev, 2))
      ) %>%
      addLegend(
        pal = predicted_palette,
        values = common_range(),
        title = "Predicted Prevalance",
        position = "bottomright"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
