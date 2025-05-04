library(shiny)
library(shinydashboard) # For layout/effects
library(RSQLite)
library(DBI)
library(ggplot2)
library(plotly) # For 'biometric plot' and 'calendar'
library(RPostgres)
library(dplyr) # For 'calendar' joins
library(lubridate) # For 'calendar', helps with date data
library(igraph) # For 'network graph'
library(ggraph) # For 'network graph'

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Veterinary Clinic Dashboard"),
  skin = "purple",
  # Setting up collapsible sidebar:
  dashboardSidebar(
    sidebarMenu(
      # Sidebar selections
      menuItem("Tabular Views", tabName = "tables", icon = icon("clipboard")),
      menuItem("Compare Biometrics", tabName = "plots", icon = icon("chart-line")),
      menuItem("View Appointments", tabName = "appoints", icon = icon("calendar")),
      menuItem("Pet-Owner Network", tabName = "network", icon = icon("sitemap"))
    )
    ),
  
  dashboardBody(
    # Setting up the database tables:
    tabItems(
      tabItem(tabName = "tables",
              fluidRow(
                tabsetPanel(
                  id = 'dataset',
                  # One tab for each relation
                  tabPanel("Animals", DT::dataTableOutput("mytable1")),
                  tabPanel("Owners", DT::dataTableOutput("mytable2")),
                  tabPanel("Veterinarians", DT::dataTableOutput("mytable3")),
                  tabPanel("Appointments", DT::dataTableOutput("mytable4")),
                  tabPanel("Vital Records", DT::dataTableOutput("mytable5"))
                )
              )),
      # Set up the biometric plot:
      tabItem(tabName = "plots",
              box(title = "Compare Aggregate Biometrics", width = 12, # Adjust width as needed
                  fluidRow(
                    # Let user choose selected bio-metrics (can add more options later)
                    column(width = 6, selectInput("x_biometric", "Select X-axis:", choices = c("weight", "pulse", "ph", "glucose", "rbc", "wbc"))),
                    column(width = 6, selectInput("y_biometric", "Select Y-axis:", choices = c("pulse", "weight", "ph", "glucose", "rbc", "wbc"), selected = "pulse")),
                    column(width = 6, selectInput("x1_biometric", "Select Hue Element:", choices = c("weight", "pulse", "ph", "glucose", "rbc", "wbc"))),
                    column(width = 6, selectInput("y1_biometric", "Select Size Element:", choices = c("pulse", "weight", "ph", "glucose", "rbc", "wbc")))
                  ),
                  plotlyOutput("scatterPlot")
                  
              )),
      # Set up the calendar:
      tabItem(tabName = "appoints",
              fluidRow(
                box(title = "Appointment Calendar", width = 12,
                    plotlyOutput("calendarPlot"))
              )),
      # Set up the network grpah:
      tabItem(tabName = "network",
              fluidRow(
                box(title = "Owner-Pet Network", width = 12,
                    plotOutput("ownerAnimalNetwork"))
              ))
    )
  )
)

server <- function(input, output, session) {
  # SQLite connection
  sqlite_conn <- dbConnect(RSQLite::SQLite(), dbname ='proj4.db') #.db file for my project
  
  # Table Output
  # I am grabbing all attributes for each relation
  output$mytable1 <- DT::renderDT({
      
    query <- paste0("SELECT * ",
                      "FROM animal")
    data <- dbGetQuery(sqlite_conn, query)
    data
  })
    
  output$mytable2 <- DT::renderDT({
      
    query <- paste0("SELECT * ",
                    "FROM own")
    data <- dbGetQuery(sqlite_conn, query)
    data
  }) 
  
  output$mytable3 <- DT::renderDT({
      
    query <- paste0("SELECT * ",
                    "FROM staff") 
    data <- dbGetQuery(sqlite_conn, query)
    data
  }) 
    
  output$mytable4 <- DT::renderDT({
      
    query <- paste0("SELECT * ",
                    "FROM appointment") 
    data <- dbGetQuery(sqlite_conn, query)
    data
  }) 
    
  output$mytable5 <- DT::renderDT({
      
    query <- paste0("SELECT * ",
                    "FROM vital") 
    data <- dbGetQuery(sqlite_conn, query)
    data
  }) 
    
  # Bio-plot
  # Get all attributes from 'vital'
  output$scatterPlot <- renderPlotly({
    query <- paste0("SELECT * ",
                    "FROM vital") 
    data <- dbGetQuery(sqlite_conn, query)
    data
    
    # Add input fields
    x_var <- input$x_biometric
    y_var <- input$y_biometric
    x1_var <- input$x1_biometric
    y1_var <- input$y1_biometric
    
    # If data is selected (not null), proceed to use values of fields above
    if (!is.null(data) && !is.null(x_var) && !is.null(y_var) && x_var != "" && y_var != "") {
      # Then, plot it
      p <- ggplot(data, aes_string(x = x_var, y = y_var, color = x1_var, size = y1_var)) +
        geom_point() +
        theme_minimal() +
        labs(title = paste(y_var, "vs.", x_var),
             x = x_var,
             y = y_var) +
        theme(text = element_text(family = "Palatino"))
      ggplotly(p)
    } else {
      # Display a message if nothing is selected (hopefully never)
      plotly() %>% add_trace(x = 1, y = 1, text = "Select metrics to display plot", mode = "text")
    }
  })
  
  # Calendar Heat Map w/ 'plotly' using 'ggtitle'
  # Made with Google Gemini (see report and github repo)
  output$calendarPlot <- renderPlotly({
    # Select all attributes from 'appointments'
    query_appointments <- "SELECT appointment_date, appointment_time, reason, ontime, s_id, aid, o_id FROM appointment"
    all_appointments <- dbGetQuery(sqlite_conn, query_appointments) # Collect query from .db file
    
    if (nrow(all_appointments) > 0) { # Cehck if 'NA'
      all_appointments$appointment_date <- as.Date(all_appointments$appointment_date) # Convert date format
      
      # Aggregate data to check for appointments on each date and gather details
      daily_appointments_detail <- all_appointments %>%
        group_by(appointment_date) %>%
        summarise(
          # Added in more summarixe options
          has_appointment = TRUE, # Cehck if appointment exists
          reasons = paste(reason, collapse = "<br>"), # If so, bring in reasons
          times = paste(appointment_time, collapse = "<br>"), # Then print time
          ontimes = paste(ifelse(ontime == 1, "Yes", "No"), collapse = "<br>"), # Is on-time entered?
          vets = paste("Staff ID:", s_id, collapse = "<br>"), # Show staff id as 'Staff ID: s_id'
          aids = paste("Animal ID:", aid, collapse = "<br>"), # Ditto with aid
          owners = paste("Owner ID:", o_id, collapse = "<br>"), # Ditto with o_id
          full_date = format(appointment_date, "%Y-%m-%d") # Turn into right format (a double-check)
        )
      
      # Create a full date range for the calendar
      min_date <- min(all_appointments$appointment_date) # Earliest date
      max_date <- max(all_appointments$appointment_date) # Latest date
      full_date_range <- data.frame(appointment_date = seq(min_date, max_date, by = "day")) # split the date range into days
      
      # Join with appointment details within the full-date range
      # Really, adding the two steps above:
      calendar_data <- full_date_range %>%
        left_join(daily_appointments_detail, by = "appointment_date") %>%
        mutate(has_appointment = ifelse(is.na(has_appointment), FALSE, has_appointment), # Make sure not empty, but if not proceed to join other data
               # Add in all possible details:
               year = year(appointment_date), # Split up the year data
               month = month(appointment_date), # the month data
               day = day(appointment_date), # the day data
               weekday = wday(appointment_date, week_start = 1), # Monday as 1
               week = isoweek(appointment_date), # enumerate the week
               reasons = ifelse(is.na(reasons), "", reasons), # ditto as above aggregation
               times = ifelse(is.na(times), "", times),
               ontimes = ifelse(is.na(ontimes), "", ontimes),
               vets = ifelse(is.na(vets), "", vets),
               aids = ifelse(is.na(aids), "", aids),
               owners = ifelse(is.na(owners), "", owners),
               full_date = format(appointment_date, "%Y-%m-%d"))
      
      # For checking output/debugging:
      print("Head of calendar_data:") 
      print(head(calendar_data))
      print("Structure of calendar_data:")
      print(str(calendar_data))
      
      print(paste("Min Date:", min_date))
      print(paste("Max Date:", max_date))
      
      print(paste("Number of rows in calendar_data:", nrow(calendar_data)))
      
      # Ensure week and weekday are not NA
      calendar_data <- calendar_data %>%
        filter(!is.na(week), !is.na(weekday)) # Can't be too careful at each step!
      
      # Create the ggplot2 calendar heat map with tooltip information
      p <- ggplot(calendar_data, aes(x = week, y = weekday, fill = has_appointment, 
                                     text = paste(full_date, # This will bring in hovering info,
                                                  # that we be activated interactively by 'plotly'
                                                  # Make calendar plot above, and if appointment exists,
                                                  # modify and add info as follwos:
                                                  ifelse(has_appointment,
                                                         paste("<br>Reason(s):", reasons,
                                                               "<br>Time(s):", times,
                                                               "<br>Ontime:", ontimes,
                                                               "<br>", vets,
                                                               "<br>", aids,
                                                               "<br>", owners),
                                                         "<br>No Appointments")))) + # Print if no appointment found for the date
        geom_tile(color = "white", width = 0.9, height = 0.9) + # The standard visual tiles representing days
        scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "steelblue"),
                          labels = c("No Appointment", "Appointment")) + # Makes appointment days blue, non-appointment days grey
        # To fix years labels being upside down
        scale_y_reverse(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        facet_grid(year ~ month, scales = "free", space = "free", switch = "y") + # Months across, years up/down
        labs(title = "Please Remember to Check IDs", fill = "Appointment") +
        theme_minimal() +
        theme( # Simplify theme options
          strip.text.y = element_text(angle = 0, hjust = 0), # Changed angle to 0
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.x = unit(0.1, "lines"),
          panel.spacing.y = unit(0.1, "lines")
        )
        
      # Convert to 'plotly' object
      ggplotly(p, tooltip = "text")
    } else {
      # Display a message if there's no appointment data
      plotly() %>% add_trace(x = 0, y = 0, text = "No appointment data available", mode = "text") %>% layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE), yaxis = list(showticklabels = FALSE, zeroline = FALSE))
    }
  })
  
  # Owner-pet network graph
  output$ownerAnimalNetwork <- renderPlot({
    # Get owner and pet names from .db file
    # 'JOIN' result is to match properly
    query_owner_animal <- "
      SELECT
        o.name AS owner_name,
        a.name AS animal_name
      FROM own o
      JOIN animal a ON o.o_id = a.o_id;
    "
    owner_animal_df <- dbGetQuery(sqlite_conn, query_owner_animal)
    
    # If the result of query loaded into df is not-null (it better not be!)
    if (nrow(owner_animal_df) > 0) {
      # Make edges for  graph
      edges <- owner_animal_df %>%
        rename(from = owner_name, to = animal_name)
      
      # Create the graph
      graph_data <- graph_from_data_frame(edges, directed = FALSE)
      
      # Visualize the graph
      # I could not decide on a layout, so I let it choose with 'nicely'
      ggraph(graph_data, layout = 'nicely') + 
        # From the 'edges' make linkages
        geom_edge_link() +
        # Make nodes, if there is a match in names color by owner and animal
        geom_node_point(aes(color = ifelse(V(graph_data)$name %in% owner_animal_df$owner_name, 'Owner', 'Animal')), size = 5) +
        # Use names found for labels
        geom_node_text(aes(label = name), repel = TRUE) +
        scale_color_manual(values = c("Owner" = "steelblue", "Animal" = "tomato")) + # choose blue for owner and red for animals
        theme_graph() +
        labs(color = "Node Type") +
        ggtitle("Owner to Animal Network") 
    } else {
      # Let user know if there is no data (Impossible, but nice function to have JIC)
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No owner-animal data available to display", size = 6, color = "grey50") +
        theme_void()
    }
  })
}
  
# Run the app
shinyApp(ui, server)
