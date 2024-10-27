library(shiny)
library(here)
library(bslib)
library(lubridate)

# source script file with function that gets the data and makes the plots
source("helper.R")

preparation_output <- prepare_to_scrape_for_a_given_prefix_fn()

# scrape_time_schedule_result <- 
#  scrape_time_schedule_fn(current_time_schedule_tbl_urls = preparation_output)

# Define UI ----
ui <- page_fluid(

  navset_tab( 
  nav_panel("Start here", helpText(
    tags$h1("Create plots of undergrad student enrollment over time for a UW course prefix"),
    hr(),
    tags$p(
      "Choose a course prefix and a date range, and then click on the button below to scrape the ",
      tags$a("UW Time Schedule web pages", 
      href = "https://www.washington.edu/students/timeschd/"),". Then click the tabs along the 
      top to view the results. It make take a few seconds for the plots to appear. Course 
      codes ending in 99 and 69 are omitted. Data are scraped from the UW Time Schedule 
      web pages when you click on the button below, that scraping may take several seconds. This is a 
      very simple application I developed to assist with course planning in my department, 
      and may not work or be useful for other departments. The source code for this app is 
      freely available at",
      tags$a("https://github.com/benmarwick/uw-time-schedule-data-viz", 
      href = "https://github.com/benmarwick/uw-time-schedule-data-viz"),"."
  )),
  hr(),
  selectInput(
    inputId = "selected_prefix",
    label = h3("Choose a prefix:"),
    choices = 
      setNames(as.list(preparation_output$prefixes), 
               preparation_output$txt)
  ),
  dateRangeInput(inputId ="date", 
                 format = "yyyy",
                 startview = "year",
                 start = Sys.Date() - years(5), 
                 end = Sys.Date(), 
                 min = NULL, 
                 max = NULL,
                 label = h3("Choose a range of years:")),
  actionButton("generate_plots", 
               "Click here to scrape the data from the UW Time Schedule webpages, then click on the tabs above to see the plots")
  ), 
  nav_panel("Plots of student numbers over time",   
            plotOutput("plot1",
                       height = "1000px")), 
  nav_panel("Plots of ratio to capacity over time", 
            plotOutput("plot2",
                       height = "1000px")), 
  nav_panel("Plots by course", 
            plotOutput("plot3",
                       height = "1000px")), 
), 
id = "tab" 
)

# Define server logic ----

server <- function(input, output) {
  
  # Reactive value to hold plot data
  plot_data <- reactiveVal(NULL)  # Initialize to NULL
  
  observeEvent(input$generate_plots, {
    # Show a modal when the button is clicked
    showModal(modalDialog(
      title = "Collecting data and generating plots...",
      "Please wait for this message to automatically close, then view the plots using the tabs at the top of the page.",
      easyClose = FALSE,  # Prevent modal from closing automatically
      footer = NULL
    ))
    
     # Generate plot data here
     scrape_time_schedule_fn(
        current_time_schedule_tbl_urls = preparation_output, 
        prefix = input$selected_prefix,
        start_year = as.numeric(year(input$date[[1]])),
        end_year =   as.numeric(year(input$date[[2]]))
      ) %>% plot_data()
     
     # Dismiss the modal
     removeModal()  # Remove the modal after data generation
      
  })  
  
  output$plot1 <- renderPlot({ 
    req(plot_data()) 
    plot_data()[[1]] 

  },
  height = 1500) 
  
  
  output$plot2 <- renderPlot({ 
    req(plot_data()) 
    plot_data()[[2]] 
  },
  height = 1500) 
  
  
  output$plot3 <- renderPlot({ 
    req(plot_data()) 
    plot_data()[[3]] 
  },
  height = 1500) 
  
  
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)

