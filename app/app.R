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
    "Create plots of student enrollment over time for a course prefix. Choose a course prefix and a date range, and then click on one of the plot tabs at the top. It make take 10 or more seconds for the plots to appear. "
  ),
  selectInput(
    inputId = "selected_prefix",
    label = h3("Choose a prefix"),
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
                 label = h3("Date range"))
  
  ), 
  nav_panel("Plot of student numbers",   
            plotOutput("plot1",
                       height = "1000px")), 
  nav_panel("Plot of ratio to capacity", 
            plotOutput("plot2",
                       height = "1000px")), 
  nav_panel("Plot by course", 
            plotOutput("plot3",
                       height = "1000px")), 
  nav_menu( 
    "Other links", 
    nav_panel("D", "Panel D content"), 
    "----", 
    "Description:", 
    nav_item( 
      a("Time Schedule Home", href = "https://www.washington.edu/students/timeschd/", target = "_blank") 
    ), 
  ), 
), 
id = "tab" 
)

# Define server logic ----

server <- function(input, output) {
  
  output$selected_prefix <- renderText({
    paste("You have selected", input$selected_prefix, 
          "for the period ",   
          year(input$date[1]), " to ",
          year(input$date[2]))
          
  })
  
  output$plot1 <- renderPlot({ 
    scrape_time_schedule_fn(
      current_time_schedule_tbl_urls = preparation_output, 
      prefix = input$selected_prefix,
      start_year = as.numeric(year(input$date[[1]])),
      end_year =   as.numeric(year(input$date[[2]]))
      )[[1]]
  },
  height = 1500) 
  
  
  output$plot2 <- renderPlot({ 
    scrape_time_schedule_fn(
      current_time_schedule_tbl_urls = preparation_output, 
      prefix = input$selected_prefix,
      start_year = as.numeric(year(input$date[[1]])),
      end_year =   as.numeric(year(input$date[[2]]))
    )[[2]]
  },
  height = 1500) 
  
  
  output$plot3 <- renderPlot({ 
    scrape_time_schedule_fn(
      current_time_schedule_tbl_urls = preparation_output, 
      prefix = input$selected_prefix,
      start_year = as.numeric(year(input$date[[1]])),
      end_year =   as.numeric(year(input$date[[2]]))
    )[[3]]
  },
  height = 1500) 
  
  
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)

