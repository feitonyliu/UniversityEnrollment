library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

# Generate random data
set.seed(123)
years <- 2014:2024
universities <- c(
  "University of Sydney", "University of Melbourne", "Monash University",
  "University of Queensland", "Australian National University",
  "University of New South Wales", "University of Western Australia"
)
faculties <- c(
  "Engineering", "Medicine", "Business", "Law", "Science",
  "Arts", "Education"
)

# Create sample data
n_records <- 2000
data <- data.frame(
  year = sample(years, n_records, replace = TRUE),
  university = sample(universities, n_records, replace = TRUE),
  faculty = sample(faculties, n_records, replace = TRUE),
  enrolled_students = sample(1:50, n_records, replace = TRUE),
  average_atar = round(runif(n_records, min = 70, max = 99), 1)
)

ui <- page_sidebar(
  title = "Southern Cross Grammar - University Enrollment Dashboard",
  sidebar = sidebar(
    selectInput("year", "Select Year", 
                choices = sort(unique(data$year)), 
                selected = max(data$year)),
    selectInput("university", "Select University",
                choices = c("All", sort(unique(data$university)))),
    selectInput("faculty", "Select Faculty",
                choices = c("All", sort(unique(data$faculty)))),
    hr(),
    downloadButton("downloadData", "Download Data")
  ),
  
  layout_columns(
    value_box(
      title = "Total Enrollments",
      value = textOutput("total_enrollments"),
      showcase = bsicons::bs_icon("people-fill")
    ),
    value_box(
      title = "Average ATAR",
      value = textOutput("avg_atar"),
      showcase = bsicons::bs_icon("graph-up")
    ),
    value_box(
      title = "Number of Universities",
      value = textOutput("num_universities"),
      showcase = bsicons::bs_icon("buildings")
    )
  ),
  
  layout_columns(
    card(
      card_header("Enrollment Trends Over Time"),
      plotOutput("trend_plot")
    ),
    card(
      card_header("Faculty Distribution"),
      plotOutput("faculty_plot")
    )
  ),
  
  card(
    card_header("Detailed Enrollment Data"),
    DT::dataTableOutput("enrollment_table")
  )
)

server <- function(input, output, session) {
  
  # Filtered dataset
  filtered_data <- reactive({
    result <- data
    
    if (input$university != "All") {
      result <- result %>% filter(university == input$university)
    }
    
    if (input$faculty != "All") {
      result <- result %>% filter(faculty == input$faculty)
    }
    
    result %>% filter(year == input$year)
  })
  
  # Value boxes
  output$total_enrollments <- renderText({
    sum(filtered_data()$enrolled_students)
  })
  
  output$avg_atar <- renderText({
    round(mean(filtered_data()$average_atar), 1)
  })
  
  output$num_universities <- renderText({
    n_distinct(filtered_data()$university)
  })
  
  # Trend plot
  output$trend_plot <- renderPlot({
    trend_data <- data
    if (input$university != "All") {
      trend_data <- trend_data %>% filter(university == input$university)
    }
    if (input$faculty != "All") {
      trend_data <- trend_data %>% filter(faculty == input$faculty)
    }
    
    trend_data %>%
      group_by(year) %>%
      summarise(total_enrolled = sum(enrolled_students)) %>%
      ggplot(aes(x = year, y = total_enrolled)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      theme_minimal() +
      labs(x = "Year", y = "Total Enrolled Students") +
      theme(text = element_text(size = 12))
  })
  
  # Faculty distribution plot
  output$faculty_plot <- renderPlot({
    filtered_data() %>%
      group_by(faculty) %>%
      summarise(total_enrolled = sum(enrolled_students)) %>%
      ggplot(aes(x = reorder(faculty, -total_enrolled), y = total_enrolled)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_minimal() +
      labs(x = "Faculty", y = "Total Enrolled Students") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data table
  output$enrollment_table <- DT::renderDataTable({
    filtered_data() %>%
      select(university, faculty, enrolled_students, average_atar) %>%
      arrange(desc(enrolled_students))
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("enrollment_data_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
