library(shiny)
library(ggplot2)

# Calculate statistics from the data
MEAN_THC <- 12.8
SD_THC <- 3.2
Q1_THC <- 10.3
Q3_THC <- 15.3

ui <- fluidPage(
  titlePanel("Compare Your THC Level"),
  sidebarLayout(
    sidebarPanel(
      numericInput("thc_value", 
                  "Enter your sample's THC %:", 
                  value = 12.8,
                  min = 4,
                  max = 30,
                  step = 0.1),
      textOutput("percentile_text")
    ),
    mainPanel(
      plotOutput("thc_plot", height = "400px")
    )
  )
)

server <- function(input, output) {
  # Generate distribution data
  x <- seq(4, 25, length.out = 1000)
  density_data <- data.frame(
    x = x,
    y = dnorm(x, mean = MEAN_THC, sd = SD_THC)
  )
  
  output$thc_plot <- renderPlot({
    ggplot() +
      # Plot density curve
      geom_line(data = density_data, aes(x = x, y = y), 
                color = "#003E42ff", size = 1) +
      geom_area(data = density_data, aes(x = x, y = y), 
                fill = "#003E42ff", alpha = 0.3) +
      # Add reference lines
      geom_vline(xintercept = Q1_THC, 
                 linetype = "dashed", color = "#003E42ff", alpha = 0.7) +
      geom_vline(xintercept = Q3_THC, 
                 linetype = "dashed", color = "#003E42ff", alpha = 0.7) +
      geom_vline(xintercept = MEAN_THC, 
                 linetype = "dashed", color = "#003E42ff") +
      # Add user's THC value
      geom_vline(xintercept = input$thc_value, 
                 color = "#EEC99Bff", size = 1.5) +
      # Add labels
      annotate("text", 
               x = c(Q1_THC, Q3_THC),
               y = c(0, 0),
               label = c("Q1: 25%", "Q3: 75%"),
               vjust = -0.5,
               color = "#003E42ff") +
      # Customize theme
      theme_minimal() +
      labs(title = "THC Distribution with Your Sample",
           x = "THC (%)",
           y = "Density") +
      theme(
        text = element_text(family = "sans-serif"),
        plot.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        plot.background = element_rect(fill = "#f8f9fa", color = NA)
      )
  })
  
  output$percentile_text <- renderText({
    value <- input$thc_value
    if (value < Q1_THC) {
      sprintf("Your sample's THC level (%.1f%%) is below the 25th percentile (Q1: %.1f%%)", 
             value, Q1_THC)
    } else if (value > Q3_THC) {
      sprintf("Your sample's THC level (%.1f%%) is above the 75th percentile (Q3: %.1f%%)", 
             value, Q3_THC)
    } else {
      sprintf("Your sample's THC level (%.1f%%) is between the 25th and 75th percentiles (Q1: %.1f%%, Q3: %.1f%%)", 
             value, Q1_THC, Q3_THC)
    }
  })
}

shinyApp(ui, server)
