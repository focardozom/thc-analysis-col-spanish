library(shiny)
library(ggplot2)

# Calculate statistics from the data
MEAN_THC <- 12.8
SD_THC <- 3.2
Q1_THC <- 10.3
Q3_THC <- 15.3

ui <- fluidPage(
    titlePanel("Compara tu Nivel de THC"),
    sidebarLayout(
        sidebarPanel(
            numericInput("thc_value",
                "Ingresa el % de THC de tu muestra:",
                value = MEAN_THC, # Set default to mean
                min = 4,
                max = 30,
                step = 0.1
            ),
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

    # Reactive value for validated THC input
    valid_thc <- reactive({
        if (is.null(input$thc_value) || is.na(input$thc_value)) {
            return(MEAN_THC) # Return mean if input is null or NA
        }
        return(input$thc_value)
    })

    output$thc_plot <- renderPlot({
        # Use validated THC value
        thc_value <- valid_thc()

        ggplot() +
            # Plot density curve
            geom_line(
                data = density_data, aes(x = x, y = y),
                color = "#003E42ff", size = 1
            ) +
            geom_area(
                data = density_data, aes(x = x, y = y),
                fill = "#003E42ff", alpha = 0.3
            ) +
            # Add reference lines
            geom_vline(
                xintercept = Q1_THC,
                linetype = "dashed", color = "#003E42ff", alpha = 0.7
            ) +
            geom_vline(
                xintercept = Q3_THC,
                linetype = "dashed", color = "#003E42ff", alpha = 0.7
            ) +
            geom_vline(
                xintercept = MEAN_THC,
                linetype = "dashed", color = "#003E42ff"
            ) +
            # Add user's THC value (only if valid)
            geom_vline(
                xintercept = thc_value,
                color = "#EEC99Bff", size = 1.5
            ) +
            # Add labels
            annotate("text",
                x = c(Q1_THC, Q3_THC),
                y = c(0, 0),
                label = c("Q1: 25%", "Q3: 75%"),
                vjust = -0.5,
                color = "#003E42ff"
            ) +
            # Customize theme
            theme_minimal() +
            labs(
                title = "Distribución de THC con tu Muestra",
                x = "THC (%)",
                y = "Densidad"
            ) +
            theme(
                text = element_text(family = "sans-serif"),
                plot.title = element_text(face = "bold"),
                panel.background = element_rect(fill = "#f8f9fa", color = NA),
                plot.background = element_rect(fill = "#f8f9fa", color = NA)
            )
    })

    output$percentile_text <- renderText({
        # Use validated THC value
        value <- valid_thc()

        if (is.null(value) || is.na(value)) {
            return("Por favor ingresa un valor válido de THC")
        }

        if (value < Q1_THC) {
            sprintf(
                "El nivel de THC de tu muestra (%.1f%%) está por debajo del percentil 25 (Q1: %.1f%%)",
                value, Q1_THC
            )
        } else if (value > Q3_THC) {
            sprintf(
                "El nivel de THC de tu muestra (%.1f%%) está por encima del percentil 75 (Q3: %.1f%%)",
                value, Q3_THC
            )
        } else {
            sprintf(
                "El nivel de THC de tu muestra (%.1f%%) está entre los percentiles 25 y 75 (Q1: %.1f%%, Q3: %.1f%%)",
                value, Q1_THC, Q3_THC
            )
        }
    })
}

shinyApp(ui, server)
