library(ggplot2)
library(tidyverse)

varnames <- names(cars)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 12,

          # Variables Inputs:
          varSelectInput("variables", "Select Input Variables", cars, multiple = TRUE),
          selectizeInput("outvar", "Select Output Variable", choices = varnames, "speed", multiple = F),

          # Run Button
          actionButton(inputId = "run", label = "Run")
        )
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    cars %>% dplyr::select(!!!input$variables, input$outvar)
  })


  plt <- eventReactive(input$run, {
    
    #Just creating lm formula
    current_formula <- paste0(input$outvar, " ~ ", paste0(input$variables, collapse = " + "))
    current_formula <- as.formula(current_formula)
    #Fitting lm
    fit <- lm(current_formula, data = df())
    pred <- predict(fit, newdata = df())

    #Plotting
    ggplot(df(), aes(df()[, input$outvar], pred)) +
      labs(x = "Observed", y = "Predicted") +
      geom_point() +
      theme_bw()

     #plot(df()[, input$outvar], pred)       #This one works fine!!!!
  })


  output$plot <- renderPlot({
     plt()
  })
}

# Run the application
shinyApp(ui = ui, server = server)