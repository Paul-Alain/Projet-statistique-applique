ui <- fluidPage(
  titlePanel("Exploration of solvency indicators by company"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "societe",
        label = "Select a company:",
        choices = sort(unique(dff_clean$SOCIETE))
      )
    ),
    mainPanel(
      plotOutput("soc_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$soc_plot <- renderPlot({
    plot_society_dual(dff_clean, input$societe)
  })
}

shinyApp(ui, server)
