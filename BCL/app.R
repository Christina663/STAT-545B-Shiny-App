library(shiny)
library(tidyverse)
bcl <- read.csv("bcl-data.csv")
options(shiny.autoreload = TRUE)

# Putting stuff on the page to where we want they to go
ui <- fluidPage(
  titlePanel("BCL App"),
  "(Explanation goes here)
  this is an app that ...",
  sidebarLayout(
    sidebarPanel(
      sliderInput("my_slider", "Select a price range", min = 0, max = 200, value = c(10, 30)),
      selectInput("my_select", "Select a beverage cateogry", choices = unique(bcl$Type))

    ),
    mainPanel(
      plotOutput("my_plot"),
      tableOutput("my_table")
    )
  )
)

server <- function(input, output) {
  #Use the reaction() function to keep our interactive objects (plots, tables, etc)
  #updated for each user modification
  filtered <- reactive(
    {
   #  Print out the content and length of an unknown object
   #  print(input$my_slider)
      bcl %>%
        filter(Price < input$my_slider[2],
               Price > input$my_slider[1],
               Type == input$my_select)
    }
  )



  output$my_plot <- renderPlot(
    filtered() %>%
    ggplot(aes(Alcohol_Content)) + geom_histogram()
  )
  output$my_table <- renderTable(
    filtered()
  )
}

shinyApp(ui = ui, server = server)

