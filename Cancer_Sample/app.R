library(datateachr)
library(tidyverse)
library(lubridate)
library(shiny)
edited_games <- read.csv(here::here("datasets", "edited_games.csv"))
options(shiny.autoreload = TRUE)

ui <- fluidPage(
  titlePanel("Steam Games Visualization"),
  "Explanations go here...",
  sidebarLayout(
    sidebarPanel(
      sliderInput("my_slider", "Select a date range:", min = 1981, max = 2025, value = c(2000, 2021)),
      selectInput("my_select", "Select a price category:", choices = c("cheap", "medium", "expensive", "extremely expensive"))
    ),
    mainPanel(
      plotOutput("my_plot"),
      tableOutput("my_table")
    )
  )
)


server <- function(input, output) {

  games <- reactive ({
    steam_games %>%
      select(types, name, release_date, developer, publisher, original_price) %>%
      mutate(standardized_date = year(mdy(release_date)),
             categorical_price = case_when(
               original_price < 10 ~ "cheap",
               original_price < 30 ~ "medium",
               original_price < 50 ~ "expensive",
               original_price < 70 ~ "extremely expensive"
             )
      ) %>%
      filter(standardized_date < input$my_slider[2],
             standardized_date > input$my_slider[1],
             categorical_price == input$my_select)
    }
  )


  output$my_plot <- renderPlot(
    ggplot(games(), aes(standardized_date)) + geom_histogram(binwidth = 0.5)
  )
  output$my_table <- renderTable(
    select(games(), name, release_date, developer, original_price) %>%
    head(50)
  )
}

shinyApp(ui = ui, server = server)

