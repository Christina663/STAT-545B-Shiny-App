# Load in packages
library(datateachr)
library(tidyverse)
library(lubridate)
library(shiny)
options(shiny.autoreload = TRUE)

# UI to organize how components appear on the app
ui <- fluidPage(
  titlePanel("Steam Games Trend Visualization"),
  "This is a Shiny App demonstrating the number of games released on Steam each year.",
  "The histogram shows a trend of gradual increased number of games as time goes by, with a peak occuring around 2018.
  The table displays the plotted data, including the name, release date, developer and original price of the games.
  Users are welcomed to interact with the plot and table: You can drag the slide bar to change the range of release year, and
  you can also use the select menu to filter the price range of games. The plot and table update automatically!",

  sidebarLayout(
    sidebarPanel(
      # Feature 1: Slider for customizing the range of release years of the games
      sliderInput("my_slider", "Select a range of release year:", min = 1981, max = 2025, value = c(1991, 2021)),
      # Feature 2: Select menu for filtering games in one specific price category
      selectInput("my_select", "Select a price category:", choices = c("cheap", "medium", "expensive", "extremely expensive"))
    ),
    mainPanel(
      # Feature 3a: Interactive histogram plot to show the trend of number of games released each year
      plotOutput("my_plot"),
      # Feature 3b: Interactive table containing data of the games being plotted, with the name, release date, developer and original price of the games.
      tableOutput("my_table")
    )
  )
)

# Server to formulate the components to feed into UI
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
    ggplot(games(), aes(standardized_date)) + geom_histogram(binwidth = 0.5, color = "pink", fill = "blue", alpha = 0.3)+ labs(x = "Release Year", y = "Number of Games")
  )
  output$my_table <- renderTable(
    select(games(), name, release_date, developer, original_price) %>%
    head(50)
  )
}

# Shiny App line
shinyApp(ui = ui, server = server)

