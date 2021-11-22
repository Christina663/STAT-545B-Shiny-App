library(shiny)
library(tidyverse)
bcl <- read.csv("bcl-data.csv")
options(shiny.autoreload = TRUE)

ui <- fluidPage(FILL_THIS_IN)

server <- function(input, output) {
  FILL_THIS_IN
}

shinyApp(ui = ui, server = server)

