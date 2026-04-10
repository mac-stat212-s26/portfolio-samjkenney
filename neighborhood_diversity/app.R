#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Neighborhood Diversity"),
    selectInput("city", label= "City", choices = unique(data_by_dist$metro_name)),
    sliderInput("smooth", label = "Smoothness", min = 0, max = 1, value = 0.5),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distToHall"),
           plotOutput("diversityMap"),
           plotOutput("raceDistribution")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    output$distToHall <- renderPlot({
      ggplot(data = data_by_dist |> filter(metro_name == input$city), aes(x = entropy, y = distmiles)) +
        geom_point() +
        geom_smooth(span = input$smooth)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
