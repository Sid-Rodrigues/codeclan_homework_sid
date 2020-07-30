
library(shiny)
library(tidyverse)
library(CodeClanData)
library(ggthemes)
library(ggimage)
library(shinyWidgets)

source("global.R")

all_teams <- unique(olympics_overall_medals$team)
# UI section 
ui <- fluidPage(
    setBackgroundImage(
        src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Olympic_rings_without_rims.svg/1200px-Olympic_rings_without_rims.svg.png"
    ),
    titlePanel("Five Country Medal Comparison"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("season",
                         "Summer or Winter Olympics?",
                         choices = c("Summer", "Winter")),
            radioButtons("medal",
                         "Gold, Silver or Bronze?",
                         choices = c("Gold", "Silver", "Bronze"))
           
        ),
        
        mainPanel(
            plotOutput("medal_plot")
        )
       
    )
)

# Server section 
server <- function(input, output) {
    output$medal_plot <- renderPlot({
        
        plot_medal(input$season, input$medal) 
        
    }, bg = "transparent")
}
# Run the app
shinyApp(ui = ui, server = server)