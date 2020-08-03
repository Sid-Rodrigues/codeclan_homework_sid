library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(CodeClanData)
library(datasets)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Game Sales Info"),
    
    tabsetPanel(
        tabPanel(
            "Search",
            
            fluidRow(
                column(3,
                       radioButtons('rating',
                                    'Select Rating',
                                    choices = c("E", "E10+", "T", "M"),
                                    inline = TRUE)
                ),
                column(3,
                       selectInput("genre",
                                   "Select Genre",
                                   choices = NULL
                                   )
                ),
                column(3,
                       selectInput('platform',
                                    'Select Platform',
                                    choices = NULL
                                   )
                                    
                ),
                
                column(3,
                       selectInput('year',
                                    'Select Year',
                                   choices = NULL)
                ),
                
            ),
            
            fluidRow(
                column(6,
                checkboxGroupInput("checked", 
                                   "Select Fields",
                                   choices = c("Publisher", "Developer", "Sales", "Critic Score", "User Score"),
                                   inline = TRUE),
                )
            ),
            
            fluidRow(
                column(12,
                   tableOutput("game_table"))
                        
            
            )
        ),

        tabPanel(
        "Sales",
        fluidRow(
            column(width = 6, offset = 7,
                   selectInput('year_sales',
                               'Select Year',
                               choices = sort(unique(game_sales$year_of_release)))
            )
            
        ),
        
        fluidRow(
            column(6,
                   plotOutput("total_sales")),
            
            column(6,
                   plotOutput("year_sales"))
            
            
        )
        ),
        
        tabPanel(
            "Popularity",
            fluidRow(
                column(6,
                       radioButtons('rating_popularity',
                                    'Select Rating',
                                    choices = c("E", "E10+", "T", "M"),
                                    inline = TRUE)
                ),
                
                column(6,
                       selectInput('year_popularity',
                                   'Select Year',
                                   choices = NULL)
                )
                
            ),
            
            fluidRow(
                column(12,
                       plotOutput("top_five_games")
                )
            ),
            
            fluidRow(
                column(12,
                       plotOutput("top_five_publishers")
                )
            
            )
            
        )
)
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #updating genre selectInput based on rating
    observe({ 
        filtered_genre <- game_sales %>%
            filter(rating == input$rating) %>%
            select(genre)
        updateSelectInput(session, "genre", "Select Genre", choices = unique(filtered_genre))
    })
    
    #updating platform selectInput based on rating and genre
    observe({ 
        filtered_platform <- game_sales %>%
            filter(rating == input$rating) %>%
            filter(genre == input$genre) %>%
            select(platform)
        updateSelectInput(session, "platform", "Select Platform", choices = unique(filtered_platform))
    })
    
    #updating year selectInput based on rating, genre and platform 
    observe({
        filtered_year <- game_sales %>%
            filter(rating == input$rating) %>%
            filter(genre == input$genre) %>%
            filter(platform == input$platform) %>%
            select(year_of_release) %>%
            arrange(year_of_release)
        updateSelectInput(session, "year", "Select Year", choices = unique(filtered_year))
    })
    
    
    #reactive function for popularity tab
    top_five_filtered <- reactive({
        game_sales %>%
            select(name, platform, year_of_release, sales, rating, publisher) %>%
            filter(rating == input$rating_popularity) %>%
            filter(year_of_release == input$year_popularity) %>% 
            group_by(rating) %>%
            arrange(desc(sales)) %>%
            slice(seq_len(5))
        
    })
    
    output$game_table <- renderTable({
        game_sales %>%
            filter(rating == input$rating) %>%
            filter(genre == input$genre) %>%
            filter(platform == input$platform) %>%
            filter(year_of_release == input$year) %>%  
            slice(1:10)
    })
    
    output$total_sales <- renderPlot({ 
        game_sales %>%
            select(year_of_release, sales, rating) %>%
            group_by(rating) %>%
            arrange(desc(sales)) %>%
            ggplot() +
            aes(x = year_of_release, y = sales, fill = rating) +
            geom_bar(width = 0.5, stat = "identity") +
            #theme_minimal() +
            theme(panel.grid.minor = element_blank()) +
            scale_x_continuous(breaks = c(1988 , 1996, 2000, 2005, 2010, 2015)) +
            scale_y_continuous(breaks = c(1,5, 10, 20, 30, 40, 50, 100, 125, 150, 175, 200, 225)) +
            labs(
                x = "\nYear of release",
                y = "Sales in millions",
                title = "Total Global Sales by Year"
            )
    })
    
    output$year_sales <- renderPlot({ 
        game_sales %>%
            filter(year_of_release == input$year_sales) %>%
            group_by(rating) %>%
            summarise(total_sales = sum(sales)) %>%
            mutate(percent = total_sales/sum(total_sales)*100) %>%
            ggplot() +
            aes(x = "", y = total_sales, fill = rating) +
            geom_bar(stat = "identity") + 
            coord_polar("y") +
            theme_void() +
            labs(
                x = "\nYear of release",
                y = "Sales in millions",
                title = "Yearly Global Sales by Rating"
            ) +
            geom_text(aes(x = 1.6, label = paste0(round(percent, 1), "%")), 
                      size = 4, position = position_stack(vjust = 0.5)) 
    })
    
    #Popularity Tab
    #updating genre selectInput based on rating
    observe({ 
        filtered_year_popularity <- game_sales %>%
            filter(rating == input$rating_popularity) %>%
            select(year_of_release) %>%
            arrange(year_of_release)
        updateSelectInput(session, "year_popularity", "Select Year", choices = unique(filtered_year_popularity))
    })
    
    output$top_five_games <- renderPlot({ 
            ggplot(top_five_filtered()) +
            aes(x = name, y = sales, fill = platform) +
            geom_bar(width = 0.5, stat = "identity", position = "dodge") +
            #theme_minimal() +
            theme(panel.grid.minor = element_blank()) +
            labs(
                x = "\nGames",
                y = "Sales in millions",
                title = "Top 5 Games by Year "
            )
    })
    
    output$top_five_publishers <- renderPlot({ 
        ggplot(top_five_filtered()) +
        aes(x = publisher, y = sales, fill = platform) +
        geom_bar(width = 0.5, stat = "identity", position = "dodge") +
        #theme_minimal() +
        theme(panel.grid.minor = element_blank()) +
        labs(
            x = "\nPublishers",
            y = "Sales in millions",
            title = "Top 5 Publishers by Year"
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
