library(shiny)
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), 'pokemon_db.db')
poke_data <- dbReadTable(con, 'pokemon_data')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pokemon Datset"),
    p('This page will be where users can submit a Pokemon'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('gen',
                        'Generation',
                        choices=poke_data$generation,
                        selected=1),
            
            uiOutput('pokemon_ui'),
            
            actionButton('submit', "Submit Favorite"),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$submit, {
        print(input$pokemon)
        print(input$gen)
        submit_date <- as.character(Sys.time())
        df <- data.frame(generation = input$gen, 
                         pokemon_name = input$pokemon,
                         submit_date = submit_date)
        dbWriteTable(con, 'poke_survey', df, append=T)
    })
    
    output$pokemon_ui <- renderUI({
        choices <- poke_data[which(poke_data$generation==input$gen),'name']
        selectInput("pokemon",
                    "Choose a Pokemon:",
                    choices = choices)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
