library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)
library(dplyr)
library(stringr)
library(plotly)
library(tidyr)

con <- dbConnect(RSQLite::SQLite(), 'pokemon_db.db')
poke_data <- dbReadTable(con, 'pokemon_data')

# UI Function
ui <- fluidPage(tags$head(tags$style("
                  #abilities{
                  display:inline
                  }")),

    # Application title
    titlePanel("Pokemon Selector"),
    p('Choose a Pokemon and compare their base stats against 
      the average in the dataset'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('poke_gen',
                        'Generation',
                        choices=poke_data$generation,
                        selected=1),
            
            uiOutput('pokemon_ui'),
            
            p("If this is your favorite pokemon, press submit below"),
            actionButton('submit', "Submit Favorite"),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput('bar_comp'),
           div(
               h5("Abilities:", style="display:inline"),
               textOutput('abilities')
           ),
        )
    )
)

#  Server Function
server <- function(input, output) {
    observeEvent(input$submit, {
        print(input$pokemon)
        print(input$gen)
        submit_date <- as.character(Sys.time())
        df <- data.frame(generation = input$poke_gen, 
                         pokemon_name = input$pokemon_name,
                         submit_date = submit_date)
        dbWriteTable(con, 'poke_survey', df, append=T)
    })
    
    output$pokemon_ui <- renderUI({
        choices <- poke_data[which(poke_data$generation==input$poke_gen),'name']
        selectInput("pokemon_name",
                    "Choose a Pokemon:",
                    choices = choices)
    })
    
    selected_pokemon <- reactive({
        poke_selection <- poke_data[which(poke_data$name == input$pokemon_name),]
    })
    
    output$abilities <- renderText({
        req(input$pokemon_name)
        # print(selected_pokemon())
        ability <- selected_pokemon()$abilities
        ability <- strsplit(ability, ",")[[1]]
        ability <- str_replace_all(ability, "\\[", "")
        ability <- str_replace_all(ability, "\\]", "")
        ability <- str_replace_all(ability, "' ", "")
        ability <- paste(str_replace_all(ability, "'", ""), sep = ',')
        
        return(ability)
    })
    
    output$bar_comp <- renderPlotly({
        req(input$pokemon_name)
        df <- selected_pokemon() %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Pokemon')
        df_avg <- poke_data %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            summarise_all(list(mean)) %>%
            summarise_all(list(round)) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Average') %>%
            mutate(Value = -Value)
            
    
        df_full <- rbind(df,df_avg)

        l <- list(
            font = list(
                family = "sans-serif",
                size = 12,
                color = "#000"),
            x = -.001, y = 0, orientation = 'h')
        
        plot <- df_full %>% 
            ggplot(aes(x = Stat, y = Value, group = side, fill = side,
            text = paste0(ifelse(side=='Average', 'Group Average', input$pokemon_name),
                         '<br>', Stat, ': ', abs(Value)
                         ))) + 
            geom_bar(stat = "identity", width = 0.75) +
            coord_flip() +#Make horizontal instead of vertical
            scale_x_discrete(limits = df$Stat) +
            scale_y_continuous(breaks = seq(-300, 300, 50),
                               labels = abs(seq(-300, 300, 50))) +
            labs(x = "", y = "") +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill =  "grey90"))
            # scale_fill_manual(values=c("red", "blue"),
            #                   name="",
            #                   breaks=c("Pokemon", "Average"),
            #                   labels=c("Pokemon", "Average"))
        
        ggplotly(plot, tooltip = c("text")) %>%
            layout(legend = l)
            # # reverse the order of items in legend
            # # guides(fill = guide_legend(reverse = TRUE)) +
            # # change the default colors of bars

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
