library(shiny)
library(DBI)
library(RSQLite)
library(shinyalert)
library(jsonlite)
library(dplyr)
library(stringr)
library(plotly)
library(tidyr)

con <- dbConnect(RSQLite::SQLite(), 'pokemon_db.db')
poke_data <- dbReadTable(con, 'pokemon_data')

# UI Function
ui <- navbarPage("Clerb is Crey",
     tabPanel("Selector",
  fluidPage(
     useShinyalert(),
    tags$head(
        tags$style("
                  #abilities{
                  display:inline
                  }")),

    # Application title
    fluidRow(
        column(width=4,
               titlePanel("Pokemon Selector"),
               div(p('Choose a Pokemon and compare their base stats against 
      the average in the dataset'),style='width:100px; display:inline'))
    ),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('poke_gen',
                        'Generation',
                        choices=poke_data$generation,
                        selected=1),
            
            uiOutput('pokemon_ui'),
            
            p("If this is one of your favorite Pokemon, press submit below"),
            div(actionButton('submit', "Submit Favorite"),
            imageOutput("pokemon_image", inline = T), style='margin-top:-10px')
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           div(plotlyOutput('bar_comp'),style="margin-top:-85x;"),
           div(
               h5("Abilities:", style="display:inline"),
               textOutput('abilities')
           ),
        )
    )
)),
    tabPanel("Survey Display",
             fluidPage(
                 h3("Database will go here")
            )
     ),
    tabPanel("Analysis",
             fluidPage(
                 h3("Pokemon Clustering Will go here")
             ))

)

#  Server Function
server <- function(input, output, session) {
    
    selected_pokemon <- reactive({
        poke_selection <- poke_data[which(poke_data$name == input$pokemon_name),]
    })
    
    observeEvent(input$submit, {
        results <- shinyalert(
            title = "Submit Pokemon?",
            text = "Click Confirm to Submit",
            closeOnEsc = TRUE, 
            closeOnClickOutside = TRUE,
            html = TRUE,
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Submit",
            confirmButtonCol = "#539BBD",
            cancelButtonText = "Cancel",
            inputId = 'submission_alert',
            callbackR = function(value) { shinyalert(paste("Congratulations! You submitted something")) }
        )
    })
    
    observeEvent(input$submission_alert, {
        if (input$submission_alert) {
            submit_date <- as.character(Sys.time())
            df <- data.frame(generation = input$poke_gen, 
                             pokemon_name = input$pokemon_name,
                             submit_date = submit_date)
            dbWriteTable(con, 'poke_survey', df, append=T)
        }
    })
    
    output$pokemon_ui <- renderUI({
        choices <- poke_data[which(poke_data$generation==input$poke_gen),'name']
        selectInput("pokemon_name",
                    "Choose a Pokemon:",
                    choices = choices)
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
            ggtitle(paste0("Comparison of ", input$pokemon_name, " Stats Against Average")) +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill =  "white")) + 
            scale_fill_manual(values=c('#2a75bb','#ffcb05'),
                              name="",
                              breaks=c(input$pokemon_name, "Average"),
                              labels=c(input$pokemon_name, "Average"))
        
        ggplotly(plot, tooltip = c("text")) %>%
            layout(legend = l)
    })
    
    output$pokemon_image <- renderImage({
        req(input$pokemon_name)
        # filename is ./images/`pokemon_name`.png
        # Won't work with the .jpg images which I should fix at some point
            filename <- normalizePath(file.path('./images',
                                      paste(tolower(input$pokemon_name), '.png', sep='')))
            
            # Return a list containing the filename
            list(src = filename)
          }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
