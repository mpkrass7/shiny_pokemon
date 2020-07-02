# Shiny
library(shiny)
library(shinyalert)
library(shinythemes)

#Data Munging
library(dplyr)
library(stringr)
library(tidyr)

#Databases
library(DBI)
library(RSQLite)

#Plotting
library(ggplot2)
library(plotly)

#Data table configuration
dt_config <- list(pageLength=10,
                  colunDefs = list(list(className='dt-center', targets= '_all')))

con <- dbConnect(RSQLite::SQLite(), 'data/pokemon_db.db')
poke_data <- dbReadTable(con, 'pokemon_data')
dbDisconnect(con)

# UI Function
ui <- navbarPage(selected = "Selector",
  title=div(tags$img(src="poke_ball.png", height =50),
                style="margin-top: -25px; padding:10px"),
  #theme = "journal",
  
  windowTitle="Pokemon Web App",
 tabPanel("Selector", 
  fluidPage(
    theme=shinythemes::shinytheme('flatly'),
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "pika_logo.png")
    ),
     useShinyalert(),
    tags$head(
        tags$style("
                  #abilities{
                  display:inline
                  }"),
        ),

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
                        choices=unique(poke_data$generation),
                        selected=1),
            
            uiOutput('pokemon_ui'),
            
            p("If this is one of your favorite Pokemon, press submit below"),
            div(actionButton('submit', "Submit Favorite"),
            imageOutput("pokemon_image", inline = T), style='margin-top:-10px')
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           div(plotlyOutput('bar_comp'),style="background: margin-top:-85x; border-style: groove; padding-right:10px"),
           div(
               h5("Abilities:", style="display:inline"),
               textOutput('abilities')
           ),
        )
    )
)),
    tabPanel("Survey Display",
             fluidPage(
                 fluidRow(
                   column(width=6,
                          h3('Top 10 Pokemon By Votes'),
                          plotlyOutput('survey_by_pokemon'),
                   ),
                   column(width=6,
                          h3('Count of Favorites by Generation'),
                          plotlyOutput('survey_by_generation'),
                   )
                 ),
                 fluidRow(
                   h3("Survey Results"),
                   DT::dataTableOutput('survey_overall')
                 )
            )
     ),
    tabPanel("Analysis",
             fluidPage(
                 h3("Pokemon Clustering Will go here")
             ))

)

#  Server Function
server <- function(input, output, session) {
    
    # Choose a pokemon
    output$pokemon_ui <- renderUI({
        choices <- poke_data[which(poke_data$generation==input$poke_gen),'name']
        selectInput("pokemon_name",
                    "Choose a Pokemon:",
                    choices = choices)
    })
    
    # Dataset filtered for the selected pokemon
    selected_pokemon <- reactive({
        poke_selection <- poke_data[which(poke_data$name == input$pokemon_name),]
    })
    
    # Create Image
    output$pokemon_image <- renderImage({
        req(input$pokemon_name)
        # filename is ./images/`pokemon_name`.png
        # Won't work with the .jpg images which I should fix at some point
        filename <- normalizePath(file.path('./images',
                                            paste(tolower(input$pokemon_name), '.png', sep='')))
        
        # Return a list containing the filename
        list(src = filename,
             contentType = 'image/png'
             ,width = 200,
             height = 200
             )
    }, deleteFile = FALSE)
    
    #################
    # Input results #
    #################
    
    # List abilities
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
    
    # Create Butterfly Chart
    output$bar_comp <- renderPlotly({
        req(input$pokemon_name)
        stat_names <- c('Attack', 'Defense', 'Special Attack', 'Special Defense', 'Health Points', 'Speed')
        df <- selected_pokemon() %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            `colnames<-`(stat_names) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Pokemon')
        df_avg <- poke_data %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            summarise_all(list(mean)) %>%
            summarise_all(list(round)) %>%
            `colnames<-`(stat_names) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Average') %>%
            mutate(Value = -Value)
            
        df_full <- rbind(df,df_avg)
        print(df_full)
        the_order <- rev(unique(df_full$Stat))
        
        
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
            scale_x_discrete(limits = the_order) +
            scale_y_continuous(breaks = seq(-300, 300, 50),
                               labels = abs(seq(-300, 300, 50))) +
            labs(x = "", y = "") +
            ggtitle(paste0("Comparison of ", input$pokemon_name, " Stats Against Average")) +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(colour = 'white'),
                  plot.background = element_rect(fill =  "transparent",colour = NA)) +
            scale_fill_manual(values=c('#2a75bb','#ffcb05'))
        
        ggplotly(plot, tooltip = c("text")) %>%
            layout(legend = l)
    })
    
    # Modal Dialog
    observeEvent(input$submit, {
        results <- shinyalert(
            title = "Submit Pokemon?",
            text = sprintf("Click Confirm to submit %s as one of your favorites",input$pokemon_name),
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
            callbackR = function(value) {
              if(input$submission_alert){
                shinyalert(title="Congratulations! You submitted something",
                           type='success')}}
        )
    })
    
    # Write to database if confirmed
    observeEvent(input$submission_alert, {
        if (input$submission_alert) {
            submit_date <- as.character(Sys.time())
            df <- data.frame(generation = input$poke_gen, 
                             pokemon_name = input$pokemon_name,
                             submit_date = submit_date)
            
            #Connect to database and write Table
            con <- dbConnect(RSQLite::SQLite(), 'data/pokemon_db.db')
            dbWriteTable(con, 'poke_survey', df, append=T)
            dbDisconnect(con)
        }
    })
    
    ######################
    # Survey Display Tab #
    ######################
    
    #Update Survey Data
    survey_data <- reactive({
      input$submit
      con <- dbConnect(RSQLite::SQLite(), 'data/pokemon_db.db')
      poke_survey <- dbReadTable(con, 'poke_survey')
      dbDisconnect(con)
      poke_survey
    })
    
    output$survey_overall <- DT::renderDataTable({
      survey_data()
    }, rownames = FALSE,
    options = dt_config)
    
    output$survey_by_pokemon <- renderPlotly({
      
      data <- survey_data() %>%
        group_by(pokemon_name) %>%
        summarise(Votes = n()) %>%
        select(Votes, Pokemon = pokemon_name) %>%
        arrange(desc(Votes), desc(Pokemon))
      data <- data[1:10,]
      # print(data)
      order <- rev(unique(data$Pokemon))
      
      g <- ggplot(data, aes(x=Pokemon, y=Votes, fill = Votes,
                            text = paste0('Pokemon:', Pokemon,
                                          '<br>Votes: ', Votes))) +
        geom_col(color='black') + 
        scale_x_discrete(limits = order) + 
        coord_flip() +
        xlab("") + ylab("") +
        theme(legend.position = "none",
              panel.background = element_rect(colour = 'white'),
              plot.background = element_rect(fill =  "transparent",colour = NA))
      
      ggplotly(g, tooltip = c("text"))
      
    })
    
    output$survey_by_generation <- renderPlotly({
      data <- survey_data() %>%
        group_by(generation) %>%
        summarise(Votes= n()) %>%
        select(Votes, Generation = generation)
      
      g <- ggplot(data, aes(x=Generation, y=Votes, fill=Votes,
                            text = paste0('Generation:', Generation,
                                          '<br>Votes: ', Votes))) +
        geom_col(color = 'black') +
        ylab("") +
        theme(legend.position = "none",
              panel.background = element_rect(colour = 'white'),
              plot.background = element_rect(fill =  "transparent",colour = NA))
      ggplotly(g, tooltip = c("text"))
      
    })
}
    

# Run the application 
shinyApp(ui = ui, server = server)
