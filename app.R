# Shiny
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)

#Data Munging
library(tidyverse)

#Databases
library(DBI)
library(pool)
library(DT)
library(RMySQL)

#Plotting
library(ggplot2)
library(plotly)

#Analytics
library(RANN)
library(ggbiplot)

#Functions
source('pokelytics.R')

# PCA Function

#Data table configuration
dt_config <- list(pageLength=10,
                  colunDefs = list(list(className='dt-center', targets= '_all')))

con <- dbConnect(
  drv      = RMySQL::MySQL(),
  dbname   = "mpkrass_pokeShiny",
  host     = "johnny.heliohost.org",
  user = "mpkrass_admin",
  password= "mytestpassword!",
  port=3306
)
poke_data <- dbReadTable(con, 'pokemon_data') %>% filter(generation != 7)
dbDisconnect(con)
# Types
types <- unique(c(poke_data$type1, poke_data$type2))[1:18]
names(types) <- str_to_title(types)
features <- c('attack', 'defense', 'sp_attack', 'sp_defense', 'hp', 'speed', 'capture_rate')
names(features) <- c('Attack', 'Defense', 'Special Attack', 'Special Defense', 'Health Points', 'Speed', 'Capture Rate')
feature_df <- data.frame(feature_name=names(features), stringsAsFactors = F)
rownames(feature_df) <- features
# feature_df[c('attack','defense'), 'feature_name']
# UI Function
ui <- navbarPage(
  tags$div(tags$style(HTML(".dropdown-menu{z-index:10000 !important;"))),
  title=div(tags$img(src="poke_ball.png", height=50),
                style="margin-top: -25px; padding:10px"),
  windowTitle="Pokemon Web App",
  selected = "Selector",
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
            p('Use the checkboxes to change the average pokemon comparison on the right'),
            checkboxInput('match_legend', 'Match Legendary Status?', value=F),
            checkboxInput('match_type', 'Match Type?', value=F),
            checkboxInput('match_generation', 'Match Generation?', value=F),
            
            p("If this is one of your favorite Pokemon, press submit below"),
            div(actionButton('submit', "Submit Favorite"),
            imageOutput("pokemon_image", inline = T), style='margin-top:-10px')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           div(plotlyOutput('bar_comp'),style="background: margin-top:-85x; border-style: groove; padding-right:10px"),
           splitLayout(div(
               h5("Abilities:"),
               textOutput('abilities')
               ),
           div(
               h5("Type:"),
               textOutput('type')
           ))
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
             fluidPage(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
               fluidRow(h3("Run PCA to Compare Pokemon"),
                 splitLayout(cellWidths = c(rep(300,3), 250),
                    pickerInput('pca_generation', "Generation", 
                             choices=seq(1:6), selected=seq(1:6), 
                             multiple=T,width='100%',
                             options = list(`actions-box` = TRUE)),
                   div(pickerInput('pca_type', 'Type',
                               choices = types, selected=types,
                               multiple=T,width='100%',
                               options = list(`actions-box` = TRUE)), style='width: 250px; margin-left: 100px;'),
                   div(actionBttn('run_pca', 'Run PCA', style='jelly', color = 'success'), style='margin-left: 150px;'),
                   div(bsButton("q1", label = "", icon = icon("question"),
                            style = "info")),
                   bsPopover(id = "q1", title = "PCA", trigger='click',
                             content = paste0("PCA is a feature reduction method that takes linear combinations of variables to create fewer orthogonal features that capture as much of the variance as possible. See a ",
                                              a("PCA explanation here",  
                                                href = "https://builtin.com/data-science/step-step-explanation-principal-component-analysis",
                                                target="_blank")))
                 ),
                 #is a feature reduction method that takes 
                 #linear combinations of variables to create fewer orthogonal features
                 #that capture as much of the variance as possible
                
                 
                 plotlyOutput('poke_pca')
               ),
               fluidRow(h3("Find The Most Similar Pokemon by Euclidian Distance"),
                 splitLayout(cellWidths = c(rep(350,3), 250),
                    uiOutput('pokemon_nn_ui', width='100%'),
                    div(pickerInput('nn_features', '', 
                                choices = features, selected = features, 
                                multiple=T,width='100%',inline=F,
                                options = list(`action-box` = TRUE)), style='margin-top:20px;'),
                    div(actionBttn('run_nn', 'Find Closest Pokemon', style='jelly', color = 'success'),
                        style= 'margin-top:20px; margin-left: 50px'),
                    div(bsButton("q2", label = "", icon = icon("question"),
                                 style = "info"), style=' margin-top:20px;'),
                    bsPopover(id = "q2", title = "Nearest Neighbors", trigger='click',
                              content = paste0("Limiting by the first two Principal Componenets, the pokemon closest to each other in the scatter plot may not be the most similar. Instead we can use the",
                                               a("Euclidian Distance of the selected features.",  
                                                 href = "https://en.wikipedia.org/wiki/Euclidean_distance#:~:text=In%20mathematics%2C%20the%20Euclidean%20distance,metric%20as%20the%20Pythagorean%20metric.",
                                                 target="_blank")))
                    # div()
                    ),
                 DT::dataTableOutput('poke_nn')
    
               )
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
        ability <- selected_pokemon()$abilities
        ability <- strsplit(ability, ",")[[1]]
        ability <- str_replace_all(ability, "\\[", "")
        ability <- str_replace_all(ability, "\\]", "")
        ability <- str_replace_all(ability, "' ", "")
        ability <- paste(str_replace_all(ability, "'", ""), sep = ',')
        
        return(ability)
    })
    
    output$type <- renderText({
      req(input$pokemon_name)
      str_to_title(c(selected_pokemon()$type1, selected_pokemon()$type2))
    })
    
    # Create Butterfly Chart
    output$bar_comp <- renderPlotly({
        req(input$pokemon_name)
        stat_names <- names(features)[1:6]
        df <- selected_pokemon() %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            `colnames<-`(stat_names) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Pokemon')
        is_legend <- selected_pokemon()$is_legendary
        is_generation <- selected_pokemon()$generation
        is_type <- c(selected_pokemon()$type1, selected_pokemon()$type2)
        poke_filter <- poke_data
        if (input$match_legend){poke_filter <- filter(poke_filter, is_legendary==is_legend)}
        if (input$match_generation){poke_filter <- filter(poke_filter, generation==is_generation)}
        if (input$match_type){poke_filter <- filter(poke_filter, type1 %in% is_type | type2 %in% is_type)}
        df_avg <- poke_filter %>%
            select(attack, defense, sp_attack, sp_defense, hp, speed) %>%
            summarise_all(list(mean)) %>%
            summarise_all(list(round)) %>%
            `colnames<-`(stat_names) %>%
            gather("Stat", "Value") %>%
            mutate(side = 'Average') %>%
            mutate(Value = -Value)
            
        df_full <- rbind(df,df_avg)
        # print(df_full)
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
            layout(legend = l) %>%
          config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
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
            # print(df)
            #Connect to database and write Table
            con <- dbConnect(
              drv=RMySQL::MySQL(),
              dbname="mpkrass_pokeShiny",
              host="johnny.heliohost.org",
              user="mpkrass_admin",
              password="mytestpassword!",
              port=3306
            )
            
            query <- sprintf(
              "INSERT INTO poke_survey (%s) VALUES (%s, '%s', '%s')",
              paste(names(df), collapse = ", "),
              df[[1]],df[[2]],df[[3]]
            )
            # print(paste(df, collapse = "', '"))
            dbSendQuery(con,  query)
            dbDisconnect(con)
        }
    })
    
    ######################
    # Survey Display Tab #
    ######################
  
    #Update Survey Data
    survey_data <- reactive({
      input$submit
      con <- dbConnect(
        drv      = RMySQL::MySQL(),
        dbname   = "mpkrass_pokeShiny",
        host     = "johnny.heliohost.org",
        user = "mpkrass_admin",
        password= "mytestpassword!",
        port=3306
      )
      poke_survey <- dbReadTable(con, 'poke_survey')
      dbDisconnect(con)
      poke_survey
    })
    
    output$survey_by_pokemon <- renderPlotly({
      
      data <- survey_data() %>%
        group_by(pokemon_name) %>%
        tally() %>%
        select(Votes = n, Pokemon = pokemon_name) %>%
        arrange(desc(Votes), desc(Pokemon))
      data <- data[1:10,]
      # print(data)
      order <- rev(unique(data$Pokemon))
      
      g <- ggplot(data, aes(x=Pokemon, y=Votes, fill = Votes,
                            text = paste0('Pokemon:', Pokemon,
                                          '<br>Votes: ', Votes))) +
        geom_col(color='black') + 
        scale_x_discrete(limits = order) + 
        scale_y_continuous(breaks = integer_breaks()) + 
        coord_flip() +
        xlab("") + ylab("") +
        theme(legend.position = "none",
              panel.background = element_rect(colour = 'white'),
              plot.background = element_rect(fill =  "transparent",colour = NA))
      
      ggplotly(g, tooltip = c("text")) %>%
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
      
    })
    
    output$survey_by_generation <- renderPlotly({
      data <- survey_data() %>%
        group_by(generation) %>%
        tally() %>%
        select(Generation = generation, Votes=n)

      g <- ggplot(data, aes(x=Generation, y=Votes, fill=Votes,
                            text = paste0('Generation:', Generation,
                                          '<br>Votes: ', Votes))) +
        geom_col(color = 'black') +
        ylab("") +
        theme(legend.position = "none",
              panel.background = element_rect(colour = 'white'),
              plot.background = element_rect(fill =  "transparent",colour = NA))
      ggplotly(g, tooltip = c("text")) %>%
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
      
    })
    
    output$survey_overall <- DT::renderDataTable({
      dt <- survey_data()
      colnames(dt) <- c('Generation', 'Pokemon', 'Submit Date (GMT)')
      dt
    }, rownames = FALSE,
    options = dt_config)
    
    ##################
    ## Analysis Tab ##
    ##################
    
    pca_df <- reactiveValues()
    observeEvent(input$run_pca, {
      poke_data$name[c(29,32,122,439, 669)] <- c('mr-mime', 'nidoran-f', 'nidoran-m', 'mime-jr', 'flabebe')
      poke_sample <- poke_data %>% 
        filter(generation %in% input$pca_generation) %>% 
        filter(type1 %in% input$pca_type | type2 %in% input$pca_type)
      pca_df[['poke_sample']] <- poke_sample
    })
    
    pca_dataset <- reactive({
      req(input$run_pca)
      pca_df[['poke_sample']]
    })
    
    output$poke_pca <- renderPlotly({
      req(input$pokemon_nn_name)
      # print(input$pokemon_nn_name)  
      run_pca_plot(pca_dataset(), str_to_lower(input$pokemon_nn_name))
    })
    
    # Choose a pokemon
    output$pokemon_nn_ui <- renderUI({
      filter_data <- poke_data %>%
        filter(generation %in% input$pca_generation) %>% 
        filter(type1 %in% input$pca_type | type2 %in% input$pca_type)
      choices <- filter_data$name
      selectInput("pokemon_nn_name",
                  "Choose a Pokemon:",
                  selected = 'None',
                  choices = c('None', choices), width = 300)
    })
    
    nn_df <- reactiveValues()
    observeEvent(input$run_nn, {
      if (input$pokemon_nn_name == 'None') return()
      poke_sample <- poke_data %>% 
        filter(generation %in% input$pca_generation) %>% 
        filter(type1 %in% input$pca_type | type2 %in% input$pca_type) 
      feature_set <- input$nn_features
      poke_sample$capture_rate <- as.numeric(poke_sample$capture_rate)
      poke_scale <- as.data.frame(apply(poke_sample[,all_of(feature_set)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
      poke_scale$name <- poke_sample$name
      # print(head(poke_scale))
      
      pokemon <- poke_scale[which(poke_scale$name==input$pokemon_nn_name),]
      closest_idx <- nn2(poke_scale[,all_of(feature_set)], pokemon[,all_of(feature_set)], k=11)$nn.idx[1,]
      
      nn_table <- poke_sample[closest_idx,c('name', all_of(feature_set))]
      colnames(nn_table) <- c("Pokemon Name", feature_df[input$nn_features, 'feature_name'])
      nn_table$`Pokemon Index` <- row.names(nn_table)
      nn_df[['poke_sample']] <- nn_table[, c(ncol(nn_table),1:(ncol(nn_table)-1))]
    })
    
    nn_dataset <- reactive({
      req(input$run_nn)
      nn_df[['poke_sample']]
    })
    
    output$poke_nn <- DT::renderDataTable({
      nn_table <- nn_dataset()
    }, rownames = FALSE,
    options = list(pagelength=11, 
                             dom = 't',
                             colunDefs = list(list(className='dt-center', targets= '_all'))))
    
}
    

# Run the application 
shinyApp(ui = ui, server = server)
