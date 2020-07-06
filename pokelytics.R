integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

run_pca_plot <- function(poke_sample, pokemon_name){
  library(ggbiplot)
  features <- c('attack', 'defense', 'sp_attack', 'sp_defense', 'hp', 'speed', 'experience_growth', 'capture_rate')
  poke_pca_data <- poke_sample %>%
    select(all_of(features)) %>%
    mutate(capture_rate = as.numeric(capture_rate))
  #PCA
  poke_pca <- prcomp(poke_pca_data, center = T, scale. = T)
  pc_power1 <- round(data.frame(summary(poke_pca)[6])[2,1],3)*100
  pc_power2 <- round(data.frame(summary(poke_pca)[6])[2,2],3)*100
  
  g <- ggbiplot(poke_pca, ellipse=TRUE, obs.scale = 1, var.scale = 1, group=as.character(poke_sample$is_legendary)) %>%
    ggplot_build()
  
  pca_locs <- g$data[[2]]
  poke_names <- str_to_lower(poke_sample$name)
  pca_locs$pokemon <- poke_names
  
  pca_locs <- cbind(pca_locs, poke_sample[,all_of(features)])
  
  # Plotly Images of Pokemon
  poke_pics <- purrr::map_chr(
    pca_locs$pokemon, ~ base64enc::dataURI(file = sprintf("images/%s.png", .x))
  )
  # print(pca_locs$pokemon)
  pca_sel <- filter(pca_locs, pokemon==pokemon_name)
  
  a <- list(
    x = pca_sel$x,
    y = pca_sel$y,
    text = "X",
    xref = "x",
    yref = "y",
    ax = 0,
    ay = 0,
    font = list(color = 'red',
                family = 'arial',
                face="bold",
                size = 15),
    opacity = ifelse(pokemon_name=='none', 0,1)
  )
  
  # print(pca_locs)
  pca_locs$is_legendary <- ifelse(poke_sample$is_legendary == 1, "Legendary", "Normal")
  fig <- plot_ly(data = pca_locs, x=~x, y=~y, 
                 customdata = poke_pics, 
                 type = 'scatter',
                 mode = 'markers',
                 color = ~is_legendary, colors = c('red', 'blue'),
                 symbol = ~is_legendary, symbols = c('star','circle'),
                 text = ~paste("Pokemon: ",str_to_title(pokemon),
                               "<br> HP:", hp,
                               "<br> Attack:", attack,
                               "<br> Defense:", defense,
                               "<br> Sp Attack:", sp_attack,
                               "<br> Sp Defense:", sp_defense,
                               "<br> Speed:", speed)
  ) %>%
    htmlwidgets::onRender(readLines("js/tooltip-image.js"))
  fig %>% layout(xaxis = list(zeroline=F, title=sprintf("PC1: Explains %s%% Variance", pc_power1)),
                 yaxis = list(zeroline=F, title=sprintf("PC2: Explains %s%% Variance", pc_power2)),
                 legend = list(orientation = 'h'),
                 annotations=a)
}
# run_pca_plot(poke_data, generations, types)

