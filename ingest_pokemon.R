library(dplyr)
library(DBI)
library(RSQLite)

poke_data <- read.csv('pokemon.csv', stringsAsFactors = F)
con <- dbConnect(RSQLite::SQLite(), 'pokemon_db.db')
dbWriteTable(con, 'pokemon_data', poke_data, overwrite=T)
str(dbReadTable(con, 'pokemon_data'))

other_poke_data <- read.csv('data/pokemon_with_ids.csv')
evo_data <- read.csv('data/pokemon_evolution.csv')
other_poke_data
evo_data
