# **************************************************
# ************ Preparación Datos *******************
# **************************************************

options(scipen=999)
library(dplyr)
library(reshape2)
library(stringr)

#*********************************
## 1. lectura datos ####
#*********************************

team_py <- read.csv("./data/Players by team.csv", encoding="UTF-8")
team_py$Nombre.Jugador <- toupper(str_replace_all(team_py$Nombre.Jugador, "-", " "))
team_py$Apellido1 <- sapply(team_py$Nombre.Jugador, function(x){
     last_name <- unlist(str_split(x, " "))
     last_name[length(last_name)]
})

team_py$Apellido2 <- sapply(team_py$Nombre.Jugador, function(x){
     last_name <- unlist(str_split(x, " "))
     last_name[1]
})

players <- read.csv2("./scraping/Full_details.csv", encoding = "UTF-8")
players$Name <- toupper(str_replace_all(players$Name, "-", " "))
players$last_name <- sapply(players$Name, function(x){
     last_name <- unlist(str_split(x, " "))
     last_name[length(last_name)]
})
players$key <- str_replace_all(paste0(players$Nationality, players$last_name), " ", "")


## Tidy data - jugadores
fifa_py1 <- players %>%
            filter(Name %in% team_py$Nombre.Jugador)

no_merge <- team_py %>%
            filter(!(Nombre.Jugador %in% players$Name)) %>%
            mutate(key1 = str_replace_all(paste0(Selección, Apellido1), " ", ""),
                   key2 = str_replace_all(paste0(Selección, Apellido2), " ", ""))

fifa_py2 <- players %>%
            filter(key %in% no_merge$key1 | key %in% no_merge$key2)

no_merge <- no_merge %>%
            filter(!(key1 %in% players$key) & !(key2 %in% players$key))

fifa_py <- rbind(fifa_py1, fifa_py2)
rm(fifa_py1, fifa_py2)



