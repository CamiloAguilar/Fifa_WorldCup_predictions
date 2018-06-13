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

## Fifaindex
players_1 <- read.csv2("./scraping/Full_details_p1.csv", encoding = "UTF-8")
players_2 <- read.csv2("./scraping/Full_details_p2.csv", encoding = "UTF-8")

players <- rbind(players_1, players_2)
players$Name <- toupper(str_replace_all(players$Name, "-", " "))
players$last_name <- sapply(players$Name, function(x){
     last_name <- unlist(str_split(x, " "))
     last_name[length(last_name)]
})
players$key <- str_replace_all(paste0(players$Nationality, players$last_name), " ", "")


#*********************************
## 2. Tidy data - jugadores ####
#*********************************

## Llaves Fifaindex
fifa_py1 <- players %>%
            filter(Name %in% team_py$Nombre.Jugador) %>%
            mutate(llave=Name)

no_merge <- team_py %>%
            filter(!(Nombre.Jugador %in% players$Name)) %>%
            mutate(key1 = str_replace_all(paste0(Selección, Apellido1), " ", ""),
                   key2 = str_replace_all(paste0(Selección, Apellido2), " ", ""))

fifa_py2 <- players %>%
            filter(key %in% no_merge$key1) %>%
            mutate(llave=key)

fifa_py3 <- players %>%
            filter(key %in% no_merge$key2) %>%
            mutate(llave=key)

no_merge <- no_merge %>%
            filter(!(key1 %in% players$key) & !(key2 %in% players$key))

fifa_py <- rbind(fifa_py1, fifa_py2, fifa_py3) %>%
           select(-(key), -(last_name))

## llaves team_py
team_py <- team_py %>%
           mutate(key1 = str_replace_all(paste0(Selección, Apellido1), " ", ""),
                  key2 = str_replace_all(paste0(Selección, Apellido2), " ", "")) %>%
           mutate(llave = ifelse(Nombre.Jugador %in% fifa_py$llave, Nombre.Jugador,
                                 ifelse(key1 %in% fifa_py$llave, key1,
                                        ifelse(key2 %in% fifa_py$llave, key2, 
                                               "sin llave"))),
                  Convocado = 1) %>%
           select(llave, Nombre.Jugador, Convocado, Posición, Titular, Suplente, Ingreso)


## Base jugadores definitiva
fifa_players <- merge(team_py, fifa_py, by = "llave", all.x = T, all.y=T)

df <- data.frame(llave="sin llave", Nombre.Jugador=players$Name, Convocado=0, Posición=NA, 
                 Tituar=NA, Suplente=NA, Ingreso=NA)
players <- cbind(df, players)
players <- players %>%
           filter(!(Name %in% fifa_players$Name)) %>%
           select(-(key), -(last_name))
names(players) <- names(fifa_players)
fifa_players <- rbind(fifa_players, players)


#*********************************
## 3. Tidy data - equipos ####
#*********************************

positions <- fifa_players %>% 
             mutate(Position=ifelse(is.na(as.character(National_Position)), as.character(Club_Position), 
                                    as.character(National_Position))) %>% 
             group_by(Posición, Position) %>%
             summarise(n())

fifa_team_py <- fifa_py %>%



fifa_team_stat <- df

