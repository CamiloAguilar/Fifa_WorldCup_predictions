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
players_3 <- read.csv2("./scraping/Full_details_p3.csv", encoding = "UTF-8")
players_4 <- read.csv2("./scraping/Full_details_p4.csv", encoding = "UTF-8")

players <- rbind(players_1, players_2, players_3, players_4)
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
fifa_players$National_Position <- ifelse(fifa_players$National_Position=="" | is.na(fifa_players$National_Position), NA, 
                                         as.character(fifa_players$National_Position))
fifa_players$Club_Position <- ifelse(fifa_players$Club_Position=="" | is.na(fifa_players$Club_Position), NA, 
                                         as.character(fifa_players$Club_Position))

fifa_players <- fifa_players %>% 
                mutate(Position=ifelse(is.na(National_Position), Club_Position, National_Position))
names(fifa_players)

## Definición de posiciones de juego
positions <- fifa_players %>% 
             group_by(Posición, Position) %>%
             summarise(n())
write.table(positions, "./data/positions.csv", sep=";", row.names = F)
positions <- read.table("./data/aux_positions.csv", sep=";", header = T)

fifa_players <- merge(fifa_players, positions, by="Position", all.x = T) %>%
                mutate(Posición = ifelse(!is.na(as.character(Posición)), as.character(Posición), 
                                         as.character(Posición_def))) %>%
                select(-(Position), -(Posición_def)) %>%
                group_by(Nombre.Jugador) %>%
                summarise(llave=first(llave), Nationality=first(na.omit(Nationality)), Convocado=first(na.omit(Convocado)), 
                          Posición=first(first(na.omit(Posición))), Titular=first(Titular), 
                          Suplente=first(na.omit(Suplente)), Ingreso=first(na.omit(Ingreso)),
                          Rating=max(Rating, na.rm=T), Height=first(na.omit(Height)), Weight=first(na.omit(Weight)),
                          PFoot=first(na.omit(Preffered_Foot)), Age=first(na.omit(Age)), Work_Rate=first(na.omit(Work_Rate)),
                          Weak_foot=max(Weak_foot, na.rm = T), Skill_Moves=max(Skill_Moves, na.rm = T), 
                          Ball_Control=max(Ball_Control, na.rm = T), Dribbling=max(Dribbling, na.rm = T), 
                          Marking=max(Marking, na.rm = T), Sliding_Tackle=max(Sliding_Tackle, na.rm = T),
                          Standing_Tackle=max(Standing_Tackle, na.rm = T), Aggression=max(Aggression, na.rm = T), 
                          Reactions=max(Reactions, na.rm = T), Attacking_Position=max(Attacking_Position, na.rm = T), 
                          Interceptions=max(Interceptions, na.rm = T), Vision=max(Vision, na.rm = T),
                          Composure=max(Composure, na.rm = T), Crossing=max(Crossing, na.rm = T), 
                          Short_Pass=max(Short_Pass, na.rm = T), Long_Pass=max(Long_Pass, na.rm=T), 
                          Acceleration=max(Acceleration, na.rm = T), Speed=max(Speed, na.rm = T), 
                          Stamina=max(Stamina, na.rm=T), Strength=max(Strength, na.rm = T), Balance=max(Balance, na.rm = T),
                          Agility=max(Agility, na.rm = T), Jumping=max(Jumping, na.rm = T), Heading=max(Heading, na.rm = T), 
                          Shot_Power=max(Shot_Power, na.rm = T), Finishing=max(Finishing, na.rm = T), 
                          Long_Shots=max(Long_Shots, na.rm = T), Curve=max(Curve, na.rm = T), 
                          Freekick_Accuracy=max(Freekick_Accuracy, na.rm=T), Penalties=max(Penalties, na.rm = T), 
                          Volleys=max(Volleys, na.rm = T), GK_Positioning=max(GK_Positioning, na.rm = T), 
                          GK_Diving=max(GK_Diving, na.rm = T), GK_Kicking=max(GK_Kicking, na.rm = T), 
                          GK_Handling=max(GK_Handling, na.rm = T), GK_Reflexes=max(GK_Reflexes, na.rm = T))

fifa_players$Height <- sapply(fifa_players$Height, function(x){
     last_name <- unlist(str_split(x, " "))
     as.numeric(last_name[1])
})

fifa_players$Weight <- sapply(fifa_players$Weight, function(x){
     last_name <- unlist(str_split(x, " "))
     as.numeric(last_name[1])
})


saveRDS(fifa_players, file = "./RDS/fifa_players.rds")

#*********************************
## 3. Tidy data - equipos ####
#*********************************

## Arqueros
fifa_team_stat_arq <- fifa_players %>%
                      filter(Posición=="Arquero") %>%
                      arrange(Nationality, desc(Convocado), desc(Rating)) %>%
                      group_by(Nationality) %>%
                      top_n(n=1, wt=Rating) 

## Titulares
fifa_team_stat_tit <- fifa_players %>%
                      filter(Posición!="Arquero") %>%
                      arrange(Nationality, desc(Convocado), desc(Rating)) %>%
                      group_by(Nationality) %>%
                      top_n(n=10, wt=Rating)

## Suplencia
fifa_team_stat_sup <- fifa_players %>%
     filter(!(Nombre.Jugador %in% fifa_team_stat_tit$Nombre.Jugador)) %>%
     arrange(Nationality, desc(Convocado), desc(Rating)) %>%
     group_by(Nationality) %>%
     top_n(n=11, wt=Rating)

fifa_team_def <- rbind(fifa_team_stat_arq, fifa_team_stat_tit) %>%
                 group_by(Country=Nationality) %>%
                 summarise(Rating=mean(Rating, na.rm = T), Height=mean(Height, na.rm = T), Weight=mean(Weight, na.rm=T),
                           Age=mean(Age, na.rm = T), Weak_foot=mean(Weak_foot, na.rm = T), 
                           Skill_Moves=mean(Skill_Moves, na.rm = T), Ball_Control=mean(Ball_Control, na.rm = T), 
                           Dribbling=mean(Dribbling, na.rm = T), 
                           Marking=mean(Marking, na.rm = T), Sliding_Tackle=mean(Sliding_Tackle, na.rm = T),
                           Standing_Tackle=mean(Standing_Tackle, na.rm = T), Aggression=mean(Aggression, na.rm = T), 
                           Reactions=mean(Reactions, na.rm = T), Attacking_Position=mean(Attacking_Position, na.rm = T), 
                           Interceptions=mean(Interceptions, na.rm = T), Vision=mean(Vision, na.rm = T),
                           Composure=mean(Composure, na.rm = T), Crossing=mean(Crossing, na.rm = T), 
                           Short_Pass=mean(Short_Pass, na.rm = T), Long_Pass=mean(Long_Pass, na.rm=T), 
                           Acceleration=mean(Acceleration, na.rm = T), Speed=mean(Speed, na.rm = T), 
                           Stamina=mean(Stamina, na.rm=T), Strength=mean(Strength, na.rm = T), 
                           Balance=mean(Balance, na.rm = T),
                           Agility=mean(Agility, na.rm = T), Jumping=mean(Jumping, na.rm = T), 
                           Heading=mean(Heading, na.rm = T), 
                           Shot_Power=mean(Shot_Power, na.rm = T), Finishing=mean(Finishing, na.rm = T), 
                           Long_Shots=mean(Long_Shots, na.rm = T), Curve=mean(Curve, na.rm = T), 
                           Freekick_Accuracy=mean(Freekick_Accuracy, na.rm=T), Penalties=mean(Penalties, na.rm = T), 
                           Volleys=mean(Volleys, na.rm = T), GK_Positioning=mean(GK_Positioning, na.rm = T), 
                           GK_Diving=mean(GK_Diving, na.rm = T), GK_Kicking=mean(GK_Kicking, na.rm = T), 
                           GK_Handling=mean(GK_Handling, na.rm = T), GK_Reflexes=mean(GK_Reflexes, na.rm = T)) %>%
                arrange(desc(Rating))
                      
saveRDS(fifa_team_def, file = "./RDS/fifa_team_def.rds")


#p <- players %>% filter(Nationality=="Albania")

