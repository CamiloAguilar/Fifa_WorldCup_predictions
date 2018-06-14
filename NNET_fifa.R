#****************************************************************************************************************
### Modelo de predicción partidos mundialistas
#****************************************************************************************************************
options(scipen = 999)
rm(list = ls()); gc()
library(dplyr); library(nnet)

#***************************************
## 1. Definición base trainning ####
#***************************************
fifa_teams <- readRDS("./RDS/fifa_team_def.rds")
fifa_matches <- read.table("./data/Matches_fifa.csv", sep=";", header = T)[,-1]

fifa_matches <- merge(fifa_matches, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
fifa_matches <- merge(fifa_matches, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)
p <- fifa_matches %>% filter(is.na(Rating.y))

fifa_matches <- fifa_matches %>% filter(!is.na(Rating.y))
p <- fifa_matches %>% filter(is.na(Rating.y))

# labels
y_win <- ifelse(fifa_matches$Victoria_A==1, 1, 0)
y_tied <- ifelse(fifa_matches$Empate==1, 1, 0)
y_lost <- ifelse(fifa_matches$Victoria_A==0 & fifa_matches$Empate==0, 1, 0)

set.seed(13062018)
ind.test <- sample(1:nrow(fifa_matches), floor(0.3*nrow(fifa_matches)), replace = FALSE)
test <- fifa_matches[ind.test, -(1:4)]
y_win_test <- y_win[ind.test]
y_tied_test <- y_tied[ind.test]
y_lost_test <- y_lost[ind.test]
y_test <- ifelse(y_win_test==1, "win", 
                 ifelse(y_tied_test==1, "tied", "lost"))

train <- fifa_matches[-ind.test, -(1:4)]
y_win_train <- y_win[-ind.test]
y_tied_train <- y_tied[-ind.test]
y_lost_train <- y_lost[-ind.test]
y_train <- ifelse(y_win_train==1, "win", 
                 ifelse(y_tied_train==1, "tied", "lost"))


#***************************************
## 2. Entrenamiento ####
#***************************************

### Centrado de datos
centro <- colMeans(train)
train <- train - matrix(centro, nrow(train), ncol(train), byrow=TRUE)
test <- test - matrix(centro, nrow(test), ncol(test), byrow=TRUE)

## Componentes principales
pc <- prcomp(train, center = F) 
pc_train <- pc$x
pc_test <- predict(pc, test)

## Observamos a continuación la varianza ganada con diferente cantidad de componentes principales
var_ganada <- cumsum(pc$sdev^2)/sum(pc$sdev^2)
plot(var_ganada)
abline(h=0.5, col="brown2")
abline(h=0.8, col="brown3")
abline(h=0.9, col="brown4")
abline(h=0.95, col="green4")
abline(h=0.99, col="green2")

which(var_ganada>0.5)[1] # 2 componentes
which(var_ganada>0.8)[1] # 5 componentes
which(var_ganada>0.9)[1] # 10 componentes
which(var_ganada>0.95)[1] # 17 componentes
which(var_ganada>0.99)[1] # 38 componentes

#********************
## Red neuronal
#********************

## Componentes a usar
componentes <- 10 # >> 90% de la varianza
iteraciones <- 1:5

net_win <- list()
net_tied <- list()
net_lost <- list()

require(progress)
pb <- progress_bar$new(total = length(iteraciones))
for(i in iteraciones){
     net_win[[i]] <- nnet(pc_train[, 1:componentes], y_win_train, size=10, trace=F, maxit=500, MaxNWts=2000)
     net_tied[[i]] <- nnet(pc_train[, 1:componentes], y_tied_train, size=10, trace=F, maxit=500, MaxNWts=2000)
     net_lost[[i]] <- nnet(pc_train[, 1:componentes], y_lost_train, size=10, trace=F, maxit=500, MaxNWts=2000)
     pb$tick()
}

## Guardamos los resultados
saveRDS(net_win, "./RDS/net_win.rds")
saveRDS(net_tied, "./RDS/net_tied.rds")
saveRDS(net_lost, "./RDS/net_lost.rds")


#***************************************
## 3. pruebas ####
#***************************************
pred_win <- NULL
pred_tied <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_test[, 1:componentes])
     pred_tied[[i]] <- predict(net_tied[[i]], pc_test[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_test[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_tied <- rowMeans(data.frame(pred_tied[[1]], pred_tied[[2]], pred_tied[[3]], pred_tied[[4]], pred_tied[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
resultados <- data.frame(real_label=y_test, res_win, res_tied, res_lost)
resultados$final_predict <- apply(resultados[,2:4], 1, which.max)
resultados$final_predict <- ifelse(resultados$final_predict==1, "win",
                                   ifelse(resultados$final_predict==2, "tied", "lost"))


#***************************************
## 4. Predicciones FIFA ####
#***************************************

## Fase de grupos
fase_grupos <- read.table("./data/Matches_Groups.csv", sep=";", header = T)[,-1]
fase_grupos <- merge(fase_grupos, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
fase_grupos <- merge(fase_grupos, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)

encuentros <- fase_grupos[,c(2,1)]
fase_grupos <- fase_grupos[,-(1:2)]

fase_grupos <- fase_grupos - matrix(centro, nrow(fase_grupos), ncol(fase_grupos), byrow=TRUE)
pc_fase_grupos <- predict(pc, fase_grupos)

pred_win <- NULL
pred_tied <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_fase_grupos[, 1:componentes])
     pred_tied[[i]] <- predict(net_tied[[i]], pc_fase_grupos[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_fase_grupos[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_tied <- rowMeans(data.frame(pred_tied[[1]], pred_tied[[2]], pred_tied[[3]], pred_tied[[4]], pred_tied[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
res_fase_grupos <- cbind(encuentros, data.frame(res_win, res_tied, res_lost))
res_fase_grupos$final_predict <- apply(res_fase_grupos[,3:5], 1, which.max)
res_fase_grupos$final_predict <- ifelse(res_fase_grupos$final_predict==1, "win",
                                   ifelse(res_fase_grupos$final_predict==2, "tied", "lost"))

write.table(res_fase_grupos, "./results/res_fase_grupos.csv", row.names = F, sep=";")


#************************
## Octavos de final
#************************
octavos <- read.table("./data/Matches_Oct.csv", sep=";", header = T)[,-1]
octavos <- merge(octavos, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
octavos <- merge(octavos, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)

encuentros <- octavos[,c(2,1)]
octavos <- octavos[,-(1:2)]

octavos <- octavos - matrix(centro, nrow(octavos), ncol(octavos), byrow=TRUE)
pc_octavos <- predict(pc, octavos)

pred_win <- NULL
pred_tied <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_octavos[, 1:componentes])
     pred_tied[[i]] <- predict(net_tied[[i]], pc_octavos[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_octavos[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_tied <- rowMeans(data.frame(pred_tied[[1]], pred_tied[[2]], pred_tied[[3]], pred_tied[[4]], pred_tied[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
res_octavos <- cbind(encuentros, data.frame(res_win, res_tied, res_lost))
res_octavos$final_predict <- apply(res_octavos[,3:5], 1, which.max)
res_octavos$final_predict <- ifelse(res_octavos$final_predict==1, "win",
                                        ifelse(res_octavos$final_predict==2, "tied", "lost"))

write.table(res_octavos, "./results/res_octavos.csv", row.names = F, sep=";")

#************************
## Cuartos de final
#************************
cuartos <- read.table("./data/Matches_quarters.csv", sep=";", header = T)[,-1]
cuartos <- merge(cuartos, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
cuartos <- merge(cuartos, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)

encuentros <- cuartos[,c(2,1)]
cuartos <- cuartos[,-(1:2)]

cuartos <- cuartos - matrix(centro, nrow(cuartos), ncol(cuartos), byrow=TRUE)
pc_cuartos <- predict(pc, cuartos)

pred_win <- NULL
pred_tied <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_cuartos[, 1:componentes])
     pred_tied[[i]] <- predict(net_tied[[i]], pc_cuartos[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_cuartos[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_tied <- rowMeans(data.frame(pred_tied[[1]], pred_tied[[2]], pred_tied[[3]], pred_tied[[4]], pred_tied[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
res_cuartos <- cbind(encuentros, data.frame(res_win, res_tied, res_lost))
res_cuartos$final_predict <- apply(res_cuartos[,3:5], 1, which.max)
res_cuartos$final_predict <- ifelse(res_cuartos$final_predict==1, "win",
                                    ifelse(res_cuartos$final_predict==2, "tied", "lost"))

write.table(res_cuartos, "./results/res_cuartos.csv", row.names = F, sep=";")


#************************
## Semifinal
#************************
semis <- read.table("./data/Matches_semis.csv", sep=";", header = T)[,-1]
semis <- merge(semis, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
semis <- merge(semis, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)

encuentros <- semis[,c(2,1)]
semis <- semis[,-(1:2)]

semis <- semis - matrix(centro, nrow(semis), ncol(semis), byrow=TRUE)
pc_semis <- predict(pc, semis)

pred_win <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_semis[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_semis[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
res_semis <- cbind(encuentros, data.frame(res_win, res_lost))
res_semis$final_predict <- apply(res_semis[,3:4], 1, which.max)
res_semis$final_predict <- ifelse(res_semis$final_predict==1, "win", "lost")

write.table(res_semis, "./results/res_semis.csv", row.names = F, sep=";")


#************************
## Final
#************************
final <- read.table("./data/Matches_final.csv", sep=";", header = T)[,-1]
final <- merge(final, fifa_teams, by.x = "Equipo_A", by.y = "Country", all.x = T, all.y = F)
final <- merge(final, fifa_teams, by.x = "Equipo_B", by.y = "Country", all.x = T, all.y = F)

encuentros <- final[,c(2,1)]
final <- final[,-(1:2)]

final <- final - matrix(centro, nrow(final), ncol(final), byrow=TRUE)
pc_final <- predict(pc, final)

pred_win <- NULL
pred_lost<- NULL

for(i in iteraciones){
     pred_win[[i]] <- predict(net_win[[i]], pc_final[, 1:componentes])
     pred_lost[[i]] <- predict(net_lost[[i]], pc_final[, 1:componentes])
}

## Se define a continuación el resultado como el promedio de los resultados individuales de cada iteración
res_win <- rowMeans(data.frame(pred_win[[1]], pred_win[[2]], pred_win[[3]], pred_win[[4]], pred_win[[5]]))
res_lost <- rowMeans(data.frame(pred_lost[[1]], pred_lost[[2]], pred_lost[[3]], pred_lost[[4]], pred_lost[[5]]))

## La tabla a continuación define el resultado final como la máxima probabilidad enconttrada
res_final <- cbind(encuentros, data.frame(res_win, res_lost))
res_final$final_predict <- apply(res_final[,3:4], 1, which.max)
res_final$final_predict <- ifelse(res_final$final_predict==1, "win", "lost")

write.table(res_final, "./results/res_final.csv", row.names = F, sep=";")

#***********
## Ranking 
#***********
## De acuerdo a la calidad de los jugadores, se toma el Rating de cada equipo como indicador para
## determinar las posiciones dentro del mundial, para aquellas posiciones no definidas directamente dentro
## del modelo
teams32 <- c("Argentina", "Australia", "Belgium", "Brazil", "Colombia", "Costa Rica", "Croatia", "Denmark", "Egypt", 
             "England", "France", "Germany", "Iceland", "Iran", "Japan", "Korea Republic", "Mexico", "Morocco", "Nigeria", 
             "Panama", "Peru", "Poland", "Portugal", "Russia", "Saudi Arabia", "Senegal", "Serbia", "Spain", "Sweden", 
             "Switzerland", "Tunisia", "Uruguay")

rating <- fifa_teams %>% filter(Country %in% teams32) %>%
          arrange(desc(Rating)) 

write.table(rating, "./data/rating.csv", sep=";", row.names = F)

