library(e1071)
library(XML)

load("data/traindata.RData")

spieltag <- data.frame(readHTMLTable(doc = "http://www.bulibox.de/statistik/bundesliga-ergebnisse.php?liga=1&spieltag=6"))[, 2:3]
table <- data.frame(readHTMLTable(doc = "http://www.bulibox.de/tabelle/dynamische-bundesligatabelle.php?ab=1&bis=5&liga=1&Abfrage=OK"), stringsAsFactors=FALSE)[, 1:6]
names(table) <- c("place", "team", "games", "sun", "goals", "points")

sun <- strsplit(as.character(table$sun), split = "-")

wins <- as.numeric(unlist(lapply(sun, function(x) x[1])))
ties <- as.numeric(unlist(lapply(sun, function(x) x[2])))
loses <- unlist(lapply(sun, function(x) x[3]))
loses <- as.numeric(substr(loses, 1, 2))

table <- cbind(table, wins, ties, loses)

table$place <- as.character(table$place)

for (i in 1:nrow(table)) {
   table$place[i] <- substr(as.character(table$place[i]), 1, nchar(table$place[i]) -1)
}

table$place <- factor(table$place)


table$team <- as.character(table$team)

for (i in 1:nrow(table)) {
   table$team[i] <- substr(table$team[i], 1, nchar(table$team[i]) - 1)
}

table$team <- factor(table$team)

## create predictdata
predictdata <- traindata[1:9,]

predictdata$team_a <- spieltag[,1]
predictdata$team_b <- spieltag[,2]

predictdata$saison_weight <- 13
predictdata$saison <- "2012_13"

predictdata$spieltag <- 6

predictdata[,11:20] <- as.integer(0)

## Change Names etc.
predictdata$team_a <- gsub("ü", "u", predictdata$team_a)
predictdata$team_a <- gsub("ö", "o", predictdata$team_a)
predictdata$team_a <- gsub("ä", "a", predictdata$team_a)

predictdata$team_b <- gsub("ü", "u", predictdata$team_b)
predictdata$team_b <- gsub("ö", "o", predictdata$team_b)
predictdata$team_b <- gsub("ä", "a", predictdata$team_b)

predictdata$team_a[predictdata$team_a == "Sport-Club Freiburg"] <- "SC Freiburg"
predictdata$team_b[predictdata$team_b == "Sport-Club Freiburg"] <- "SC Freiburg"

predictdata$team_a <- factor(predictdata$team_a)
predictdata$team_b <- factor(predictdata$team_b)


table$team <- gsub("ü", "u", table$team)
table$team <- gsub("ö", "o", table$team)
table$team <- gsub("ä", "a", table$team)

table$team[table$team == "Sport-Club Freiburg"] <- "SC Freiburg"

table$team <- factor(table$team)


## match table into predictdata
for (i in predictdata$team_a) {
   tempdata <- table[table$team == i,]
   
   predictdata$wins_a[predictdata$team_a == i] <- as.numeric(as.character(tempdata$wins))
   predictdata$tied_a[predictdata$team_a == i] <- as.numeric(as.character(tempdata$ties))
   predictdata$loses_a[predictdata$team_a == i] <- as.numeric(as.character(tempdata$loses))
   predictdata$points_a[predictdata$team_a == i] <- as.integer(as.character(tempdata$points))
   predictdata$place_a[predictdata$team_a == i] <- as.integer(as.character(tempdata$place))
}

for (i in predictdata$team_b) {
   tempdata <- table[table$team == i,]
   
   predictdata$wins_b[predictdata$team_b == i] <- as.numeric(as.character(tempdata$wins))
   predictdata$tied_b[predictdata$team_b == i] <- as.numeric(as.character(tempdata$ties))
   predictdata$loses_b[predictdata$team_b == i] <- as.numeric(as.character(tempdata$loses))
   predictdata$points_b[predictdata$team_b == i] <- as.integer(as.character(tempdata$points))
   predictdata$place_b[predictdata$team_b == i] <- as.integer(as.character(tempdata$place))
}


## calculate percentage values
predictdata$wins_pro_a <- predictdata$wins_a / 2
predictdata$points_pro_a <- predictdata$points_a / (2 * 3)

predictdata$wins_pro_b <- predictdata$wins_b / 2
predictdata$points_pro_b <- predictdata$points_b / (2 * 3)


## Home/Guest
traindata$hg_01 <- as.numeric(traindata$heim_gast)
predictdata$hg_01 <- as.numeric(predictdata$heim_gast)


## define Traindata and Predictdata
traindata$choose <- 0
predictdata$choose <- 1

all_data <- rbind(traindata, predictdata)

### SVM trainieren
svm_sun <- svm(sun_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = all_data[all_data$choose == 0,], scale = T, probability = T)

predict_sun <- predict(svm_sun, all_data[all_data$choose == 1,], probability = F)
predict_sun
predict_sun2 <- predict(svm_sun, all_data[all_data$choose == 1,], probability = T)
predict_sun2

cbind(predictdata[,1:2], predict_sun)
predicted_games <- cbind(as.character(predictdata$team_a), as.character(predictdata$team_b), as.character(predict_sun))
predicted_games