library(e1071)
library(XML)

load("data/traindata.RData")

spieltag <- data.frame(readHTMLTable(doc = "http://www.bulibox.de/statistik/bundesliga-ergebnisse.php?liga=1&spieltag=1"))[, 2:3]

predictdata <- traindata[1:9,]

predictdata$team_a <- spieltag[,1]
predictdata$team_b <- spieltag[,2]

predictdata$saison_weight <- 13
predictdata$saison <- "2012_13"

predictdata[,11:20] <- as.integer(0)

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

traindata$hg_01 <- as.numeric(traindata$heim_gast)
predictdata$hg_01 <- as.numeric(predictdata$heim_gast)

traindata$choose <- 0
predictdata$choose <- 1

all_data <- rbind(traindata, predictdata)

### SVM trainieren
svm_sun <- svm(sun_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = all_data[all_data$choose == 0,], scale = T, probability = T)

predict_sun <- predict(svm_sun, all_data[all_data$choose == 1,], probability = F)
predict_sun
predict_sun <- predict(svm_sun, all_data[all_data$choose == 1,], probability = T)
predict_sun