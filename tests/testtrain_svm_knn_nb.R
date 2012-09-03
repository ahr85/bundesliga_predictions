library(e1071)
library(ggplot2)
library(MASS)
library(sqldf)


load("data/traindata.RData")

# home and guest into 0 and 1
traindata$hg_01 <- as.numeric(traindata$heim_gast)

# Teamname into a number
all_team <- factor(unique(c(as.character(traindata$team_a))))
all_team_num <- as.numeric(all_team)

all_team_df <- data.frame(cbind(as.character(all_team), all_team_num))
names(all_team_df) <- c("team", "team_nr")

traindata <- merge(traindata, all_team_df, by.x = "team_b", by.y = "team")
colnames(traindata)[26] <- "team_b_nr"

traindata <- merge(traindata, all_team_df, by.x = "team_a", by.y = "team")
colnames(traindata)[27] <- "team_a_nr"


## split data

train <- traindata[traindata$saison != "2011_12",]
testdata <- traindata[traindata$saison == "2011_12",]

## set folling variables to 0/1 etc in the testdata as ist must be calculated first
# 0 points before the first game
testdata$points_a <- 0
testdata$points_b <- 0
# every teams shares first place
testdata$place_a <- 1
testdata$place_b <- 1
# no likelihoods to win
testdata$wins_pro_a <- 0
testdata$wins_pro_b <- 0
# no likelihood of points
testdata$points_pro_a <- 0
testdata$points_pro_b <- 0
# wins, tied, loses to 0
testdata$wins_a <- 0
testdata$tied_a <- 0
testdata$loses_a <- 0
testdata$wins_b <- 0
testdata$tied_b <- 0
testdata$loses_b <- 0

## now lets calculate for the other matchdays

# temporal values
#testdata$wins_temp_a <- as.integer(0)
#testdata$wins_temp_b <- as.integer(0)
testdata$points_temp_a <- 0
testdata$points_temp_b <- 0

# here all the values according to the actual matchday are calculated
for (i in 2:34) {
   
   tempdata <- testdata[testdata$spieltag == i, ]
   tempdata_last <- testdata[testdata$spieltag == i - 1, ]
   
   for (ii in tempdata$team_a) {
      
      # calculate points and wins for team_a (always home)
      if (tempdata_last$sun_a[tempdata_last$team_a == ii] == "S") {
         # won last game: point + 3, wins + 1, tied same, loses same
         tempdata$wins_a[tempdata$team_a == ii] <- tempdata_last$wins_a[tempdata_last$team_a == ii] + 1
         tempdata$tied_a[tempdata$team_a == ii] <- tempdata_last$tied_a[tempdata_last$team_a == ii]
         tempdata$loses_a[tempdata$team_a == ii] <- tempdata_last$loses_a[tempdata_last$team_a == ii]
         
         tempdata$points_temp_a[tempdata$team_a == ii] <- tempdata_last$points_temp_a[tempdata_last$team_a == ii] + 3
      } else if (tempdata_last$sun_a[tempdata_last$team_a == ii] == "U") {
         # tied last game: points + 1; wins same, tied +1, loses same
         tempdata$wins_a[tempdata$team_a == ii] <- tempdata_last$wins_a[tempdata_last$team_a == ii]
         tempdata$tied_a[tempdata$team_a == ii] <- tempdata_last$tied_a[tempdata_last$team_a == ii] + 1
         tempdata$loses_a[tempdata$team_a == ii] <- tempdata_last$loses_a[tempdata_last$team_a == ii]
         
         tempdata$points_temp_a[tempdata$team_a == ii] <- tempdata_last$points_temp_a[tempdata_last$team_a == ii] + 1
      } else {
         # lost last game: points same, wins same, tied same, loses + 1
         tempdata$wins_a[tempdata$team_a == ii] <- tempdata_last$wins_a[tempdata_last$team_a == ii]
         tempdata$tied_a[tempdata$team_a == ii] <- tempdata_last$tied_a[tempdata_last$team_a == ii]
         tempdata$loses_a[tempdata$team_a == ii] <- tempdata_last$loses_a[tempdata_last$team_a == ii] + 1
         
         tempdata$points_temp_a[tempdata$team_a == ii] <- tempdata_last$points_temp_a[tempdata_last$team_a == ii]
         
      }

      # calculate points and wins for team_b (always guest)
      if (tempdata_last$sun_b[tempdata_last$team_b == ii] == "S") {
         # won last game: point + 3, wins + 1
         tempdata$wins_b[tempdata$team_b == ii] <- tempdata_last$wins_b[tempdata_last$team_b == ii] + 1
         tempdata$tied_b[tempdata$team_b == ii] <- tempdata_last$tied_b[tempdata_last$team_b == ii]
         tempdata$loses_b[tempdata$team_b == ii] <- tempdata_last$loses_b[tempdata_last$team_b == ii]
         
         tempdata$points_temp_b[tempdata$team_b == ii] <- tempdata_last$points_temp_b[tempdata_last$team_b == ii] + 3
      } else if (tempdata_last$sun_b[tempdata_last$team_b == ii] == "U") {
         # tied last game: points + 1; wins simply copied
         tempdata$wins_b[tempdata$team_b == ii] <- tempdata_last$wins_b[tempdata_last$team_b == ii]
         tempdata$tied_b[tempdata$team_b == ii] <- tempdata_last$tied_b[tempdata_last$team_b == ii] + 1
         tempdata$loses_b[tempdata$team_b == ii] <- tempdata_last$loses_b[tempdata_last$team_b == ii]
         
         tempdata$points_temp_b[tempdata$team_b == ii] <- tempdata_last$points_temp_b[tempdata_last$team_b == ii] + 1
      } else {
         # lost last game: simply copy values
         tempdata$wins_b[tempdata$team_b == ii] <- tempdata_last$wins_b[tempdata_last$team_b == ii]
         tempdata$tied_b[tempdata$team_b == ii] <- tempdata_last$tied_b[tempdata_last$team_b == ii]
         tempdata$loses_b[tempdata$team_b == ii] <- tempdata_last$loses_b[tempdata_last$team_b == ii] + 1
         
         tempdata$points_temp_b[tempdata$team_b == ii] <- tempdata_last$points_temp_b[tempdata_last$team_b == ii]
         
      }
      
   }
   
   # get the tempdata values back to the testdata
   testdata$points_temp_a[testdata$spieltag ==i] <- tempdata$points_temp_a
   testdata$points_temp_b[testdata$spieltag ==i] <- tempdata$points_temp_b

   testdata$wins_pro_a[testdata$spieltag == i] <- tempdata$wins_a / i
   testdata$wins_pro_b[testdata$spieltag == i] <- tempdata$wins_b / i
   
   testdata$points_pro_a[testdata$spieltag == i] <- tempdata$points_temp_a / (3 * i)
   testdata$points_pro_b[testdata$spieltag == i] <- tempdata$points_temp_b / (3 * i)
   
   #place through points (double places are possible)
   testdata$place_a[testdata$spieltag == i] <- as.numeric(factor(tempdata$points_temp_a, levels = rev(levels(factor(tempdata$points_temp_a)))))
   testdata$place_b[testdata$spieltag == i] <- as.numeric(factor(tempdata$points_temp_b, levels = rev(levels(factor(tempdata$points_temp_b)))))
   
   # wins, tied and loses back
   testdata$wins_a[testdata$spieltag == i] <- tempdata$wins_a
   testdata$tied_a[testdata$spieltag == i] <- tempdata$tied_a
   testdata$loses_a[testdata$spieltag == i] <- tempdata$loses_a

   testdata$wins_b[testdata$spieltag == i] <- tempdata$wins_b
   testdata$tied_b[testdata$spieltag == i] <- tempdata$tied_b
   testdata$loses_b[testdata$spieltag == i] <- tempdata$loses_b
   
}


# we do only need half of the games
testdata <- testdata[testdata$heim_gast == "Heim",]


###########
### SVM ###
### 49% ###

svm_sun <- svm(sun_a ~ team_a_nr + team_b_nr + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = train, probability = TRUE)

predict_svm_sun <- predict(svm_sun, testdata)
tab_svm_sun <- table(pred = predict_svm_sun, true = testdata$sun_a)
tab_svm_sun


###################
### Naive Bayes ###
### 42 %!       ###

nb_sun <- naiveBayes(sun_a ~ team_a_nr + team_b_nr + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = train)

testdata_2 <- testdata[,c(26,27,3,5,17,19,18,20,25,21,23,22,24,11,12,13,14,15,16)]

predict_nb_sun <- predict(nb_sun, testdata_2)
tab_nb_sun <- table(pred = predict_nb_sun, true = testdata$sun_a)
tab_nb_sun


###########
### knn ###
### 29% ###

testdata_2 <- testdata[,c(26,27,3,5,17,19,18,20,25,21,23,22,24,11,12,13,14,15,16)]
traindata_2 <- traindata[,c(26,27,3,5,17,19,18,20,25,21,23,22,24,11,12,13,14,15,16)]

knn_sun <- knn(traindata_2, testdata_2, traindata[,9], k =2)
predict_knn_sun <- knn_sun
tab_knn_sun <- table(pred = predict_knn_sun, true = testdata$sun_a)
tab_knn_sun


###########################################
### Plotting Barplot all the algorithms ###

true <- c("S", "U", "N")

tables <- c("tab_svm_sun", "tab_nb_sun", "tab_knn_sun")

for (i in tables) {

   for (ii in 1:ncol(get(i))) {
      
      temp <- get(i)[,ii]
      temp_names <- names(temp)
      
      temp_data <- data.frame(cbind(temp, temp_names, i, true[ii]))
      
      if (ii == 1) {
         temp_data_2 <- temp_data
      } else {
         temp_data_2 <- rbind(temp_data_2, temp_data)
      }
      
   }

   if (i == tables[1]) {
      plot_data <- temp_data_2
   } else {
      plot_data <- rbind(plot_data, temp_data_2)
   }
}

names(plot_data) <- c("count", "predicted", "algorithm", "real")

plot_data$right <- "right"

for (i in 1:nrow(plot_data)) {
   if (as.character(plot_data$predicted[i]) != as.character(plot_data$real[i])) { plot_data$right[i] <- "false" }
}

levels(plot_data$algorithm)[levels(plot_data$algorithm) == "tab_svm_sun"] <- "SVM"
levels(plot_data$algorithm)[levels(plot_data$algorithm) == "tab_nb_sun"] <- "Naive bayes"
levels(plot_data$algorithm)[levels(plot_data$algorithm) == "tab_knn_sun"] <- "knn"

plot_data$count <- as.numeric(levels(plot_data$count))[plot_data$count]


plot <- ggplot(plot_data, aes(x = real, group = right, fill = right)) + geom_bar(aes(y = count), stat = "identity", position = "dodge")
plot <- plot + facet_wrap(~ algorithm) + scale_fill_brewer("Predictions",type = "qual", palette= "Set1", breaks=c("right", "false"))
plot <- plot + scale_x_discrete(labels = c("Wins", "Ties", "Loses"))
plot <- plot + opts(axis.title.x = theme_blank(), axis.text.x = theme_text(size = 11, angle = 90))
plot

png(file= "plots/tests/compare_svm_nb_knn_bar.png", width = 500, height = 250)
   plot
dev.off()


#########################################
### Plotting lines over all matchdays ###

spieltag <- sort(rep(1:34, 9))

temp_1 <- cbind(spieltag, predict_svm_sun, testdata$sun_a, "SVM")
temp_2 <- cbind(spieltag, predict_nb_sun, testdata$sun_a, "nb")
temp_3 <- cbind(spieltag, predict_knn_sun, testdata$sun_a, "knn")
plot_data_2 <- data.frame(rbind(temp_1, temp_2, temp_3))

plot_data_2$right <- 0

for (i in 1:nrow(plot_data_2)) {
   if(plot_data_2$predict_svm_sun[i] == plot_data_2$V3[i]) { plot_data_2$right[i] <- 1 }
}

names(plot_data_2) <- c("spieltag", "predict", "real", "algorithmus",  "right")

plot_data_2 <- sqldf("select spieltag as matchday, algorithmus as algorithm, avg(right) as mean_right from plot_data_2
                     group by spieltag, algorithmus;")

plot_data_2$matchday <- as.integer(plot_data_2$matchday)


plot_2 <- ggplot(plot_data_2, aes(x = matchday, y = mean_right, group = algorithm, colour = algorithm)) + geom_line(size = .75, alpha = 0.5)
plot_2 <- plot_2 + scale_color_brewer(type = "qual", palette = "Set1") + geom_smooth(method = loess, se = F, size = 1.25)
plot_2

png(file = "plots/tests/compare_svm_nb_knn_time.png", width = 500, height = 250)
   plot_2
dev.off()