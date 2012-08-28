library(e1071)
library(rpart)
#library(FSelector)
library(ggplot2)

load("data/traindata.RData")

# cm <- function (actual, predicted) {
#    # Produce a confusion matrix
#    
#    t<-table(predicted,actual)
#    # there is a potential bug in here if columns are tied for ordering
#    t[apply(t,2,function(c) order(-c)[1]),] 
# }


# Heim und Gast in 0 und 1 umwandeln
traindata$hg_01 <- as.numeric(traindata$heim_gast)

train <- traindata[traindata$saison != "2011_12",]
testdata <- traindata[traindata$saison == "2011_12",]
#testdata <- traindata[traindata$saison == "2011_12" & traindata$heim_gast == "Heim",]


#test <- knn(train[,c(3,5,11:15)], testdata[,c(3,5,11:15)], train[,9], k = 1)



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

   #testdata$wins_temp_a[testdata$spieltag ==i] <- tempdata$wins_temp_a
   #testdata$wins_temp_b[testdata$spieltag ==i] <- tempdata$wins_temp_b
   
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

###############
### ReliefF ###

#select_sun <- relief(sun_a ~ team_a + team_b + tore_a + tore_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = traindata, neighbours.count= 5, sample.size = 50)
#select_tore_a <- relief(tore_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = traindata, neighbours.count= 5, sample.size = 50)
#select_tore_b <- relief(tore_b ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = traindata, neighbours.count= 5, sample.size = 50)

#subset_sun <- cutoff.k.percent(select_sun, 1)
#formula_sun <- as.simple.formula(subset_sun, "sun_a")
# Formula: sun_a ~ team_a + place_a + tied_a + loses_a + hg_01 + loses_b + points_pro_a + points_a + wins_pro_a + wins_a

###########
### SVM ###

#tuned <- tune.svm(sun_a ~ spieltag + saison_weight + place_a + place_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = train, gamma = 10^(-6:-1), cost = 10^(1:2))

svm_sun <- svm(sun_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = train, probability = TRUE)
#svm_sun <- svm(formula_sun, data = train)
#svm_tore_a <- svm(as.factor(tore_a) ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = train, probability = TRUE)
#svm_tore_b <- svm(tore_b ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + points_a + points_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b + wins_a + tied_a + loses_a + wins_b + tied_b + loses_b, data = train)
#svm_all <- svm(sun_a ~ spieltag + saison_weight + hg_01, data = train, cost = 100, gamma = 1)

predict_sun <- predict(svm_sun, testdata)
tab_sun <- table(pred = predict_sun, true = testdata$sun_a)
tab_sun
# 99!!! Prozent richtig, aber keine Unentschieden getippt

#predict_tore_a <- predict(svm_tore_a, testdata)
#tab_tore_a <- table(pred = predict_tore_a, true = testdata$tore_a)
#tab_tore_a
#cor(predict_tore_a, testdata$tore_a)

testdata <- cbind(testdata, predict_sun)
testdata$right <- 0

for (i in 1:nrow(testdata)) {
   if (testdata$sun_a[i] == testdata$predict_sun[i]) {
      testdata$right[i] <- 1
   }
}
 
ddply(.data=testdata, .(spieltag), mean)[,29]

testdata$mean <- testdata

plot_svm <- ggplot(testdata, aes(x = spieltag, y =right)) +  geom_line(aes(group = team_a))
plot_svm




###################
### Naive Bayes ###

nb_all <- naiveBayes(sun_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = train)

predict_nb_all <- predict(nb_all, testdata[,c(1,2,3,5,10,12,19,14,16,13,15)])

tab_nb <- table(pred = predict_nb_all, true = testdata$sun_a)
# etwa 30 Prozent richtig!


######################
### Decision trees ###

dt_all <- rpart(sun_a ~ team_a + team_b + spieltag + saison_weight + place_a + place_b + hg_01 + wins_pro_a + wins_pro_b + points_pro_a + points_pro_b, data = train)

?predict

cm(testdata$sun_a, predict_all)

1 - sum(diag(m)) / sum(m)

predict(s, testdata)
traceback()
?predict.svm
## Werder gegen HSV

train_2 <- traindata[traindata$team_a == "SV Werder Bremen" & traindata$team_b == "Hamburger SV" & traindata$saison != "2011_12",]
testdata_2 <- traindata[traindata$team_a == "SV Werder Bremen" & traindata$team_b == "Borussia Dortmund" & traindata$saison == "2011_12",]

test_2 <- knn(train_2[,c(3,5,11:15)], testdata_2[,c(3,5,11:15)], train_2[,9], k = 1)

table(test_2, testdata_2$sun_a)


#s_2 <- svm(sun_a ~ spieltag + saison_weight + as.numeric(place_a) + as.numeric(points_a) + as.numeric(place_b) + as.numeric(points_b) + hg_01, data = train_2, cost = 100, gama = 1)
s_2 <- svm(sun_a ~ spieltag + saison_weight + as.numeric(place_a) +  as.numeric(place_b) + hg_01, data = train_2, cost = 100, gama = 1)
test_2 <- predict(s, testdata_2)
table(test_2, testdata_2$sun_a)