#library(plyr)
library(sqldf)

load("data/alle_spiele.RData")
load("data/endtables.RData")

traindata <- sqldf("select team_a, team_b, spieltag, a.saison, saison_weight, heim_gast, tore_a, tore_b, sun_a, sun_b, b.wins as wins_a, b.tied as tied_a, b.loses as loses_a, c.wins as wins_b, c.tied as tied_b, c.loses as loses_b, b.place as place_a, b.points as points_a, c.place as place_b, c.points as points_b, b.wins_pro as wins_pro_a, b.points_pro as points_pro_a, c.wins_pro as wins_pro_b, c.points_pro as points_pro_b from alle_spiele as a
               join endtables as b
                  on a.team_a = b.team and a.saison = b.saison
               join endtables as c
                  on a.team_b = c.team and a.saison = c.saison")

## some last data management

traindata$place_a <- as.integer(traindata$place_a)
traindata$place_b <- as.integer(traindata$place_b)

traindata$points_a <- as.integer(traindata$points_a)
traindata$points_b <- as.integer(traindata$points_b)

traindata$tore_a <- as.integer(levels(traindata$tore_a))[traindata$tore_a]
traindata$tore_b <- as.integer(levels(traindata$tore_b))[traindata$tore_b]

traindata$wins_a <- as.integer(traindata$wins_a)
traindata$tied_a <- as.integer(traindata$tied_a)
traindata$loses_a <- as.integer(traindata$loses_a)

traindata$wins_b <- as.integer(traindata$wins_b)
traindata$tied_b <- as.integer(traindata$tied_b)
traindata$loses_b <- as.integer(traindata$loses_b)

save(traindata, file = "data/traindata.RData")