library(XML)

tables <- list.files(path = "html_shots/", pattern = "endtable.htm", include.dirs = FALSE)

for (i in tables) {
   
   table <- readHTMLTable(doc = paste("html_shots/", i, sep = ""))[[1]]
   
   # get season
   saison <- substr(i, 8, 14)
   
   table <- data.frame(lapply(table, as.character), stringsAsFactors=FALSE)
   table <- table[-1,]
   
   table$V1 <- substr(table$V1, 1, nchar(table$V1)-1)
   
   # split wins (S), ties (U) and loses (N)
   sun <- data.frame(strsplit(table$V4, "-"))
   
   sieg <- t(sun)[,1]
   unent <- t(sun)[,2]
   niederl <- t(sun)[,3]
   
   # calculate percent of all possible points (total: 102)
   point_pro <- as.numeric(table$V6) / 102
   
   # likelihood of a win; total games: 34
   sieg_pro <- as.numeric(sieg) / 34
   
   # make data frame
   table <- cbind(table[,c(2,1,6)], saison, sieg, unent, niederl, point_pro, sieg_pro)
   table <- data.frame(lapply(table, as.vector))
   
   # put together all seasons
   if (i == tables[1]) {
      endtables <- table
   } else {
      endtables <- rbind(endtables, table)
   }
}

# variable renaming
names(endtables) <- c("team", "place", "points", "saison", "wins", "tied", "loses", "points_pro", "wins_pro")

# some teams are double again
endtables$team[endtables$team == "Hertha BSC"] <- "Hertha BSC Berlin"
endtables$team[endtables$team == "Werder Bremen"] <- "SV Werder Bremen"
endtables$team[endtables$team == "Sport-Club Freiburg"] <- "SC Freiburg"
endtables$team[endtables$team == "Schalke 04"] <- "FC Schalke 04"
endtables$team[endtables$team == "Bayern München"] <- "FC Bayern München"
endtables$team[endtables$team == "Bor. Mönchengladbach"] <- "Borussia Mönchengladbach"
endtables$team[endtables$team == "Bayer Leverkusen"] <- "Bayer 04 Leverkusen"
endtables$team[endtables$team == "FSV Mainz 05"] <- "1. FSV Mainz 05"
endtables$team[endtables$team == "Energie Cottbus"] <- "FC Energie Cottbus"
endtables$team[endtables$team == "Arminia Bielefeld"] <- "DSC Arminia Bielefeld"

# get rid of German umlauts again :-)
endtables$team <- gsub("ü", "u", endtables$team)
endtables$team <- gsub("ö", "o", endtables$team)
endtables$team <- gsub("ä", "a", endtables$team)

endtables$team <- factor(endtables$team)

## saving
save(endtables, file = "data/endtables.RData")
