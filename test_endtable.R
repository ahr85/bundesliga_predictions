library(XML)

#testtable <- readHTMLTable(doc = "html_shots/season_2000_01_endtable.htm")[[1]]

tables <- list.files(path = "html_shots/", pattern = "endtable.htm", include.dirs = FALSE)

for (i in tables) {
   
   table <- readHTMLTable(doc = paste("html_shots/", i, sep = ""))[[1]]
   
   # Saison extrahieren
   saison <- substr(i, 8, 14)
   
   table <- data.frame(lapply(table, as.character), stringsAsFactors=FALSE)
   table <- table[-1,]
   
   table$V1 <- substr(table$V1, 1, nchar(table$V1)-1)
   
   # Sieg, Unentschieden, Niederlage trennen
   sun <- data.frame(strsplit(table$V4, "-"))
   
   sieg <- t(sun)[,1]
   unent <- t(sun)[,2]
   niederl <- t(sun)[,3]
   
   # data frame erstellen
   table <- cbind(table[,c(2,1,6)], saison, sieg, unent, niederl)
   table <- data.frame(lapply(table, as.vector))
   
   # alle einzelnen Saisons zusammenmatchen
   if (i == tables[1]) {
      endtables <- table
   } else {
      endtables <- rbind(endtables, table)
   }
}

# Spaltennamen
names(endtables) <- c("team", "place", "points", "saison", "wins", "tied", "loses")

# wieder doppelte Mannschaftsnamen raus
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

endtables$team <- factor(endtables$team)

### Hiermit sind wir auch fertig!
save(endtables, file = "data/endtables.RData")



testtable <- data.frame(lapply(testtable, as.character), stringsAsFactors=FALSE)

testtable <- testtable[-1,]

# Punkt hinter Plazierung weg
testtable$V1 <- substr(testtable$V1, 1, nchar(testtable$V1)-1)

# Sieg, Unentschieden, Niederlage trennen
sun <- data.frame(strsplit(testtable$V4, "-"))

sieg <- t(sun)[,1]
unent <- t(sun)[,2]
niederl <- t(sun)[,3]

# data frame erstellen
df <- cbind(testtable[,c(2,1,6,7)], sieg, unent, niederl)
names(df) <- c("team", "place", "point", "season", "wins", "tied", "loses")
df <- data.frame(lapply(df, as.vector))