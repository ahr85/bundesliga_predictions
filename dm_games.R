library(XML)

games <- list.files(path = "html_shots/", pattern = "games.htm", include.dirs = FALSE)

saison_weight <- 0

for (i in games) {

   table <- readHTMLTable(doc = paste("html_shots/", i, sep = ""))
   
   # Saison extrahieren
   saison <- substr(i, 8, 14)
   saison_weight <- saison_weight + 1
   
   for (ii in 1:34) {
      temp <- data.frame(lapply(table[[ii]], as.character), stringsAsFactors=FALSE)
      temp <- temp[2:nrow(temp),]
      
      # Mannschaften
      teams <- data.frame(t(data.frame(strsplit(temp$V1,  "\\s-\\s"))), row.names=1:nrow(temp))
      
      # Tore und Sieg/Unentschieden/Niederlage
      for (iii in 1:nrow(temp)) {
         char <- nchar(temp$V2[iii])
         tore_heim <- substr(temp[iii,2], 1,1)
         tore_gast <- substr(temp[iii,2], char, char)
         
         sun_heim <- "U"
         sun_gast <- "U"
         if (tore_heim > tore_gast) {
            sun_heim <- "S"
            sun_gast <- "N"
         } else if (tore_heim < tore_gast) {
            sun_heim <- "N"
            sun_gast <- "S"
         }
         
         # zusamenmatchen eines Spieltags
         if (iii == 1) {
            tore <- data.frame(cbind(tore_heim, tore_gast, sun_heim, sun_gast))
         } else {
            tore <- rbind(tore, cbind(tore_heim, tore_gast, sun_heim, sun_gast))
         }
      }
      
      # alle Spieltage zusammenmatchen
      if (ii == 1) {
         spiele <- cbind(teams, ii, saison, saison_weight, "Heim", tore)
      } else {
         spiele <- rbind(spiele, cbind(teams, ii, saison, saison_weight, "Heim", tore))
      }
      
   }

   # Saisons zusammenmatchen
   if (i == games[1]) {
      alle_spiele <- spiele
   } else {
      alle_spiele <- rbind(alle_spiele, spiele)
   }
   
}
# Variable benennen
names(alle_spiele) <- c("team_a", "team_b", "spieltag", "saison", "saison_weight", "heim_gast", "tore_a", "tore_b", "sun_a", "sun_b")


## hier nochmal alle Spiele drehen, um aus der Heimmannschaft die Gastmannschaft etc zu machen
temp <- alle_spiele[,c(2,1,3,4,5,6,8,7,10,9)]
names(temp) <- c("team_a", "team_b", "spieltag", "saison", "saison_weight", "heim_gast", "tore_a", "tore_b", "sun_a", "sun_b")

# die vorherige Heiommannschaft ist jetzt Gastmannschaft und umgekehrt
temp$heim_gast <- "Gast"

## alles zusammenmatchen
alle_spiele <- rbind(alle_spiele, temp)

# einige Teams sind doppelt - die müssen raus
alle_spiele$team_a[alle_spiele$team_a == "Hertha BSC"] <- "Hertha BSC Berlin"
alle_spiele$team_a[alle_spiele$team_a == "Werder Bremen"] <- "SV Werder Bremen"
alle_spiele$team_a[alle_spiele$team_a == "Sport-Club Freiburg"] <- "SC Freiburg"
alle_spiele$team_a[alle_spiele$team_a == "Schalke 04"] <- "FC Schalke 04"
alle_spiele$team_a[alle_spiele$team_a == "Bayern München"] <- "FC Bayern München"
alle_spiele$team_a[alle_spiele$team_a == "Bor. Mönchengladbach"] <- "Borussia Mönchengladbach"
alle_spiele$team_a[alle_spiele$team_a == "Bayer Leverkusen"] <- "Bayer 04 Leverkusen"
alle_spiele$team_a[alle_spiele$team_a == "FSV Mainz 05"] <- "1. FSV Mainz 05"
alle_spiele$team_a[alle_spiele$team_a == "Energie Cottbus"] <- "FC Energie Cottbus"
alle_spiele$team_a[alle_spiele$team_a == "Arminia Bielefeld"] <- "DSC Arminia Bielefeld"

alle_spiele$team_a <- factor(alle_spiele$team_a)

alle_spiele$team_b[alle_spiele$team_b == "Hertha BSC"] <- "Hertha BSC Berlin"
alle_spiele$team_b[alle_spiele$team_b == "Werder Bremen"] <- "SV Werder Bremen"
alle_spiele$team_b[alle_spiele$team_b == "Sport-Club Freiburg"] <- "SC Freiburg"
alle_spiele$team_b[alle_spiele$team_b == "Schalke 04"] <- "FC Schalke 04"
alle_spiele$team_b[alle_spiele$team_b == "Bayern München"] <- "FC Bayern München"
alle_spiele$team_b[alle_spiele$team_b == "Bor. Mönchengladbach"] <- "Borussia Mönchengladbach"
alle_spiele$team_b[alle_spiele$team_b == "Bayer Leverkusen"] <- "Bayer 04 Leverkusen"
alle_spiele$team_b[alle_spiele$team_b == "FSV Mainz 05"] <- "1. FSV Mainz 05"
alle_spiele$team_b[alle_spiele$team_b == "Energie Cottbus"] <- "FC Energie Cottbus"
alle_spiele$team_b[alle_spiele$team_b == "Arminia Bielefeld"] <- "DSC Arminia Bielefeld"

alle_spiele$team_b <- factor(alle_spiele$team_b)


## alle Spiele noch nach Spieltag und Saison sortieren
alle_spiele <- alle_spiele[order(alle_spiele$saison, alle_spiele$spieltag, alle_spiele$heim_gast),]

alle_spiele$team_a <- gsub("ü", "u", alle_spiele$team_a)
alle_spiele$team_a <- gsub("ö", "o", alle_spiele$team_a)
alle_spiele$team_a <- gsub("ä", "a", alle_spiele$team_a)

alle_spiele$team_b <- gsub("ü", "u", alle_spiele$team_b)
alle_spiele$team_b <- gsub("ö", "o", alle_spiele$team_b)
alle_spiele$team_b <- gsub("ä", "a", alle_spiele$team_b)

alle_spiele$team_a <- factor(alle_spiele$team_a)
alle_spiele$team_b <- factor(alle_spiele$team_b)

### Damit sind wir für diesen Teil fertig!!!
save(alle_spiele, file = "data/alle_spiele.RData")