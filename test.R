library(XML)

testtable <- readHTMLTable(doc = "html_shots/season_2000_01_endtable.htm")[[1]]

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