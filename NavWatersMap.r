require(XML)
require(stringr)
require(plyr)

## Get html from the url and parse out the tables, then convert to a dataframe
url <- "http://parl.gc.ca/HousePublications/Publication.aspx?Language=E&Mode=1&DocId=5765988&File=615&Col=1"
url.doc <- htmlParse(readLines(url))
url.tabs <- readHTMLTable(url.doc)
lakesOceans <- data.frame(url.tabs[1])[,2:4]

## Convert factors to strings, rename columns, classify water bodies
lakesOceans <- data.frame(lapply(lakesOceans, as.character), stringsAsFactors=F)
names(lakesOceans) <- c("Name","Approx_Loc","Description")
lakesOceans$Type <- ifelse(grepl("ocean", lakesOceans$Name, ignore.case=T)
                           , "Ocean", "Lake")

## Extract LAT and LONG from Approx_Location
lakesOceans$LAT <- substr(lakesOceans$Approx_Loc,0,8)
lakesOceans$LONG <- ifelse(nchar(lakesOceans$Approx_Loc)==25
                           , substr(lakesOceans$Approx_Loc,14,22)
                           , substr(lakesOceans$Approx_Loc,14,21)
)

## Subsitite ":" for deg and min symbols (easier to work with)
substr(lakesOceans$LAT,3,3) <- ":"
substr(lakesOceans$LAT,6,6) <- ":"

## use str_sub from pkg stringr to sub from right
str_sub(lakesOceans$LONG,rep(-3,length(lakesOceans$LONG)),rep(-3,length(lakesOceans$LONG))) <- ":"
str_sub(lakesOceans$LONG,rep(-6,length(lakesOceans$LONG)),rep(-6,length(lakesOceans$LONG))) <- ":"

dms_dd <- function(x, sep=":", hem) {
# x: a vector containing the lat or long with elements separated by single character
# sep: the character separating the degrees, minutes, seconds
# hem: the hemisphere ("N","S","E","W").  Assumes all coords in the same hemisphere
  if (hem %in% c("N","S","E","W")) {
    x <- lapply(strsplit(x,sep), as.numeric)
    x <- unlist(lapply(x, function(y) (y[1]+y[2]/60+y[3]/3600)))
    ifelse(hem %in% c("N","E"),
           ifelse(x>0,mult <- 1, mult <- -1),
           ifelse(x<0,mult <- 1, mult <- -1))
    x <- x*mult
    x
  } else {
    print("Error: 'hem' must be N,S,E, or W")
  }
}

lakesOceans$LAT_DD <- dms_dd(lakesOceans$LAT,":","N")
lakesOceans$LONG_DD <- dms_dd(lakesOceans$LONG,":","W")


rivers <- data.frame(url.tabs[2])[2:5]
rivers <- data.frame(lapply(rivers, as.character), stringsAsFactors=F)
names(rivers) <- c("Name","Approx_Downstream_Point"
                   ,"Approx_Upstream_Point","Description")
rivers$DOWN_LAT <- substr(substr(rivers$Approx_Downstream_Point,0,11)
rivers$DOWN_LONG <- substr(rivers$Approx_Downstream_Point,14,26)
rivers$UP_LAT <- substr(rivers$Approx_Upstream_Point,0,11)
rivers$UP_LONG <- substr(rivers$Approx_Upstream_Point,14,26)

