getLat <- function(placename)  {
  require("RJSONIO")
  name <- gsub(" ", "+", placename) # Replace spaces with +
  connectStr <- paste('http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=', name, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  x <- ifelse(data.json["status"]=="OK", data.json["results.geometry.location.lat"], NA)
  names(x) <- NULL
  return(x)
}

getLng <- function(placename)  {
  require("RJSONIO")
  name <- gsub(" ", "+", placename) # Replace spaces with +
  connectStr <- paste('http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=', name, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  x <- ifelse(data.json["status"]=="OK", data.json["results.geometry.location.lng"], NA)
  names(x) <- NULL
  return(x)
}

scrub <- function(y, m, d) {
  filename <- paste(y, ifelse(m < 10, paste("0", m, sep=""), m),
    ifelse(d < 10, paste("0", d, sep=""), d), ".atrocities.coded.csv", sep="")
  df <- read.csv(filename, stringsAsFactors=FALSE)
  df <- subset(df, ushmm.eoi==1)
  df$loc <- ifelse(is.na(df$location.alt)==FALSE,
    as.character(df$location.alt), as.character(df$ActionGeo_FullName))
  for (i in 1:dim(df)[1]) df$lat[i] <- getLat(df$loc[i])
  for (i in 1:dim(df)[1]) df$lng[i] <- getLng(df$loc[i])

  df$ActionGeo_FullName <- as.character(df$loc)
  df$date <- ifelse(is.na(df$year.alt)==FALSE,
    paste(df$year.alt,
      ifelse(df$month.alt < 10, paste("0",df$month.alt,sep=""), df$month.alt),
      ifelse(df$day.alt < 10, paste("0",df$day.alt,sep=""), df$day.alt),
      sep="-"),
    paste(df$year,
      ifelse(df$month < 10, paste("0",df$month,sep=""), df$month),
      ifelse(df$day < 10, paste("0",df$day,sep=""), df$day),
      sep="-") )
  df$Actor1CountryCode <- ifelse(is.na(df$Actor1CountryCode.alt)==FALSE,
    as.character(df$Actor1CountryCode.alt), as.character(df$Actor1CountryCode))
  df$Actor1Type1Code <- ifelse(is.na(df$Actor1Type1Code.alt)==FALSE,
    as.character(df$Actor1Type1Code.alt), as.character(df$Actor1Type1Code))
  df$Actor1Type2Code <- ifelse(is.na(df$Actor1Type2Code.alt)==FALSE,
    as.character(df$Actor1Type2Code.alt), as.character(df$Actor1Type2Code))
  df$Actor2CountryCode <- ifelse(is.na(df$Actor2CountryCode.alt)==FALSE,
    as.character(df$Actor2CountryCode.alt), as.character(df$Actor2CountryCode))
  df$Actor2Type1Code <- ifelse(is.na(df$Actor2Type1Code.alt)==FALSE,
    as.character(df$Actor2Type1Code.alt), as.character(df$Actor2Type1Code))
  df$Actor2Type2Code <- ifelse(is.na(df$Actor2Type2Code.alt)==FALSE,
    as.character(df$Actor2Type2Code.alt), as.character(df$Actor2Type2Code))

  sub <- subset(df, select=c(date, loc, EventCode, deaths,
    Actor1CountryCode, Actor1Type1Code, Actor1Type2Code,
    Actor2CountryCode, Actor2Type1Code, Actor2Type2Code,
    SOURCEURL, lat, lng, GLOBALEVENTID))

  write.csv(sub, paste(y, ifelse(m < 10, paste("0", m, sep=""), m),
    ifelse(d < 10, paste("0", d, sep=""), d), ".atrocities.scrubbed.csv", sep=""), row.names=FALSE)
}
