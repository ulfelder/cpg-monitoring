# Function to download & unzip daily file, filter for events of interest, and write out .csv with markers
# for screened events and screening decisions
fetch <- function(year, month, day) {
    y <- as.character(year)
    m <- ifelse(month < 10, paste("0", month, sep=""), as.character(month))
    d <- ifelse(day < 10, paste("0", day, sep=""), as.character(day))
    daily.url <- paste("http://gdelt.utdallas.edu/data/dailyupdates/", y, m, d, ".export.CSV.zip", sep = "")
    daily.csv <- paste(y, m, d, ".export.CSV", sep = "")
    temp <- tempfile()
    download.file(daily.url, temp)
    temp2 <- tempfile()
    temp2 <- unz(temp, daily.csv)
    dailies <- read.delim(temp2, header=FALSE)
    temp3 <- tempfile()
    download.file("http://gdelt.utdallas.edu/data/lookups/CSV.header.dailyupdates.txt", temp3)
    header <- read.delim(temp3, header=TRUE)
    names(dailies) <- names(header)
    unlink(temp)
    unlink(temp2)
    unlink(temp3)
    z <- subset(dailies, 
                        IsRootEvent==1 &  # Test on small sample suggested this produces ~ 20% missed pos rate
                       (EventCode==180 | EventCode==1823 | 
                          EventCode==183 | (EventCode>=1831 & EventCode<=1834) |
                          EventCode==186 |
                        EventCode==190 | EventCode==193 | EventCode==194 | EventCode==195 |
                        EventCode==200 | EventCode==201 | EventCode==202 | EventCode==203 |
                          EventCode==204 | EventCode==205 ) &
                      ((Actor2Type1Code=="CVL" | Actor2Type1Code=="OPP" |
                          Actor2Type1Code=="EDU" | Actor2Type1Code=="LAB" | 
                          Actor2Type1Code=="REL" | Actor2Type1Code=="HLH" |
                          Actor2Type1Code=="REF" | Actor2Type1Code=="MED" ) |
                       (Actor2Type2Code=="CVL" | Actor2Type2Code=="OPP" |
                          Actor2Type2Code=="EDU" | Actor2Type2Code=="LAB" | 
                          Actor2Type2Code=="REL" | Actor2Type2Code=="HLH" |
                          Actor2Type2Code=="REF" | Actor2Type2Code=="MED" )) &
                      ((Actor1Type1Code=="GOV" | Actor1Type1Code=="MIL" | 
                          Actor1Type1Code=="COP" | Actor1Type1Code=="SPY" |
                          Actor1Type1Code=="REB" | Actor1Type1Code=="SEP" | Actor1Type1Code=="UAF" ) |
                       (Actor1Type2Code=="GOV" | Actor1Type2Code=="MIL" |
                          Actor1Type2Code=="COP" | Actor1Type2Code=="SPY" |
                          Actor1Type2Code=="REB" | Actor1Type2Code=="SEP" | Actor1Type1Code=="UAF" )) ) 
    z1 <- subset(z, select=c(GLOBALEVENTID,
                             Actor1Name,
                             Actor1CountryCode,
                             Actor1Type1Code,
                             Actor1Type2Code,
                             Actor2Name,
                             Actor2CountryCode,
                             Actor2Type1Code,
                             Actor2Type2Code,
                             EventCode,
                             ActionGeo_FullName,
                             SOURCEURL))
    z1$year <- y
    z1$month <- m
    z1$day <- d
    z1$ushmm.screened <- 1
    z1$ushmm.eoi <- 0
    z1$year.alt <- NA   # Remaining vars are fields for manual corrections
    z1$month.alt <- NA
    z1$day.alt <- NA
    z1$Actor1CountryCode.alt <- NA
    z1$Actor1Type1Code.alt <- NA
    z1$Actor1Type2Code.alt <- NA
    z1$Actor2CountryCode.alt <- NA
    z1$Actor2Type1Code.alt <- NA
    z1$Actor2Type2Code.alt <- NA
    z1$location.alt <- NA
    z1$deaths <- NA
    z1$ushmm.eoi.duplicate <- NA
    z1$Comments <- NA
    outname <- paste(y, m, d, ".atrocities.csv", sep="")
    write.csv(z1, outname, row.names=FALSE)
}

# Fetch the previous day's data
yesterday <- as.Date(Sys.Date()) - 1
fetch( substr(yesterday,1,4),
       substr(yesterday,6,7),
       substr(yesterday,9,10) )
