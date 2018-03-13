###Using a list of dates and location codes, create a date object
###Optionally pass a vector of course meeting times from CX to set the date to that time
WU.as.Date <- function(dates, locs) {
      
      dates <- as.list(as.character(dates))
      zones <- WU.get.tz(locs)
      
      ##These items are currently set to how data is coming out of informer
      ##Shoul be modified if the nature of the data source cha
      dates <- gsub("  12:00:00 AM$", "", dates)
      frmt <- "%b %d, %Y"
      
      defaulttz <- "America/Chicago"
      
      dates <- lapply(seq(dates), function(x) as.POSIXlt(dates[[x]], frmt, 
                                                         tz = ifelse(is.na(zones[x]), defaulttz, zones[x])))
      
      return(dates)
}

# ###Make a list from Informer (CX) into a date object
# WU.as.Date <- function(x, time = NULL) {
#       require(lubridate)
#       
#       if (is.Date(x)) {
#             message("Object is already a date")
#       } else {
#             x <- as.Date(as.character(x), "%b %d, %Y  %T %p")
#       }
#       
#       if(!is.null(time)) {
#             x <- x + WU.as.hms(time)
#       }
#       
#       return(x)
# }

###Returns a named character vector time zones with campus codes as names.
###If vector of time zones is provided, returns all the campuses in those zones


WU.time.zones <- function(as.df = FALSE, zones = NULL) {
      TZs <- c("COLM" = "America/New_York",
               "CHAR" = "America/New_York",
               "WEBG" = "America/Chicago",
               "SPCT" = "America/New_York",
               "ORLN" = "America/New_York",
               "LTRK" = "America/Chicago",
               "MYRT" = "America/New_York",
               "HILL" = "America/Denver",
               "JACK" = "America/New_York",
               "KANS" = "America/Chicago",
               "FTBL" = "America/Denver",
               "NAVL" = "America/New_York",
               "FTLV" = "America/Chicago",
               "FTJK" = "America/New_York",
               "BJCH" = "America/Chicago",
               "SSMH" = "America/Chicago",
               "TAMP" = "America/New_York",
               "PALM" = "America/New_York",
               "LAKE" = "America/New_York",
               "GREN" = "America/New_York",
               "DWTN" = "America/Chicago",
               "WEST" = "America/Chicago",
               "WING" = "America/Chicago",
               "FTLW" = "America/Chicago",
               "LAAB" = "America/Los_Angeles",
               "BOLL" = "America/New_York",
               "LUKE" = "America/Phoenix",
               "ONLN" = "America/Chicago",
               "LUTH" = "America/Chicago",
               "GENE" = "Europe/Zurich",
               "SOMA" = "America/New_York",
               "FTSL" = "America/Chicago",
               "TINK" = "America/Chicago",
               "VIEN" = "Europe/Vienna",
               "LEJE" = "America/New_York",
               "LEID" = "Europe/Amsterdam",
               "THAI" = "Asia/Bangkok",
               "BANG" = "Asia/Bangkok",
               "OCLA" = "America/New_York",
               "AMST" = "Europe/Amsterdam",
               "MEMP" = "America/Chicago",
               "RAND" = "America/Chicago",
               "ANDY" = "America/New_York",
               "LRAF" = "America/Chicago",
               "FTBG" = "America/New_York",
               "SATX" = "America/Chicago",
               "LOUI" = "America/Kentucky/Louisville",
               "FTST" = "America/New_York",
               "HNTR" = "America/New_York",
               "NEWR" = "America/New_York",
               "SHAW" = "America/New_York",
               "COLO" = "America/Denver",
               "HALL" = "America/New_York",
               "FTBV" = "America/New_York",
               "SANA" = "America/Denver",
               "CUYO" = "America/Argentina/Mendoza",
               "JFOU" = "Asia/Tokyo",
               "OXBR" = "Europe/London",
               "BLCU" = "Asia/Shanghai",
               "SCOT" = "America/Chicago",
               "LACK" = "America/Chicago",
               "IRVN" = "America/Los_Angeles",
               "MAFB" = "America/Chicago",
               "FTGO" = "America/New_York",
               "FAYE" = "America/New_York",
               "FTCH" = "America/Chicago",
               "RLLA" = "America/Chicago",
               "PETR" = "America/Denver",
               "MGHA" = "Africa/Accra",
               "GRTL" = "America/Chicago",
               "CHEN" = "Asia/Shanghai",
               "ATHE" = "Europe/Athens",
               "SCSW" = "America/Chicago",
               "SHNG" = "Asia/Shanghai",
               "CHAF" = "America/New_York",
               "BMED" = "America/New_York",
               "DENV" = "America/Denver",
               "ROSE" = "America/Denver",
               "MRCY" = "America/Chicago",
               "SCTD" = "America/Chicago",
               "UDOV" = "Europe/Madrid",
               "WGDL" = "America/Chicago",
               "FLNC" = "Europe/Rome",
               "UROE" = "Europe/London",
               "STAH" = "America/Chicago")
      
      if(!is.null(zones)) {
            TZs <- TZs[TZs %in% zones]
      }
      
      if(as.df == TRUE) {
            TZs <- data.frame("Location" = names(TZs), "TZ" = TZs, 
                              stringsAsFactors = FALSE)
      }
      
      return(TZs)
}

###Make a vector of time zones from a vector of campus codes
WU.get.tz <- function(x) {
      
      TZs <- WU.time.zones()
      
      existing <- match(x, names(TZs))
      y <- TZs[existing]
      names(y)[is.na(y)] <- x[is.na(y)]
      
      return(y)
}

###Use string functions to convert class times to HH:MM:SS
WU.as.hms <- function(h) {
      require(lubridate)
      
      h <- as.character(h)
      h[h == "0"] <- "0000"
      h[nchar(h) == 1] <- paste0(h[nchar(h) == 1], "00")
      h[nchar(h) == 3] <- paste0("0", h[nchar(h) == 3])
      h <- gsub("(\\d{2})(\\d{2})", "\\1:\\2", h)
      h <- hm(h, quiet = TRUE)

      return(h)
}

###From a list of time-zone specific date instants
WU.add.time <- function(datelist, hmsvector) {
      require(purrr)
      require(lubridate)
      
      x <- map2(datelist, hmsvector, ~ .x + .y)
      
      return(x)
}

##Turn a CX Days object into a vector of meeting days by day of week 1-7
##Currenty this assumes a 7 character string.  Any more than 7 will cause errors
WU.meeting.days <- function(meetingdays) {
      meetingdays <- as.character(meetingdays)
      
      dayindex <- c("U" = 1,
                    "M" = 2, 
                    "T" = 3,
                    "W" = 4,
                    "R" = 5,
                    "F" = 6,
                    "S" = 7)
      
      meetingdays <- strsplit(meetingdays, "")
      
      meetingdays <- lapply(meetingdays, function(x) as.integer(na.omit(match(names(dayindex), x))))
      
      return(meetingdays)
}

###Extract all the meeting dates from the provided
##ISSUE: It looks like in CX many classes in my sample dat in 2017
##have an incorrect end date, which makes the classes one week shorter.
WU.meeting.dates <- function(startdate, enddate, meetingdays) {
      require(lubridate)
      
      ##Remove time-zone formatting just to be safe
      startdate <- as.Date(startdate)
      enddate <- as.Date(enddate)
      
      rangedates <-seq(startdate, enddate, by = "days")
      rangedatesdays <- wday(rangedates)
      
      rangedates <- rangedates[rangedatesdays %in% meetingdays]
      
      return(rangedates)
}


