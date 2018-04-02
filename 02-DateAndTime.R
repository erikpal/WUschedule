library(lubridate)

data <- readRDS(schedule_data_path)

# data$BEGDATE <- as.Date(data$`Section Begin Date`)
# data$ENDDATE <- as.Date(data$`Ending Date`)

data$BEGTIME <- as.character(data$`Beginning Time`)
data$ENDTIME <- as.character(data$`Ending Time`)

WU.as.hms <- function(h) {
  require(lubridate)
  
  h <- as.character(h)
  h[h == "0"] <- "0000"
  h[nchar(h) == 1] <- paste0(h[nchar(h) == 1], "00")
  h[nchar(h) == 3] <- paste0("0", h[nchar(h) == 3])
  h <- gsub("(\\d{2})(\\d{2})", "\\1:\\2", h)
  # h <- hm(h, quiet = TRUE)
  
  return(h)
}

data$BEGTIME <- WU.as.hms(data$BEGTIME)
data$ENDTIME <- WU.as.hms(data$ENDTIME)
# Sys.Date() + hm(data$BEGTIMEA)

data$MTIME <- paste0(data$BEGTIME,  " - ", data$ENDTIME)
# data$MTIME <- as.factor(data$MTIME)
