##Function to creat a unique collapsible sidebar panel
customSBCollapsePanel <- function(name, content_tag_list) {
  
  id <- gsub("[[:punct:]]", "", name)
  id <- gsub("[[:space:]]", "-", id)
  id0 <- paste0(id, "-0")
  id0heading <- paste0(id, "-heading")
  id0collapse <- paste0(id, "-collapse")
  id0collapsehash <- paste0("#", id0collapse)
  idhash <- paste0("#", id)
  idhref <- paste0("#", id)
  
  panel_tags <-
    #######################Collapsible Panel Tagset
    div(id = id,
        class = "panel-group", 
        role = "tablist", 
        'aria-multiselectable' = "true",
        div(id = id0,
            class = "panel panel-primary",
            div(id = id0heading,
                class = "panel-heading",
                'data-toggle' = "collapse", 
                'data-target' = id0collapsehash, 
                'data-parent' = idhash, 
                'aria-expanded' = "true", 
                'aria-controls' = id0collapse, 
                style = "cursor: pointer;",
                h4(class = "panel-title",
                   name
                )#/h4
            ),#/div id0heading
            div(id = id0collapse, 
                class = "panel-collapse collapse",
                role = "tabpanel", 
                'aria-labelledby' = id0heading,
                div(class="panel-body",
                    content_tag_list
                )#/div panel-body
            )#/div id0collapse
        )#/panel id0
    )#/id
  ######################
  
  return(panel_tags)
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


## For each term code, determine if the date is n days before
## registration opens through the start of the session to be
## default selection
## Turned off b/c pre-selecting created more confusion than help
# defaultTerms <- function() {
#       
#       ndays <- 14 
#   
#       default_terms <- list()
#       
#       default_terms$SU <- ifelse(
#             as.integer(format(Sys.Date(), "%j")) >= 
#                   as.integer(format(as.Date("2017-02-28"), "%j")) - ndays &&
#                   as.integer(format(Sys.Date(), "%j")) <
#                   as.integer(format(as.Date("2017-06-16"), "%j")),
#             TRUE, FALSE)
#       
#       default_terms$FA <- ifelse(
#             as.integer(format(Sys.Date(), "%j")) >= 
#                   as.integer(format(as.Date("2017-04-03"), "%j")) - ndays &&
#             as.integer(format(Sys.Date(), "%j")) <
#                   as.integer(format(as.Date("2017-09-08"), "%j")),
#             TRUE, FALSE)
#       
#       default_terms$F1 <- default_terms$FA
#       
#       default_terms$F2 <- ifelse(
#             as.integer(format(Sys.Date(), "%j")) >= 
#                   as.integer(format(as.Date("2017-04-03"), "%j")) - ndays &&
#                   as.integer(format(Sys.Date(), "%j")) <
#                   as.integer(format(as.Date("2017-11-10"), "%j")),
#             TRUE, FALSE)
#       
#       default_terms$SP <- ifelse(
#             as.integer(format(Sys.Date(), "%j")) >= 
#                   as.integer(format(as.Date("2017-11-06"), "%j")) - ndays &&
#             as.integer(format(Sys.Date(), "%j")) < 366 ||
#             as.integer(format(Sys.Date(), "%j")) + 366 <
#                   as.integer(format(as.Date("2018-01-26"), "%j")) + 366,
#             TRUE, FALSE)
#       
#       default_terms$S1 <- default_terms$SP
#       
#       default_terms$S2 <- ifelse(
#             as.integer(format(Sys.Date(), "%j")) >= 
#                   as.integer(format(as.Date("2017-11-06"), "%j")) - ndays &&
#                   as.integer(format(Sys.Date(), "%j")) < 366 ||
#                   as.integer(format(Sys.Date(), "%j")) + 366 <
#                   as.integer(format(as.Date("2018-03-30"), "%j")) + 366,
#             TRUE, FALSE)
#       
#       return(default_terms)
# }
# 

## Determine which years to pre-select
## Turned off b/c pre-selecting created more confusion than help
# defaultYear <- function() {
#       
#       default_terms <- defaultTerms()
#       
#       if(as.integer(format(Sys.Date(), "%m")) >= 11 &&
#          default_terms$SP == TRUE) {
#             year <- as.integer(format(Sys.Date(), "%Y")) + 1
#       } else {
#             year <- as.integer(format(Sys.Date(), "%Y")) 
#       }
#       return(year)
# }

##Incomplete function that loads section description from webster description app
# getSectionDescription <- function(TERM, YEAR, SUBJECT, COURSENO, SECTION) {
#       require(httr)
#       
#       url <- parse_url("http://apps.webster.edu/compcen/datadict/webcrs/files/TERMYEAR/COURSESECTION.html")
#       
#       TERMYEAR <- paste0(TERM, YEAR)
#       COURSESECTION <- paste0(SUBJECT, "_", COURSENO, "_", SECTION)
#       
#       url$path <- gsub("TERMYEAR", TERMYEAR, url$path)
#       url$path <- gsub("COURSESECTION", COURSESECTION, url$path)
#       
#       response <- GET(url)
#       
#       if (response$status_code == 404) {
#             return("")
#       }
#       
#       content(response)
#       
#       return(response)
# }
