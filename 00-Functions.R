##This function can be used to update the schedule data from an informer
##live excel file.  To update, set update to true, otherwise it will load the
##last retreived data set.

loadFrame <- function(path = "./schedule_frame.RDS", update = FALSE) {
      require(dplyr)
      
      if (update == TRUE) {
            source("../Informer/00-Functions-OpenIQY.R")
            
            ##Use todays date minus offset, and today plus offset for the query
            ##Cap_offset is used to exclude fat finger dates like 2051
            days_offset <- 14
            cap_offset <- 365 * 2
            load_date <- as.character(format(Sys.Date() - days_offset, "%m/%d/%Y"))
            cap_date <- as.character(format(Sys.Date() + cap_offset, "%m/%d/%Y"))
            
            qparams <- list("parameter_0" = load_date, 
                            "parameter_1" = cap_date) 

            x <- openIqy("../Informer/Queries/Schedule Frame.iqy", qparams)
            
            #Clean up used variables
            x <- x[!is.na(x$`Course Number`), ]
            
            x$SECLOC <- as.character(x$SECLOC)
            x$COMBINELOC <- as.character(x$COMBINELOC)
            x$BUILDINGDESC <- as.character(x$BUILDINGDESC)
            x$DEPT <- as.character(x$`Department Code`)
            x$COURSECODE <- as.character(x$`Course Number`)
            x$SECNO <- as.character(x$`Section Number Code`)
            x$IMDESC <- as.character(x$`IM Description`)
            x$PROGRAM <- as.character(x$PROGRAM)
            
            
            x$TERM <- as.character(x$`Term Code`)
            x$YEAR <- as.integer(as.character(x$`Calendar Year - Section`))
            x$SECHOURS <- as.integer(as.character(x$`Section Hours`))
            
            
            x$SUBJECT <- gsub("(\\D{3,4}) (\\d{4})", "\\1", x$COURSECODE)
            x$COURSENO <- gsub("(\\D{3,4}) (\\d{4})", "\\2", x$COURSECODE)
            
            ##Concatenate the titles
            x$COTITLE <- paste(x$COTITLE1, x$COTITLE2, x$COTITLE3)
            
            ##Remove any courses that do not have any of the required variables
            x <- x[!x$SECLOC == "",]
            x <- x[!x$TERM == "",]

            ##Make a column to index online classes
            x$ONLINE <- grepl("Online", x$IMDESC)
            x$WEBNET <- grepl("WebNet", x$IMDESC)
            
            #x$WEBNETHOME <- x[x$WEBNET == TRUE & x$ONLINE == TRUE]
            
            ##Recode BUILDINGDESC and COMBLOC to say Online
            x$BUILDINGDESC[x$ONLINE] <- "Online"
            x$COMBINELOC[x$ONLINE] <- "Online"
            
            
            x$STATUS <- ifelse(x$stat %in% c("X", "I"), "Canceled", "Open")
            
            ##Add GCP Details
            gcp_codes <- c("CRI" = "Critical Thinking",
                           "ETH" = "Ethical Reasoning",
                           "INTC" = "Intercultural Competence",
                           "OCOM" = "Oral Communication",
                           "WCOM" = "Written Communication",
                           "SSHB" = "Social Systems & Human Behaviors",
                           "ROC" = "Roots of Cultures",
                           "GLBL" = "Global Understaning",
                           "ARTS" = "Arts Appreciation",
                           "PNW" = "Physical & Natural World",
                           "QL" = "Quantitative Literacy")
            
            ##Set the fields characters
            x$genedcatc <- as.character(x$genedcatc)
            x$interstuda <- as.character(x$interstuda)
            x$interstudb <- as.character(x$interstudb)
            
            ##Make new boolean variables for the skills
            x$CRI <- x$interstuda == "CRI" | x$interstudb == "CRI"
            x$ETH <- x$interstuda == "ETH" | x$interstudb == "ETH"
            x$INTC <- x$interstuda == "INTC" | x$interstudb == "INTC"
            x$OCOM <- x$interstuda == "OCOM" | x$interstudb == "OCOM"
            x$WCOM <- x$interstuda == "WCOM" | x$interstudb == "WCOM"
            
            ##Make new boolean variables for the knowledge areas
            x$SSHB <- x$genedcatc == "SSHB"
            x$ROC <- x$genedcatc == "ROC"
            x$GLBL <- x$genedcatc == "GLBL"
            x$ARTS <- x$genedcatc == "ARTS"
            x$PNW <- x$genedcatc == "PNW"
            x$QL <- x$genedcatc == "QL"
            
            ##Add course descriptions from saved flat file
            y <- readRDS("../Webster Catalog/CatalogSnapshot.Rdata") %>% 
                  select(PREFIX, NUMBER, DESC_CLEAN, ATTS) %>% 
                  distinct(PREFIX, NUMBER, .keep_all = TRUE)
            
            x <- left_join(x, y, c("SUBJECT" = "PREFIX", "COURSENO" = "NUMBER"))
            
            #Save
            saveRDS(x, path)
      }
      
      x <- readRDS(path)
      return(x)
}

### Determine if term is active for potential registration ------
## For each term code, determine if the date is n days before
## registration opens through the start of the session to be
## default selection
defaultTerms <- function() {
      
      ndays <- 14 
  
      default_terms <- list()
      
      default_terms$SU <- ifelse(
            as.integer(format(Sys.Date(), "%j")) >= 
                  as.integer(format(as.Date("2017-02-28"), "%j")) - ndays &&
                  as.integer(format(Sys.Date(), "%j")) <
                  as.integer(format(as.Date("2017-06-16"), "%j")),
            TRUE, FALSE)
      
      default_terms$FA <- ifelse(
            as.integer(format(Sys.Date(), "%j")) >= 
                  as.integer(format(as.Date("2017-04-03"), "%j")) - ndays &&
            as.integer(format(Sys.Date(), "%j")) <
                  as.integer(format(as.Date("2017-09-08"), "%j")),
            TRUE, FALSE)
      
      default_terms$F1 <- default_terms$FA
      
      default_terms$F2 <- ifelse(
            as.integer(format(Sys.Date(), "%j")) >= 
                  as.integer(format(as.Date("2017-04-03"), "%j")) - ndays &&
                  as.integer(format(Sys.Date(), "%j")) <
                  as.integer(format(as.Date("2017-11-10"), "%j")),
            TRUE, FALSE)
      
      default_terms$SP <- ifelse(
            as.integer(format(Sys.Date(), "%j")) >= 
                  as.integer(format(as.Date("2017-11-06"), "%j")) - ndays &&
            as.integer(format(Sys.Date(), "%j")) < 366 ||
            as.integer(format(Sys.Date(), "%j")) + 366 <
                  as.integer(format(as.Date("2018-01-26"), "%j")) + 366,
            TRUE, FALSE)
      
      default_terms$S1 <- default_terms$SP
      
      default_terms$S2 <- ifelse(
            as.integer(format(Sys.Date(), "%j")) >= 
                  as.integer(format(as.Date("2017-11-06"), "%j")) - ndays &&
                  as.integer(format(Sys.Date(), "%j")) < 366 ||
                  as.integer(format(Sys.Date(), "%j")) + 366 <
                  as.integer(format(as.Date("2018-03-30"), "%j")) + 366,
            TRUE, FALSE)
      
      return(default_terms)
}

defaultYear <- function() {
      
      default_terms <- defaultTerms()
      
      if(as.integer(format(Sys.Date(), "%m")) >= 11 &&
         default_terms$SP == TRUE) {
            year <- as.integer(format(Sys.Date(), "%Y")) + 1
      } else {
            year <- as.integer(format(Sys.Date(), "%Y")) 
      }
      return(year)
}

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
