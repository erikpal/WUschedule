library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
source("00-Functions.R")
source("01-Configs.R")
#source("02-DateAndTime.R")

##Load the data
data <- readRDS(schedule_data_path)

##Date and time transformations
data$BEGTIME <- WU.as.hms(data$`Beginning Time`)
data$BEGTIME <- data$BEGDATE + data$BEGTIME
data$BEGTIME12 <- data$BEGTIME24 <- ""
data$BEGTIME12[!data$`Beginning Time` == 0] <- as.character(format(data$BEGTIME[!data$`Beginning Time` == 0], "%I:%M%p"))
data$BEGTIME12 <- gsub("^0", "", data$BEGTIME12)
data$BEGTIME24[!data$`Beginning Time` == 0] <- as.character(format(data$BEGTIME[!data$`Beginning Time` == 0], "%R"))

data$ENDTIME <- WU.as.hms(data$`Ending Time`)
data$ENDTIME <- data$BEGDATE + data$ENDTIME
data$ENDTIME12 <- data$ENDTIME24 <- ""
data$ENDTIME12[!data$`Ending Time` == 0] <- as.character(format(data$ENDTIME[!data$`Ending Time` == 0], "%I:%M%p"))
data$ENDTIME12 <- gsub("^0", "", data$ENDTIME12)
data$ENDTIME24[!data$`Ending Time` == 0] <- as.character(format(data$ENDTIME[!data$`Ending Time` == 0], "%R"))

##Create new colummns
data$VIEW <- ""
data$PLANNER <- FALSE
data$CREDITS <- data$`Section Hours`
data$GCPSKILL <- FALSE
data$GCPKNOWLEDGE <- FALSE
data$GCPKEYS <- ifelse(data$SUBJECT == "KEYS", TRUE, FALSE)
data$GCPFRSH <- ifelse(data$SUBJECT == "FRSH", TRUE, FALSE)
data$STATUS <- as.character(data$stat)

##Set some variable from the complete available data set
choices_year <- unique(data$YEAR[order(data$YEAR)])
choices_campus <- unique(data$BUILDINGDESC[order(data$BUILDINGDESC)])
choices_department <- unique(data$DEPTTXT[order(data$DEPTTXT)])
choices_prefix <- unique(data$SUBJECT[order(data$SUBJECT)])
choices_credithour <- unique(as.numeric(data$SECHOURS[order(data$SECHOURS)]))

gcp_skills <- c("CRI" = "Critical Thinking",
                "ETH" = "Ethical Reasoning",
                "INTC" = "Intercultural Competence",
                "OCOM" = "Oral Communication",
                "WCOM" = "Written Communication")

gcp_knowledge <- c("SSHB" = "Social Systems & Human Behaviors",
                   "ROC" = "Roots of Cultures",
                   "GLBL" = "Global Understaning",
                   "ARTS" = "Arts Appreciation",
                   "PNW" = "Physical & Natural World",
                   "QL" = "Quantitative Literacy")

##Which columns to include in the out DT
cols <- c(
  " " = "SELECT",
  "Details" = "VIEW", 
  "Title" = "COTITLE",
  "Course" = "COURSECODE",
  "Section" = "SECNO", 
  "Credit Hours" = "CREDITS",
  "Location" = "BUILDINGDESC",
  "Meeting Days" = "Days", 
  "Meeting Time" = "Beginning Time",
  "Instructor" = "Name",
  "Status" = "STATUS",
  "Time Test" = "BEGTIME12"
)

shinyServer(function(input, output, session) {
  
  ## Create reactive values object for data -----
  vals <- reactiveValues("Data" = data)
  
  ## Main Body UI: Contains the reactively generated output of the entire page UI -----
  output$mainbody <- renderUI({
    
    fluidPage(

      ## Custom JavaScript: DT checkboxes -----
      tags$script(HTML(
        '$(document).on("click", "input", function () {
          var checkboxes = document.getElementsByName("row_selected");
          var checkboxesChecked = [];
          for (var i=0; i<checkboxes.length; i++) {
            if (checkboxes[i].checked) {
            checkboxesChecked.push(checkboxes[i].value);
            }
          }
          Shiny.onInputChange("checked_rows", checkboxesChecked);
        })'
      )),
      
      # Path to custom CSS
      theme = "mystyle.css",
      
      # Page Title
      title = "Course Schedule",
      br(), 
      div(align = "center", 
          h2("Course Schedules")
          ),
      br(),

      ## Sidebar UI: Settings for left-side sidebar -----
      sidebarLayout(
        sidebarPanel(
          
          ## Sidebar: Required subsetting details -----
          h2("Search by: "), br(), br(),
          checkboxGroupInput("year", "Year",
                             choices = choices_year,
                             inline = TRUE
          ),
          selectInput("term", "Semster/Term",
                      choices = c("Spring Semester" = "SP",
                                  "Spring Term 1" = "S1",
                                  "Spring Term 2" = "S2",
                                  "Summer Session" = "SU",
                                  "Fall Semester" = "FA",
                                  "Fall Term 1" = "F1",
                                  "Fall Term 2" = "F2"),
                      multiple = TRUE,
                      selectize = TRUE
          ),
          selectInput(inputId = "campus", 
                      label = "Campus",
                      choices = choices_campus,
                      multiple = TRUE, 
                      selectize = TRUE
          ),
          checkboxInput("nearby", label = h5("Include nearby campuses")),
          checkboxInput("online", label = h5("Include online classes")),
          checkboxInput("webnetremote", label = h5("Include classes I can join remotely")),
          
          hr(),
          
          ## Sidebar: Optional subsetting details
          h2("Additional options: "), br(), br(),
          
          ### Sidebar: Options for subsetting by program/dept/subject ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Course Type:", 
                                tagList(
                                  selectInput(inputId = "department",
                                              label = "Department",
                                              choices = choices_department,
                                              multiple = TRUE,
                                              selectize = TRUE
                                  ),
                                  selectInput(inputId = "subject_prefix",
                                              label = "Subject",
                                              choices = choices_prefix,
                                              multiple = TRUE,
                                              selectize = TRUE
                                  ),
                                  textInput("course_number",
                                            label = "Course Code",
                                            placeholder = "ABCD 1234"
                                  ),
                                  checkboxGroupInput(
                                    "program_level",
                                    label = "Program Level",
                                    choices = c("Undergraduate" = "UNDG",
                                                "Graduate" = "GRAD"),
                                    selected = c("UNDG", "GRAD"),
                                    inline = TRUE
                                  ),
                                  sliderInput(
                                    "credit_hour",
                                    label = "# of Credit Hours",
                                    min = min(choices_credithour), 
                                    max = max(choices_credithour),
                                    value = c(min(choices_credithour), 
                                              max(choices_credithour)
                                              )
                                  )
                                )
          ),
          
          ### Sidebar: Options for subsetting by date/time ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Date and Time:",
                                tagList(
                                  sliderInput("times_sunday",
                                              label = checkboxInput("day_sunday", label = h5("Sunday"), value = TRUE),
                                              min = as.POSIXct("2016-02-01 00:00"),
                                              max = as.POSIXct("2016-02-01 23:59"),
                                              value = c(as.POSIXct("2016-02-01 08:00"),
                                                        as.POSIXct("2016-02-01 22:00")),
                                              timeFormat = "%H:%M",
                                              ticks = TRUE,
                                              step = 300
                                              ),
                                  sliderInput("times_monday",
                                              label = checkboxInput("day_monday", label = h5("Monday"), value = TRUE),
                                              min = as.POSIXct("2016-02-01 00:00"),
                                              max = as.POSIXct("2016-02-01 23:59"),
                                              value = c(as.POSIXct("2016-02-01 08:00"),
                                                        as.POSIXct("2016-02-01 22:00")),
                                              timeFormat = "%H:%M",
                                              ticks = TRUE,
                                              step = 300
                                  )
                                  )
          ),
          
          ### Sidebar: Options for subsetting by GCP criteria ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Global Citizenship Program (GCP):", 
                                tagList(
                                  selectInput(inputId = "gcpskills", 
                                              label = "Skill Area(s)", 
                                              choices = unname(gcp_skills),
                                              multiple = TRUE,
                                              selectize = TRUE),
                                  selectInput(inputId = "gcpknowledge", 
                                              label = "Knowledge Area(s)", 
                                              choices = unname(gcp_knowledge),
                                              multiple = TRUE,
                                              selectize = TRUE),
                                  checkboxInput("keys", 
                                                label = h5("Include Keystone Seminars")
                                  ),
                                  checkboxInput("frsh", 
                                                label = h5("Include First Year Seminars")
                                  ) 
                                )
          ),
          
          ### Sidebar: Options for subsetting by keyword ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Keyword Search: ", 
                                tagList(
                                  sidebarSearchForm(textId = "searchText", 
                                                    buttonId = "searchButton",
                                                    label = "Search")
                                )
                                
          ),

          div(align = "center",
            tags$img(src="401px-Webster_University_Logo.svg.png", alt = "Webster University Logo")
          ), br(),
          
          ### Sidebar: Feedback link -----
          div(class = "under_sidebar_center",
              actionLink("feedback_link", "Questions? Comments? Issues?"), br(), 
              div(align = "center",
                h6("v0.1.0")
              ),
              br()
              )
        ),
        
        ## Main Panel UI: Settings for main content to right of sidebar -----
        mainPanel(
          actionButton("Remove_from_planner", label = "Remove from planner", icon = icon("times")),
          actionButton("Add_to_planner", label = "Add to planner", icon = icon("plus")),
          
          tabsetPanel(type = "tabs", 
                      tabPanel("Full Schedule", class = "one", icon = icon("calendar"),
                                DT::dataTableOutput("maintable")
                      ),
                      tabPanel("My Planner", class = "one", icon = icon("paperclip"),
                                DT::dataTableOutput("plannertable")
                      )
          ),
          
          ## ADMIN PANEL -----
          conditionalPanel(
            condition = "input.admin_panel == true",
            fixedPanel(
              bottom = "15px",
              right = "15px",
              width = "200px", 
              height = "150px",
              draggable = TRUE,
              div(class = "panel panel-primary",
                  div(class = "panel-heading", "Admin Panel"),
                  div(class = "panel-body",
                      p(Sys.Date()),
                      p(paste("Data load:", file.info(schedule_data_path)$ctime)),
                      p(session$clientData$url_search),
                      checkboxInput("admin_panel",
                                    label = "Hidden Admin Panel Checkbox",
                                    value = FALSE)
                      )
              )
            )
          )
          
        )
      )
    )
  })
  
  ## Main Table: DT UI and data subsetting -----
  output$maintable <- DT::renderDataTable({
    
    ## Create a copy of the reactive data
    DT <- vals$Data
    
    ## Create html for checkboxes into the data data frames, each with a custom id by row number
    DT[["SELECT"]] <- paste0('<input type="checkbox" name="row_selected" value="Row', 1:nrow(vals$Data),'">')
    
    ## Create symbols for course status (cancelled, full, etc.)
    DT$STATUS[DT$stat == "O"] <- paste0(label = "Open ", icon("unlock"))
    DT$STATUS[DT$stat == "R"] <- paste0(label = "Open ", icon("unlock"))
    DT$STATUS[DT$stat == "C"] <- paste0(label = "Closed ", icon("lock"))
    DT$STATUS[DT$stat == "I"] <- paste0(label = "Cancelled ", icon("remove"))
    DT$STATUS[DT$stat == "X"] <- paste0(label = "Cancelled ", icon("remove"))  
    
    ###Create html/js for custom action buttons in the data frame
    ## Button has two actions, one for supplying the input value of the row id (view_details),
    ## and one for observing to trigger the event.  This allows the same button to be clicked 
    ## twice in a row
    DT[["VIEW"]] <- paste0('<div class = "btn-group" role = "group" aria-label="Basic example">
                            <button type = "button" class = "btn btn-default action-button" 
                              onclick = "Shiny.onInputChange(&quot;view_details&quot;,  this.id);
                              Shiny.onInputChange(&quot;last_click&quot;,  Math.random())" 
                              id = button_', 1:nrow(vals$Data),'>View</button>')
    
    ## Subsetting: Required initial subset ------
    
    ##Set a vector to hold the selected campuses
    selected_campuses <- input$campus
    
    ## If nearby option is selected, selected_campuses is replaced with the corresponding
    ## combined campus codes for selected_campuses.  Online is removed if included.
    ## TODO: You can select "nearby" online classes b/c of the WEBG code.  Should be nothing "nearby"
    ## an online class.
    if(input$nearby == TRUE) {
      combine_codes <- unique(DT$COMBINELOC[DT$BUILDINGDESC %in% selected_campuses])
      selected_campuses <- unique(DT$BUILDINGDESC[DT$COMBINELOC %in% combine_codes])
      if("Online" %in% selected_campuses) {
        selected_campuses <- selected_campuses[!selected_campuses %in% "Online"]
      }
    }
    
    ## Subsets the initial data frame to include the year, term, and campuses
    ## All Remote WebNet+ and Online classes in included via the OR statment
    DT <- DT[DT$YEAR %in% input$year &
               DT$TERM %in% input$term &
               (DT$BUILDINGDESC %in% selected_campuses |
               DT$ONLINE == TRUE |
               DT$WEBNET_REMOTE == TRUE), ]
    
    ### Include online classes
    ## Online and WebNet+ classes removed unless one of their "includes" is selected
    if (input$online == FALSE & input$webnetremote == FALSE) {
        DT <- DT[DT$BUILDINGDESC %in% selected_campuses, ]
    }
    
    ##Remove WebNet+ courses if not selected
    if (input$online == TRUE & input$webnetremote == FALSE) {
      DT <- DT[DT$BUILDINGDESC %in% selected_campuses | DT$ONLINE == TRUE, ]
    }
    
    ##Remove Online courses if not selected
    if (input$online == FALSE & input$webnetremote == TRUE) {
      DT <- DT[DT$BUILDINGDESC %in% selected_campuses | DT$WEBNET_REMOTE == TRUE, ]
    }
    
    ### Subsetting Splits for optional includes -----
    ## Data for options that require split/combine, isolated here and rejoined at the bottom
    
    ## Split for "Include Keystone Seminars"
    if (input$keys == TRUE) {
      DTKEYS <- DT[DT$GCPKEYS, ]
    }
    
    ## Split for "Include Freshman Seminars"
    if (input$frsh == TRUE) {
      DTFRSH <- DT[DT$GCPFRSH, ]
    }

    ## Subsetting: program/dept/subject -----
    
    ## Subset by department
    if (!is.null(input$department)) {
      DT <- DT[DT$DEPTTXT %in% input$department, ]
    }
    
    ## Subset by course prefix
    if (!is.null(input$subject_prefix)) {
      DT <- DT[DT$SUBJECT %in% input$subject_prefix, ]
    }
    
    ## Subset by credit hours
    if (!is.null(input$credit_hours)) {
      DT <- DT[DT$CREDITS %in% input$credit_hours, ]
    }
    
    ## Subset by program level
    if (!identical(input$program_level, c("UNDG", "GRAD"))) {
      DT <- DT[DT$PROGRAM %in% input$program_level, ]
    }
    
    ## Subset by course number search
    DT <- DT[grepl(pattern = input$course_number, x = DT$COURSECODE, ignore.case = TRUE), ]
    
    ## Subset by credit hours
    DT <- DT[DT$SECHOURS >= input$credit_hour[1] &
               DT$SECHOURS <= input$credit_hour[2], ]

    ### Subsetting: Global Citizenship Program -----
    ## GCP Subsetting happens by setting the GCPSKILL or GCPKNOWLEDGE column to TRUE 
    ## based on selected items
    
    ##Set GCPSKILL then subset by it
    if (!is.null(input$gcpskills)) {
      if("Critical Thinking" %in% input$gcpskills) {
        DT$GCPSKILL[DT$CRI] <- TRUE
      }
      if("Written Communication" %in% input$gcpskills) {
        DT$GCPSKILL[DT$WCOM] <- TRUE
      }
      if("Oral Communication" %in% input$gcpskills) {
        DT$GCPSKILL[DT$OCOM] <- TRUE
      }
      if("Ethical Reasoning" %in% input$gcpskills) {
        DT$GCPSKILL[DT$ETH] <- TRUE
      }
      if("Intercultural Competence" %in% input$gcpskills) {
        DT$GCPSKILL[DT$INTC] <- TRUE
      }
      DT <- DT[DT$GCPSKILL, ]
    }
    
    ##Set GCPKNOWLEDGE then subset by it
    if (!is.null(input$gcpknowledge)) {
      if("Social Systems & Human Behaviors" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$SSHB] <- TRUE
      }
      if("Roots of Cultures" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$ROC] <- TRUE
      }
      if("Global Understaning" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$GLBL] <- TRUE
      }
      if("Physical & Natural World" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$PNW] <- TRUE
      }
      if("Arts Appreciation" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$ARTS] <- TRUE
      }
      if("Quantitative Literacy" %in% input$gcpknowledge) {
        DT$GCPKNOWLEDGE[DT$QL] <- TRUE
      }
      DT <- DT[DT$GCPKNOWLEDGE, ]
    }
    
    ### Subsetting: Date/Time -----
    
    ### Subsetting: Keyword search-----
  
    DT <- DT[grepl(pattern = input$searchText, x = DT$DESC_CLEAN, ignore.case = TRUE), ]
    
    ### Subsetting Combines for optional includes -----
    
    ##Rejoin Keystone Seminar
    if (input$keys == TRUE) {
      DT <- unique(rbind(DT, DTKEYS)) ## Does this result in duplicate KEYS?
    }
    
    ##Rejoin FRSH Seminar
    if (input$frsh == TRUE) {
      DT <- unique(rbind(DT, DTFRSH))##Does this result in duplicate KEYS?
    }
    
    ## Post-Subsetting: Clean up before DT creation -----
    DT <- DT[, cols]
    
    ## Post-Subsetting: Sort -----
    DT <- DT %>% arrange(desc(STATUS), SECNO, COURSECODE)
    
    ### Post-Subsetting: DT creation -----
    ## Use the DT package to turn the data fram into a a JS table 
    DT::datatable(DT, escape = FALSE, 
                  rownames = FALSE,
                  select = "none",
                  colnames = cols,
                  extensions = c('Responsive'),
                  options = list(sDom  = '<"top">lrt<"bottom">ip',
                                 language = list(
                                   zeroRecords = "No courses to display.  Change search options."
                                 ))
    )
  })
  
  ## Planner Table: UI and Subsetting -----
  output$plannertable <- DT::renderDataTable({
    
    ## Create another copy of the reactive data
    DTplan <- vals$Data
    
    ###Same additions as the Main Table
    ## TODO: Move the creation of these columns into a function
    ## Create html for checkboxes into the data data frames, each with a custom id by row number
    DTplan[["SELECT"]] <- paste0('<input type="checkbox" name="row_selected" value="Row', 1:nrow(vals$Data),'">')
    
    ## Create symbols for course status (cancelled, full, etc.)
    DTplan$STATUS[DTplan$stat == "O"] <- paste0(label = "Open ", icon("unlock"))
    DTplan$STATUS[DTplan$stat == "R"] <- paste0(label = "Open ", icon("unlock"))
    DTplan$STATUS[DTplan$stat == "C"] <- paste0(label = "Closed ", icon("lock"))
    DTplan$STATUS[DTplan$stat == "I"] <- paste0(label = "Cancelled ", icon("remove"))
    DTplan$STATUS[DTplan$stat == "X"] <- paste0(label = "Cancelled ", icon("remove"))  
    
    ###Create html/js for custom action buttons in the data frame
    ## Button has two actions, one for supplying the input value of the row id (view_details),
    ## and one for observing to trigger the event.  This allows the same button to be clicked 
    ## twice in a row
    DTplan[["VIEW"]] <- paste0('<div class = "btn-group" role = "group" aria-label="Basic example">
                           <button type = "button" class = "btn btn-default action-button" 
                           onclick = "Shiny.onInputChange(&quot;view_details&quot;,  this.id);
                           Shiny.onInputChange(&quot;last_click&quot;,  Math.random())" 
                           id = button_', 1:nrow(vals$Data),'>View</button>')
    
    ##Subset the Planner table to only the items marked as being in the planner
    DTplan <- DTplan[DTplan$PLANNER == TRUE, ]
    
    ##Trim to include only the colums we want in the DT
    DTplan <- DTplan[, cols]

    DT::datatable(DTplan, 
                  escape = FALSE, 
                  rownames = FALSE,
                  select = "none",
                  colnames = cols,
                  extensions = c('Buttons', 'ColReorder', 'Responsive'),
                  options = list(dom = 'Bfrtip', 
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 colReorder = list(realtime = FALSE),
                                 language = list(
                                   zeroRecords = "No courses added to planner."
                                   )
                  )
    )
  })
  
  ### Observe Event: View Details Modal -----
  observeEvent(input$last_click, {
    
    ##Extract the row number from the javascript onclick
    selected_row <- as.numeric(strsplit(input$view_details, "_")[[1]][2])
    
    ##Isolate the data for the selected row
    section_data <- vals$Data[selected_row, ]
    
    ### View Details Modal Consruction -----
    
    ## Create a title object from fields
    title <- paste(section_data$COTITLE, "-", 
                   section_data$COURSECODE, 
                   section_data$YEAR, 
                   section_data$TERM
    )
    
    ## Create a Concourse link object
    concourseid <- paste(section_data$YEAR, section_data$TERM, 
                         section_data$SUBJECT, section_data$COURSENO, 
                         section_data$SECNO, sep = "_")
    concourse_url <- paste0("https://api.apidapter.com/v0/websterfdc/concourse_linker_1?course=", concourseid)
    concourse_url <- a("Click to View Syllabus in Concourse.", target = "_blank", href = concourse_url)
    
    ## Create the HTML object for the modal
    msg <- HTML(
      paste(
        p(paste("Location:", section_data$BUILDINGDESC)),
        ##Add the building and room number
        p(paste("Instructor:", section_data$Name)),
        p(paste("Instructor Email:", section_data$wuEMAL)),
        p(paste("Description:", section_data$DESC_CLEAN)),
        p(section_data$ATTS),
        p(concourse_url)
      )
    )
    
    ## Display the modal
    showModal(modalDialog(
      title = title,
      msg,
      easyClose = TRUE
    ))
  })
  
  ### Observe Event: Planner ADD button -----
  observeEvent(input$Add_to_planner,{
    row_to_add <- as.numeric(gsub("Row","",input$checked_rows))
    vals$Data$PLANNER[row_to_add] <- TRUE
  })
    
  ### Observe Event: Planner REMOVE button -----
  observeEvent(input$Remove_from_planner,{
    row_to_add <- as.numeric(gsub("Row","",input$checked_rows))
    vals$Data$PLANNER[row_to_add] <- FALSE
  })
  
  ### Observe Event: Feedback Modal  -----
  observeEvent(input$feedback_link,{
    showModal(
      modalDialog(
        tags$iframe(src = feedback_url,
                    width = "100%", height = "400px"), 
        size = "l"
      )
    )
  })
  
  ## Query String: Parse the url query string into usable actions -----
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    ## Select year
    if (!is.null(query$year)) {
      updateCheckboxGroupInput(session, "year", selected = query$year)
    }
    
    ## Select term
    if (!is.null(query$term)) {
      updateSelectInput(session, "term", selected = query$term)
    }
    
    ## Select campus
    if (!is.null(query$campus)) {
      updateSelectInput(session, "campus", selected = query$campus)
    }
    
    ## Select nearby campuses
    if (!is.null(query$nearby)) {
      updateCheckboxInput(session, "nearby", value = as.logical(query$nearby))
    }
    
    ## Select online courses
    if (!is.null(query$online)) {
      updateCheckboxInput(session, "online", value = as.logical(query$online))
    }
    
    ## Select Webnet+
    if (!is.null(query$webnetremote)) {
      updateCheckboxInput(session, "webnetremote", value = as.logical(query$webnetremote))
    }
    
    ## Select department
    if (!is.null(query$department)) {
      updateSelectInput(session, "department", selected = query$department)
    }
    
    ## Select subject
    if (!is.null(query$subject_prefix)) {
      updateSelectInput(session, "subject_prefix", selected = query$subject_prefix)
    }
    
    ## Enter course code
    if (!is.null (query$course_number)) {
      updateTextInput(session, "course_number", value = query$course_number)
    }
    
    ## Select program level
    if (!is.null(query$program_level)) {
      updateCheckboxGroupInput(session, "program_level", selected = query$program_level)
    }
    
    ## Select credit hour range
    if (!is.null(query$credit_hour)) {
      updateSliderInput(session, "credit_hour", 
                        value = query$credit_hour,  
                        step = 1) 
    }
    
    ## Select skill areas
    if (!is.null(query$gcpskills)) {
      updateSelectInput(session, "gcpskills", selected = query$gcpskills)
    }
    
    ## Select knowledge areas
    if (!is.null(query$gcpknowledge)) {
      updateSelectInput(session, "gcpknowledge", selected = query$gcpknowledge)
    }
    
    ## Select Keystone courses
    if (!is.null(query$keys)) {
      updateCheckboxInput(session, "keys", value = as.logical(query$keys))
    }
    
    ## Select First-Year Seminars
    if (!is.null(query$frsh)) {
      updateCheckboxInput(session, "frsh", value = as.logical(query$frsh))
    }
    
    ## Enter keyword search
    if (!is.null(query$searchText)) {
      updateCheckboxGroupInput(session, "searchText", selected = query$searchText)
    }
    
    ## Open Hidden Admin Panel
    if (!is.null(query$admin)) {
      updateCheckboxInput(session, "admin_panel", value = as.logical(query$admin))
    }
    
  })
  
})
