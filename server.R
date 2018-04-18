library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(knitr)
source("00-Functions.R")
source("01-Configs.R")
#source("02-DateAndTime.R")

##Load the data
data <- readRDS(schedule_data_path)

##Set some variable from the complete available data set
choices_year <- unique(data$yr[order(data$yr)])
choices_campus <- unique(data$com_bldg_table.txt[order(data$com_bldg_table.txt)])
choices_department <- unique(data$stu_dept_table.txt[order(data$stu_dept_table.txt)])
choices_prefix <- unique(data$app.crs_prefix[order(data$app.crs_prefix)])
choices_credithour <- unique(as.numeric(data$hrs[order(data$hrs)]))

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
  " " = "app.select",
  "Details" = "app.view", 
  "Title" = "calc.title",
  "Course" = "crs_no",
  "Section" = "sec_no", 
  "Credit Hours" = "hrs",
  "Location" = "stu_dept_table.txt",
  "Meeting Days" = "app.days", 
  "Meeting Time" = "app.beg_tm.12",
  "Instructor" = "com_id_rec.lastname",
  "Status" = "app.status")

shinyServer(function(input, output, session) {
  
  ## Create reactive values object for data -----
  vals <- reactiveValues("schedule_data" = data)
  
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
          selectInput("term", "Semester/Term",
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
          
          ### Sidebar: Options for subsetting by date/time ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Date and Time:",
                                tagList(
                                  helpText("You my limit your results by day of week or time of day.
                                           Course with no specified meeting day, such as online classes, 
                                           are not filtered."),
                                  radioButtons("days_mode",
                                              "Date Filter Type",
                                              choices = c(
                                                "Courses with any meetings on these days." = "any",
                                                "Courses that only meet in these days." = "only")
                                                ),
                                  checkboxGroupInput("days_selection",
                                                     "Days", 
                                                     choices = c("Sunday" = "U",
                                                                 "Monday" = "M",
                                                                 "Tuesday" = "T",
                                                                 "Wednesday" = "W",
                                                                 "Thursday" = "R",
                                                                 "Friday" = "F",
                                                                 "Saturday" = "S"), 
                                                     inline = TRUE,
                                                     selected = c("Sunday" = "U",
                                                                  "Monday" = "M",
                                                                  "Tuesday" = "T",
                                                                  "Wednesday" = "W",
                                                                  "Thursday" = "R",
                                                                  "Friday" = "F",
                                                                  "Saturday" = "S")
                                                     ),
                                  sliderInput("times_selection",
                                              label = "Times",
                                              min = as.POSIXct("2018-04-02 00:00", tz = "UTC"),
                                              max = as.POSIXct("2018-04-02 23:59", tz = "UTC"),
                                              value = c(as.POSIXct("2018-04-02 08:00", tz = "UTC"),
                                                        as.POSIXct("2018-04-02 22:00", tz = "UTC")),
                                              timeFormat = "%H:%M",
                                              timezone = "+0000",
                                              ticks = TRUE,
                                              step = 300
                                              )
                                )
          ),
          
          ### Sidebar: Options for subsetting by program/dept/subject ----
          ## customSBCollapsePanel() is a custom function for making collapsable sidebar menus
          ## Items in customSBCollapsePanel() must be in a tagList()
          customSBCollapsePanel("Course Details:", 
                                tagList(
                                  checkboxGroupInput(
                                    "program_level",
                                    label = "Program Level",
                                    choices = c("Undergraduate" = "UNDG",
                                                "Graduate" = "GRAD"),
                                    selected = c("UNDG", "GRAD"),
                                    inline = TRUE
                                  ),
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
          ## END ADMIN PANEL
        )
      )
    )
  })
  
  ## Main Table: DT UI and data subsetting -----
  output$maintable <- DT::renderDataTable({
    
    ## Create a copy of the reactive data
    DT <- vals$schedule_data
    
    ## Subsetting: Required initial subset ------
    
    ## Hide anything that shouldn't bee seen from 
    DT <- DT[DT$app.index.show == TRUE, ]
    
    ##Set a vector to hold the selected campuses
    selected_campuses <- input$campus
    
    ## If nearby option is selected, selected_campuses is replaced with the corresponding
    ## combined campus codes for selected_campuses.  Online is removed if included.
    ## TODO: You can select "nearby" online classes b/c of the WEBG code.  Should be nothing "nearby"
    ## an online class.
    if(input$nearby == TRUE) {
      combine_codes <- unique(DT$com_bldg_table.combine_camp_code[DT$com_bldg_table.txt %in% selected_campuses])
      selected_campuses <- unique(DT$com_bldg_table.txt[DT$com_bldg_table.combine_camp_code %in% combine_codes])
      if("Online" %in% selected_campuses) {
        selected_campuses <- selected_campuses[!selected_campuses %in% "Online"]
      }
    }
    
    ## Subsets the initial data frame to include the year, term, and campuses
    ## All Remote WebNet+ and Online classes in included via the OR statment
    DT <- DT[DT$yr %in% input$year &
               DT$faid_altacad_cal.term %in% input$term &
               (DT$com_bldg_table.txt %in% selected_campuses |
               DT$index.online == TRUE |
               DT$index.webnet_remote == TRUE), ]
    
    ### Include online classes
    ## Online and WebNet+ classes removed unless one of their "includes" is selected
    if (input$online == FALSE & input$webnetremote == FALSE) {
        DT <- DT[DT$com_bldg_table.txt %in% selected_campuses, ]
    }
    
    ##Remove WebNet+ courses if not selected
    if (input$online == TRUE & input$webnetremote == FALSE) {
      DT <- DT[DT$com_bldg_table.txt %in% selected_campuses | DT$index.online == TRUE, ]
    }
    
    ##Remove Online courses if not selected
    if (input$online == FALSE & input$webnetremote == TRUE) {
      DT <- DT[DT$com_bldg_table.txt %in% selected_campuses | DT$index.webnet_remote == TRUE, ]
    }
    
    ## Subsetting: Date/time -----
    
    if (input$days_mode == "any") {
      DT <- DT[grepl(paste(input$days_selection, collapse = "|"), DT$app.days) | 
                 DT$index.online == TRUE |
                 DT$index.webnet_remote == TRUE, ]
    }
    
    if (input$days_mode == "only") {
      
      if (!"U" %in% input$days_selection) {
        DT <- DT[!(DT$index.sunday & DT$index.online == FALSE), ]
      }

      if (!"M" %in% input$days_selection) {
        DT <- DT[!(DT$index.monday & DT$index.online == FALSE), ]
      }

      if (!"T" %in% input$days_selection) {
        DT <- DT[!(DT$index.tuesday & DT$index.online == FALSE), ]
      }

      if (!"W" %in% input$days_selection) {
        DT <- DT[!(DT$index.wednesday & DT$index.online == FALSE), ]
      }

      if (!"R" %in% input$days_selection) {
        DT <- DT[!(DT$index.thursday & DT$index.online == FALSE), ]
      }

      if (!"F" %in% input$days_selection) {
        DT <- DT[!(DT$index.friday & DT$index.online == FALSE), ]
      }

      if (!"S" %in% input$days_selection) {
        DT <- DT[!(DT$index.saturday & DT$index.online == FALSE), ]
      }
      
    }
    
    aaa <<- format(input$times_selection[1], "%H%M")
    bbb <<- format(DT$app.beg_tm, "%H%M")             
    
    DT <- DT[(format(DT$app.beg_tm, "%H%M") >= format(input$times_selection[1], "%H%M") &
               format(DT$app.beg_tm, "%H%M") <= format(input$times_selection[2], "%H%M")) |
               DT$index.online == TRUE, ]
    
    ## Subsetting Splits for optional includes -----
    ## Data for options that require split/combine, isolated here and rejoined at the bottom
    
    ## Split for "Include Keystone Seminars"
    if (input$keys == TRUE) {
      DTKEYS <- DT[DT$index.keys, ]
    }
    
    ## Split for "Include Freshman Seminars"
    if (input$frsh == TRUE) {
      DTFRSH <- DT[DT$index.frsh, ]
    }

    ## Subsetting: program/dept/subject -----
    
    ## Subset by department
    if (!is.null(input$department)) {
      DT <- DT[DT$stu_dept_table.txt %in% input$department, ]
    }
    
    ## Subset by course prefix
    if (!is.null(input$subject_prefix)) {
      DT <- DT[DT$app.crs_prefix %in% input$subject_prefix, ]
    }
    
    ## Subset by credit hours
    if (!is.null(input$credit_hours)) {
      DT <- DT[DT$hrs %in% input$credit_hours, ]
    }
    
    ## Subset by program level
    if (!identical(input$program_level, c("UNDG", "GRAD"))) {
      DT <- DT[DT$stu_course_rec.prog %in% input$program_level, ]
    }
    
    ## Subset by course number search
    DT <- DT[grepl(pattern = input$course_number, x = DT$crs_no, ignore.case = TRUE), ]
    
    ## Subset by credit hours
    DT <- DT[DT$hrs >= input$credit_hour[1] &
               DT$hrs <= input$credit_hour[2], ]

    ### Subsetting: Global Citizenship Program -----
    
    ##Set app.index.gcp.skill then subset by it
    if (!is.null(input$gcpskills)) {
      if("Critical Thinking" %in% input$gcpskills) {
        DT$app.index.gcp.skill[DT$index.gcp.crit] <- TRUE
      }
      if("Written Communication" %in% input$gcpskills) {
        DT$app.index.gcp.skill[DT$index.gcp.wcom] <- TRUE
      }
      if("Oral Communication" %in% input$gcpskills) {
        DT$app.index.gcp.skill[DT$index.gcp.ocom] <- TRUE
      }
      if("Ethical Reasoning" %in% input$gcpskills) {
        DT$app.index.gcp.skill[DT$index.gcp.ethc] <- TRUE
      }
      if("Intercultural Competence" %in% input$gcpskills) {
        DT$app.index.gcp.skill[DT$index.gcp.intc] <- TRUE
      }
      DT <- DT[DT$app.index.gcp.skill, ]
    }
    
    ##Set app.index.gcp.knowledge then subset by it
    if (!is.null(input$gcpknowledge)) {
      if("Social Systems & Human Behaviors" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.sshb] <- TRUE
      }
      if("Roots of Cultures" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.roc] <- TRUE
      }
      if("Global Understaning" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.glbl] <- TRUE
      }
      if("Physical & Natural World" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.pnw] <- TRUE
      }
      if("Arts Appreciation" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.arts] <- TRUE
      }
      if("Quantitative Literacy" %in% input$gcpknowledge) {
        DT$app.index.gcp.knowledge[DT$index.gcp.ql] <- TRUE
      }
      DT <- DT[DT$app.index.gcp.knowledge, ]
    }
    
    ### Subsetting: Keyword search-----
  
    DT <- DT[grepl(pattern = input$searchText, x = DT$catalog.description, ignore.case = TRUE), ]
    
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
    DT <- DT %>% arrange(desc(app.status), sec_no, crs_no)
    
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
    DTplan <- vals$schedule_data
    
    ## Hide anything that shouldn't bee seen from 
    DTplan <- DTplan[DTplan$app.index.show == TRUE, ]
    
    ##Subset the Planner table to only the items marked as being in the planner
    DTplan <- DTplan[DTplan$app.index.planner == TRUE, ]
    
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
    section_data <- vals$schedule_data[selected_row, ]
    
    ### View Details Modal Consruction -----
    
    ## Create a title object from fields
    title <- paste(section_data$calc.title, "-", 
                   section_data$crs_no, 
                   section_data$yr, 
                   section_data$faid_altacad_cal.term
    )
    
    ## Create a Concourse link object
    concourse <- a("Click to View Syllabus in Concourse.", target = "_blank", href = section_data$app.concourse.tmpl)
    
    ## Create the HTML object for the modal
    msg <- HTML(
      paste(
        p(paste("Location:", section_data$sec_loc)),
        ##Add the building and room number
        p(paste("Instructor:", section_data$com_id_rec.firstname, section_data$com_id_rec.lastname)),
        p(paste("Instructor Email:", section_data$com_id_rec.wugetemal)),
        p(paste("Description:", section_data$catalog.description)),
        p(section_data$catalog.atts),
        p(concourse)
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
    vals$schedule_data$app.index.planner[row_to_add] <- TRUE
  })
    
  ### Observe Event: Planner REMOVE button -----
  observeEvent(input$Remove_from_planner,{
    row_to_remove <- as.numeric(gsub("Row","",input$checked_rows))
    vals$schedule_data$app.index.planner[row_to_remove] <- FALSE
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
