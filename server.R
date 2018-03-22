library(shiny)
library(shinythemes)
library(shinyBS)
#library(bsplus)
library(DT)
source("00-Functions.R")
source("01-Configs.R")

##Load the data
data <- loadFrame(schedule_data_path)

##Create new colummns
data$VIEW <- ""
data$PLANNER <- FALSE
data$GCPSKILL <- FALSE
data$GCPKNOWLEDGE <- FALSE
data$GCPKEYS <- ifelse(data$SUBJECT == "KEYS", TRUE, FALSE)
data$GCPFRSH <- ifelse(data$SUBJECT == "FRSH", TRUE, FALSE)


##Set some variable from the complete available data set
default_terms <- names(defaultTerms())[unlist(defaultTerms())]
choices_year <- unique(data$YEAR[order(data$YEAR)])
choices_campus <- unique(data$BUILDINGDESC[order(data$BUILDINGDESC)])
choices_department <- unique(data$DEPTTXT[order(data$DEPTTXT)])
choices_prefix <- unique(data$SUBJECT[order(data$SUBJECT)])

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
cols <- c("Select" = "SELECT",
          "View Details" = "VIEW", 
          "Course Code" = "COURSECODE",
          "Course Title" = "COTITLE",
          "Section #" = "SECNO", 
          "Meeting Days" = "Days", 
          "Meeting Start Time" = "Beginning Time", 
          "Meeting End Time" = "Ending Time",
          "Course Location" = "BUILDINGDESC",
          "Meeting Type" = "IMDESC",
          "Instructor Email" = "wuEMAL"
          )

shinyServer(function(input, output, session) {
      vals <- reactiveValues()
      vals$Data <- data
      
      output$mainbody <- renderUI({
            fluidPage(
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
                 
                  ## Custom Webster branded theme
                  theme = "mystyle.css",
                  
                  title = "Course Schedules",
                  br(), br(), br(), br(), br(), 
                  br(), br(), br(), br(), 
                  tags$h2(HTML("<center>Course Schedules</center>")),
                  br(),

                  
                  sidebarLayout(
                          sidebarPanel(
                                  h2("Search by: "), br(), br(),
                                  checkboxGroupInput("year", "Year",
                                                     choices = choices_year,
                                                     selected = defaultYear(),
                                                     inline = TRUE
                                                ),
                                  selectInput("term", "Session",
                                              choices = c("Spring Semester" = "SP",
                                                          "Spring Term 1" = "S1",
                                                          "Spring Term 2" = "S2",
                                                          "Summer Session" = "SU",
                                                          "Fall Semester" = "FA",
                                                          "Fall Term 1" = "F1",
                                                          "Fall Term 2" = "F2"),
                                              selected = default_terms,
                                              multiple = TRUE,
                                              selectize = TRUE
                                                ),
                                  selectInput(inputId = "campus", 
                                              label = "Campus",
                                              choices = choices_campus,
                                              multiple = TRUE, 
                                              selectize = TRUE
                                                ),
                                  checkboxInput("nearby", label = h5("Include Nearby Campuses")),
                                  checkboxInput("online", label = h5("Include Online Classes")),
                                  hr(),

                                  h2("Additional options: "), br(), br(),
                                 
                                  customSBCollapsePanel("Course Type:", 
                                                        tagList(
                                                              selectInput(inputId = "department",
                                                                          label = "Department",
                                                                          choices = choices_department,
                                                                          multiple = TRUE,
                                                                          selectize = TRUE
                                                              ),
                                                              selectInput(inputId = "program_level",
                                                                          label = "Program Level",
                                                                          choices = c("GRAD", "UNDG"),
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
                                                                        label = "Course Number"
                                                              )
                                                        )
                                  ),
                                  
                                  # customSBCollapsePanel("Date and Time:",
                                  #                       tagList(
                                  #                             checkboxGroupInput("days", 
                                  #                                                label = "Days", 
                                  #                                                choices = list("Monday" = "M", 
                                  #                                                               "Tuesday" = "T",
                                  #                                                               "Wednesday" = "W",
                                  #                                                               "Thursday" = "Th",
                                  #                                                               "Friday" = "F",
                                  #                                                               "Saturday" = "S"),
                                  #                                                selected = "Monday"),
                                  #                             sliderInput("date_range", 
                                  #                                         "Times", 
                                  #                                         min = as.POSIXct("2016-02-01 00:00"),
                                  #                                         max = as.POSIXct("2016-02-01 23:59"),
                                  #                                         value = c(as.POSIXct("2016-02-01 08:00"),
                                  #                                                   as.POSIXct("2016-02-01 22:00")),
                                  #                                         timeFormat = "%H:%M", 
                                  #                                         ticks = TRUE,
                                  #                                         step = 300
                                  #                             )
                                  #                       )
                                  #                       ),
                                  
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
                                                                            label = h5("Include First-year Seminars")
                                                              ) 
                                                        )
                                  ),
                                  customSBCollapsePanel("Keyword Search:", 
                                                        textInput("keyword",
                                                                  label = "Enter word or phrase")
                                                        ),
                                  actionButton("Add_to_planner", label = h5("Add to planner")),
                                  actionButton("Remove_from_planner", label = h5("Remove from planner"))
                                ),
                  
                  mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Full Schedule", class = "one",
                                               DT::dataTableOutput("maintable")
                                               ),
                                      tabPanel("My Planner", class = "one",
                                               DT::dataTableOutput("plannertable")
                                               )
                                     )
                           )
                  )
        )
})

      output$maintable <- DT::renderDataTable({
            DT <- vals$Data
            DT[["SELECT"]] <- paste0('<input type="checkbox" name="row_selected" value="Row', 1:nrow(vals$Data),'">')
            DT[["VIEW"]] <- paste0('
                                   <div class = "btn-group" role = "group" aria-label="Basic example">
                                   <button type = "button" 
                                          class = "btn btn-default action-button" 
                                          onclick="Shiny.onInputChange(&quot;view_details&quot;,  this.id);
                                                Shiny.onInputChange(&quot;last_click&quot;,  Math.random())" 
                                          id = button_', 1:nrow(vals$Data),'>View</button>
                                   ')
            #<button type = "button" class = "btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;view_details&quot;,  this.id)\" id = button_', 1:nrow(vals$Data),'>View</button>
            #Shiny.onInputChange('lastClick', Math.random()
            
            ##Required values
            #Set a vector to hold the selected campuses
            selected_campuses <- input$campus
            


            if(input$nearby == TRUE) {
                  combine_codes <- unique(DT$COMBINELOC[DT$BUILDINGDESC %in% selected_campuses])
                  selected_campuses <- unique(DT$BUILDINGDESC[DT$COMBINELOC %in% combine_codes])
                  if("Online" %in% selected_campuses){
                        selected_campuses <- selected_campuses[!selected_campuses %in% "Online"]
                  }
                        
            }
            
            DT <- DT[DT$YEAR %in% input$year &
                  DT$TERM %in% input$term &
                  DT$BUILDINGDESC %in% selected_campuses |##This OR is problematic, fix
                  DT$ONLINE == TRUE, ]
            

            
            ##Optional values
            if (!input$online == TRUE) {
                  DT <- DT[DT$BUILDINGDESC %in% selected_campuses, ]
            }
            
            #######These are the "include" options, isolated here and rejoined at the bottome
            if (input$keys == TRUE) {
                  DTKEYS <- DT[DT$GCPKEYS, ]
            }
            
            if (input$frsh== TRUE) {
                  DTFRSH <- DT[DT$GCPFRSH, ]
            }
            #######
            
            if (!is.null(input$department)) {
                  DT <- DT[DT$DEPTTXT %in% input$department, ]
            }
            
            if (!is.null(input$subject_prefix)) {
                  DT <- DT[DT$SUBJECT%in% input$subject_prefix, ]
            }
            
            
            
            ##GCP Subsetting happens by setting the skill or knowledge column to TRUE based on selected items
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
            


            
            ##keyword <- DT[grep(keyword, DT$COTITLE)]
            
            ##Rejoin Includes
            if (input$keys == TRUE) {
                  DT <- unique(rbind(DT, DTKEYS))##Does this result in duplicate KEYS?
            }
            
            if (input$frsh == TRUE) {
                  DT <- unique(rbind(DT, DTFRSH))##Does this result in duplicate KEYS?
            }
            
            
            DT::datatable(DT, escape = FALSE, 
                          rownames = FALSE,
                          select = "none",
                          colnames = cols,
                          extensions = c('ColReorder', 'Responsive'),
                          options = list(sDom  = '<"top">lrt<"bottom">ip',
                                         colReorder = list(realtime = FALSE))
                          ) %>% formatStyle(columns = "stat", 
                                            target = "row", 
                                            backgroundColor = styleEqual(c("C", "X"), 
                                                                         c("yellow", "red")
                                                                         )
                                            )
            
            #DT <- DT[, cols]##Trim to include one the colums we want in the DT
            #colnames(DT) <- names(cols)
      })
      
      output$plannertable <- DT::renderDataTable({
            DTplan <- vals$Data
            DTplan[["SELECT"]] <- paste0('<input type="checkbox" name="row_selected" value="Row', 1:nrow(vals$Data),'">')
            DTplan[["VIEW"]] <- paste0('
                                    <div class = "btn-group" role = "group" aria-label="Basic example">
                                    <button type = "button" class = "btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;view_details&quot;,  this.id)\" id = button_', 1:nrow(vals$Data),'>View</button>
                                    ')
            
            DTplan <- DTplan[DTplan$PLANNER == TRUE, ]
            
            DTplanview <- DTplan[, cols]##Trim to include one the colums we want in the DT
            colnames(DTplanview) <- names(cols)
             
            DT::datatable(DTplanview, escape = FALSE, select = "none",
                          extensions = 'Buttons', options = list(dom = 'Bfrtip', 
                                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                                                 )
                          )
      })
      
      # observeEvent(input$view_details, {
      #       selected_row <- as.numeric(strsplit(input$view_details, "_")[[1]][2])
      #       section_data <- vals$Data[selected_row, ]
      #       
      #       msg <- paste(section_data$COURSECODE, section_data$YEAR, section_data$TERM, "is taught by", section_data$Name, "at", section_data$SECLOC)
      #       showModal(modalDialog(
      #             title = "Important message",
      #             msg,
      #             easyClose = TRUE
      #       ))
      # })
      
      observeEvent(input$view_details, {
            selected_row <- as.numeric(strsplit(input$view_details, "_")[[1]][2])
            section_data <- vals$Data[selected_row, ]
            
            title <- paste(section_data$COTITLE, "-", 
                           section_data$COURSECODE, 
                           section_data$YEAR, 
                           section_data$TERM
                           )
            
            concourseid <- paste(section_data$YEAR, section_data$TERM, 
                                 section_data$SUBJECT, section_data$COURSENO, 
                                 section_data$SECNO, sep = "_")
            concourse_url <- paste0("https://api.apidapter.com/v0/websterfdc/concourse_linker_1?course=", concourseid)
            concourse_url <- a("Click to View Syllabus in Concourse.", target = "_blank", href = concourse_url)
            
            msg <- HTML(
                        paste(
                              p(paste("Location:", section_data$BUILDINGDESC)),
                              p(paste("Instructor:", section_data$Name)),
                              p(concourse_url), 
                              p(paste("Description:", section_data$DESC)),
                              p(paste("Notes:", section_data$ATTS))
                        )
                  )
            #msg <- paste(section_data$COURSENO, section_data$YEAR, section_data$TERM, "is taught by", section_data$Name, "at", section_data$SECLOC)
            
            showModal(modalDialog(
                  title = title,
                  msg,
                  easyClose = TRUE
                  ))
      })
      
      observeEvent(input$last_click, {
            ##This is a secondary action on the view button
            ##how can we use it to allow someone to view twice in a row
            selected_row <- as.numeric(input$last_click)
      })
      
      observeEvent(input$Add_to_planner,{
            row_to_add <- as.numeric(gsub("Row","",input$checked_rows))
            vals$Data$PLANNER[row_to_add] <- TRUE
      })
      
      observeEvent(input$keyword_search,{
      })
      
      observeEvent(input$Remove_from_planner,{
            row_to_add <- as.numeric(gsub("Row","",input$checked_rows))
            vals$Data$PLANNER[row_to_add] <- FALSE
      })
})
