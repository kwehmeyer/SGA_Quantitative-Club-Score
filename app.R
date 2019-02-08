#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
    

    # Application title
    navbarPage("SGA Quantitative Club Scoring System",
               
               tabPanel("Score Card", sidebarLayout(
                   sidebarPanel(
                       h1("Preliminary Information"),
                       textInput("name","Club Name"),
                       selectInput("cat", "Club Type:",c("Academic", "Gaming","Outdoor","Recreation", "Religious","Professional Organization")),
                       sliderInput("mm", "Monthly Meetings To Date",1, min = 0, max = 20),
                       hr(),
                       h1("Club Performance Stats"),
                       numericInput("budget","Budget Use Percentage",0, min = 0, max = 100),
                       numericInput("mp","Missing or Late Paperwork",0, min = 0, max = 100),
                       numericInput("missed_meetings","Monthly Meetings Missed",0, min = 0, max = 100),
                       numericInput("bad_assets","Missing/Broken Assets",0, min = 0, max = 1000),
                       hr(),
                       h1("Meetings"),
                       numericInput("gen_meetings","Number of General Meetings",0, min = 0, max = 1000),
                       numericInput("gm_attendance","Average Attendace",0, min = 0, max = 1000),
                       numericInput("eboard_meetings","Number of E-Board Meetings",0, min = 0, max = 1000),
                       numericInput("events","Events",0, min = 0, max = 1000),
                       numericInput("event_attendance","Event Attendance",0, min = 0, max = 1000),
                       actionButton("update", "Submit Score")
                       
                       
                       
                   ),
                   mainPanel(
                       h1(textOutput("perf_result")),
                       h2("E-Board Performance"),
                       hr(),
                       h1(textOutput("social_result")),
                       h2("Social Score"),
                       hr(),
                       h1(textOutput("total_result")),
                       h2("Total Score"),
                       hr(),
                       tableOutput("table1"),
                       downloadButton("download", "Download CSV")
                       
                   )
               )),
               tabPanel("Weights",
                        h1("Club Category Bonus"),
                        numericInput("academic_bonus","Academic Bonus",3, min = 0, max = 5),
                        numericInput("gaming_bonus","Gaming Bonus",1, min = 0, max = 5),
                        numericInput("outdoor_bonus","Outdoor Bonus",2, min = 0, max = 5),
                        numericInput("recreation_bonus","Recreation Bonus",1, min = 0, max = 5),
                        numericInput("religious_bonus","Religious Bonus",1, min = 0, max = 5),
                        numericInput("po_bonus","Professional Organization Bonus",3, min = 0, max = 5),
                        h1("Club Performance Weights"),
                        numericInput("budget_threshold","Budget Threshold",75, min = 0, max = 100),
                        numericInput("budget_weight","Budget Weight",1.3, min = 0, max = 100),
                        numericInput("mlp_weight","Missing/Late Paperwork Weight",0.15, min = 0, max = 5),
                        numericInput("mba_weight","Missing/Broken Assets Weight",0.5, min = 0, max = 5),
                        h1("Meetings Weight"),
                        numericInput("gm_weight","Meetings Weight",0.2, min = 0, max = 40),
                        numericInput("gmat_weight","Meeting Attendance Weight",100, min = 0, max = 500),
                        numericInput("eb_weight","E-Board Meetings Weight",0.1, min = 0, max = 20),
                        numericInput("event_weight","Events Weight",0.5, min = 0, max = 15),
                        numericInput("eventat_weight","Event Attendance Weight",50, min = 0, max = 500),
                        hr(),
                        h4("Please consider reading the README.md file found on Github"),
                        h5("https://github.com/Khanzi/SGA_Quantitative-Club-Score")
                        
                        
                        
                        )
               )
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

        

# Score-Card-Server -------------------------------------------------------

    
    
    output$download <- downloadHandler(
        filename = function() {
            paste(Sys.Date(),"_Club_Scores", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(ftable(), file, row.names = FALSE)
        }
    )
    ftable <- reactive(values$table)
    values <- reactiveValues()
    values$table <- data.frame(Club_Name = character(), Category = character(),
                               Budget_Percentage_Used = double(),
                               Missing_Late_Paperwork = double(),
                               Monthly_Meetings_Missed = double(),
                               Missing_Broken_Assets = double(),
                               General_Meetings = double(),
                               Avg_Attendance = double(),
                               EBoard_Meetings = double(),
                               Events = double(),
                               Event_Attendance = double(),
                               Club_Performance = double(),
                               Meeting_Score = double(),
                               Total = double())
    newEntry <- observe({
        if(input$update > 0){
        perf1 <- (((1-(input$budget / (input$budget_threshold)))*(-input$budget_weight)) - (input$mp * (input$mlp_weight)) - (input$missed_meetings/input$mm) - (input$bad_assets * (input$mba_weight)))
        social1 <- (((input$gen_meetings * (input$gm_attendance / input$gmat_weight))*input$gm_weight) + 
                       ((input$events * (input$event_attendance / input$eventat_weight))*input$event_weight) + 
                       (input$eboard_meetings * input$eb_weight))
        total <- ((perf1+social1)/2) + club()
        isolate(values$table[nrow(values$table)+1, ] <- c(input$name, input$cat, input$budget,
                                                          input$mp,
                                                          input$missed_meetings,
                                                          input$bad_assets,
                                                          input$gen_meetings,
                                                          input$gm_attendance,
                                                          input$eboard_meetings,
                                                          input$events,
                                                          input$event_attendance,
                                                          perf1,
                                                          social1,
                                                          total))}
    })
    output$table1 <- renderTable({values$table[c("Club_Name","Club_Performance","Meeting_Score","Total")]})
    
    club <- reactiveVal(0)
    observeEvent(input$cat,{
        dummy <- input$cat
        
        if(dummy == "Academic"){
            club(input$academic_bonus)
        } else if(dummy == "Gaming"){
            club(input$gaming_bonus)
        } else if(dummy == "Religious"){
            club(input$religious_bonus)
        } else if(dummy == "Outdoor"){
            club(input$outdoor_bonus)
        } else if(dummy == "Recreation"){
            club(input$recreation_bonus)
        } else if(dummy == "Professional Organization"){
            club(input$po_bonus)
        }
    })
    
    
    output$perf_result <- renderText({
        perf <- 0
        perf <- (((1-(input$budget / (input$budget_threshold)))*(-input$budget_weight)) - (input$mp * (input$mlp_weight)) - (input$missed_meetings/input$mm) - (input$bad_assets * (input$mba_weight)))
    })
    
    
    
    output$social_result <- renderText({
        social <- 0
        social <- (((input$gen_meetings * (input$gm_attendance / input$gmat_weight))*input$gm_weight) + 
        ((input$events * (input$event_attendance / input$eventat_weight))*input$event_weight) + 
        (input$eboard_meetings * input$eb_weight))

    })
    
    output$total_result <- renderText({
        perf <- (((1-(input$budget / (input$budget_threshold)))*(-input$budget_weight)) - (input$mp * (input$mlp_weight)) - (input$missed_meetings/input$mm) - (input$bad_assets * (input$mba_weight)))
        social <- (((input$gen_meetings * (input$gm_attendance / input$gmat_weight))*input$gm_weight) + 
                       ((input$events * (input$event_attendance / input$eventat_weight))*input$event_weight) + 
                       (input$eboard_meetings * input$eb_weight))
        ((perf + social) / 2) + club()
    })
 
    
}

 


# Run the application 
shinyApp(ui = ui, server = server)
