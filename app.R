

library(shiny)
library(tidyverse)

# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
    

    # Application title
    titlePanel(title=div(img(src='sgalogo.png')), "SGA" ),
    headerPanel("SGA Quantitative Club Scoring System"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
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
    )
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
    values$table <- data.frame(Club_Name = NA, Category = NA,
                               Budget_Percentage_Used = NA,
                               Missing_Late_Paperwork = NA,
                               Monthly_Meetings_Missed = NA,
                               Missing_Broken_Assets = NA,
                               General_Meetings = NA,
                               Avg_Attendance = NA,
                               EBoard_Meetings = NA,
                               Events = NA,
                               Event_Attendance = NA,
                               Club_Performance = NA,
                               Meeting_Score = NA,
                               Total = NA)
    newEntry <- observe({
        if(input$update > 0){
        perf1 <- ((input$budget / 100) - (input$mp * 0.15) - (input$missed_meetings/input$mm) - (input$bad_assets * 0.5))
        social1 <- ((input$gen_meetings * (input$gm_attendance / 100)) + 
                       (input$events * (input$event_attendance / 50)) + 
                       (input$eboard_meetings * 0.1))
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
            club(3)
        } else if(dummy == "Gaming"){
            club(1)
        } else if(dummy == "Religious"){
            club(2)
        } else if(dummy == "Outdoor"){
            club(2)
        } else if(dummy == "Recreation"){
            club(1)
        } else if(dummy == "Professional Organization"){
            club(3)
        }
    })
    
    
    output$perf_result <- renderText({
        perf <- 0
        perf <- ((input$budget / 100) - (input$mp * 0.15) - (input$missed_meetings/input$mm) - (input$bad_assets * 0.5))
    })
    
    
    
    output$social_result <- renderText({
        social <- 0
        social <- ((input$gen_meetings * (input$gm_attendance / 100)) + 
        (input$events * (input$event_attendance / 50)) + 
        (input$eboard_meetings * 0.1))

    })
    
    output$total_result <- renderText({
        perf <- ((input$budget / 100) - (input$mp * 0.15) - (input$missed_meetings * 0.15) - (input$bad_assets * 0.5))
        social <- ((input$gen_meetings * (input$gm_attendance / 100)) + 
                       (input$events * (input$event_attendance / 50)) + 
                       (input$eboard_meetings * 0.1))
        ((perf + social) / 2) + club()
    })
 
    
}

 


# Run the application 
shinyApp(ui = ui, server = server)
