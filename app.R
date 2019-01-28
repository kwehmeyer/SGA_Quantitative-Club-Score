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
    titlePanel(title=div(img(src='sgalogo.png')), "SGA" ),
    headerPanel("SGA Quantitative Club Scoring System"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h1("Preliminary Information"),
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
            numericInput("event_attendance","Event Attendance",0, min = 0, max = 1000)

         
            
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
            hr()
            
        )
    )
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
