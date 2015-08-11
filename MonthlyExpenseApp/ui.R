library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  h2("Monthly Expense App",align="center"),br(),hr(),
  
  # Sidebar with a slider input for the number of bins
  fluidRow(column(3,dateRangeInput("daterange",
                                   "Enter Date Range:",
                                   start = as.Date(ifelse((as.numeric(format(Sys.Date(),"%m"))>=4&as.numeric(format(Sys.Date(),"%m"))<=12),as.Date(paste(as.numeric(format(Sys.Date(),"%Y")),"-",04,"-",01,sep="")),as.Date(paste(as.numeric(format(Sys.Date(),"%Y"))-1,"-",04,"-",01,sep=""))),origin="1970-01-01"),
                                   end   =  Sys.Date(),
                                   min   = "2010-04-01",
                                   max   = Sys.Date()),uiOutput("Box"),uiOutput("Summary")),
                  
           column(9,plotOutput("expenseplot"))
           ,
  hr()),
  fluidRow(column(9,h3("Categorywise spend in prior periods",align="left"),dataTableOutput("past_Categorywise_Expenses_Same_Period"))),
  fluidRow(column(4,h3("Top 10 entries in chosen period",align="left"),dataTableOutput("toptenexpense")),
  column(4,h3("Top 10 entries prior to that",align="left"),dataTableOutput("toptenexpense_last_year")),
  column(4,h3("Top 10 entries prior to that",align="left"),dataTableOutput("toptenexpense_last_2year")))  
))