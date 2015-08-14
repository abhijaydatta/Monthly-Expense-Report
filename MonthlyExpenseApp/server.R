library(shiny)
require("reshape2")   # for transposing data with 'cast' and 'melt' functions
require("calibrate") # to add data labels on graphs
require("plyr")     # to do ply functions
require("forecast")  # to perform moving avarages analysis
require("zoo")  # to perform moving avarages analysis
require("psych")  # to perform exploratory factor analysis
require("graphics")
require("segmented")
require("openxlsx")
require("stats")
require("ggplot2")
require("DT")

# enable viewing of warning messages
options(warn=1)

# Clear all objects from R memory
rm(list=ls())

#read the csv file
Monthly_Expense <- read.xlsx("data/Monthly Expense Tracking - ORIGINAL.xlsx",sheet=3,startRow=2,colNames=TRUE)
Monthly_Expense <- Monthly_Expense [!is.na(Monthly_Expense$Date),1:9]
#Summation_Categories <- read.csv("Summation Categories.csv",header=FALSE)

#convert month name to month number
Monthly_Expense$Month<-match(Monthly_Expense$Month,month.name)
Monthly_Expense$Full_Date<-as.Date(paste(Monthly_Expense$Year,Monthly_Expense$Month,Monthly_Expense$Date,sep = "." ),format="%Y.%m.%d")
Itemized_Monthly_Expense <- Monthly_Expense[,c('Full_Date','Item','Category','Amount')]
#Daily_Monthly_Expense<-ddply(Monthly_Expense,c("Full_Date"),summarize,Amount=sum(Amount))
Itemized_Monthly_Expense$Category<-trimws(Itemized_Monthly_Expense$Category)
All_Categories<-unique(Itemized_Monthly_Expense$Category)
All_Categories<-as.factor(All_Categories)
levels(All_Categories)<-c("Food",
                          "Rent + Maintenance",
                          "Aparrel",
                          "Hair cuts",
                          "Car Fuel",
                          "Car Maintenance",
                          "Newspaper",
                          "Electricity",
                          "Household items",
                          "Family responsibilities",
                          "Domestic Help",                          
                          "Gas",
                          "Internet",
                          "Phone",
                          "Cable",
                          "Health",
                          "Baby",
                          "Baby2",
                          "House Repair",                          
                          "A beauty expenses",
                          "Miscellaneous",
                          "Books",
                          "Property Tax",
                          "Income Tax",                          
                          "LIC Premium",
                          "Child's education",
                          "Child's education2",
                          "Charity",
                          "Travel to home city",
                          "House Transfer expenses",
                          "Loan",
                          "New car",
                          "Onnoprason",
                          "Pregnancy",
                          "Delivery"
)


Create_Last_Years_Expenses <- function(start_date,end_date,category)
{
  index<-as.numeric(format(start_date,"%Y"))-2009
  date_start<-rep(as.Date("1900-01-01"),index)
  date_end<-rep(as.Date("1900-01-01"),index)
  sum_of_spend<-rep(0,index)
  year_start<-rep(0,index)
  
  for (i in 1:index)
  {
    date_start[i]<-as.Date(paste(as.numeric(format(start_date,"%Y"))-(i-1),"-",as.numeric(format(start_date,"%m")),"-",as.numeric(format(start_date,"%d")),sep=""))
    date_end[i] <-as.Date(paste(as.numeric(format(end_date,"%Y"))-(i-1),"-",as.numeric(format(end_date,"%m")),"-",as.numeric(format(end_date,"%d")),sep=""))
    sum_of_spend[i] <- round(sum(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=date_start[i]&Itemized_Monthly_Expense$Full_Date<=date_end[i]&Itemized_Monthly_Expense$Category%in%category,c('Full_Date','Item','Amount')][,'Amount']),2)
    year_start[i] <- as.numeric(format(date_start[i],"%Y"))
  }
  
  return(data.frame(Year=year_start,Spend=sum_of_spend))
}

Create_Prior_Years_Categorywise_Expenses <- function(start_date,end_date,category)
{
  index<-as.numeric(format(start_date,"%Y"))-2009
  date_start<-rep(as.Date("1900-01-01"),index)
  date_end<-rep(as.Date("1900-01-01"),index)
  year_start<-rep(0,index)
  Category_Wise_Spend<-data.frame(Category=category)
  for (i in index:1)
  {
    date_start[i]<-as.Date(paste(as.numeric(format(start_date,"%Y"))-(i-1),"-",as.numeric(format(start_date,"%m")),"-",as.numeric(format(start_date,"%d")),sep=""))
    date_end[i] <-as.Date(paste(as.numeric(format(end_date,"%Y"))-(i-1),"-",as.numeric(format(end_date,"%m")),"-",as.numeric(format(end_date,"%d")),sep=""))
    year_start[i] <- as.numeric(format(date_start[i],"%Y"))
    Category_Wise_Spend<-merge(Category_Wise_Spend,ddply(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=date_start[i]&Itemized_Monthly_Expense$Full_Date<=date_end[i]&Itemized_Monthly_Expense$Category%in%category,],"Category",summarize,Amount=sum(Amount)),by="Category",all.x=TRUE)
    names(Category_Wise_Spend)[ncol(Category_Wise_Spend)]<-as.character(year_start[i])
  }
  Category_Wise_Spend[is.na(Category_Wise_Spend)]<-0
  Category_Wise_Spend$Diff_Last_Yr<-Category_Wise_Spend[,ncol(Category_Wise_Spend)] - Category_Wise_Spend[,(ncol(Category_Wise_Spend)-1)]
  Category_Wise_Spend<-Category_Wise_Spend[order(-Category_Wise_Spend[,ncol(Category_Wise_Spend)]),]
  rownames(Category_Wise_Spend)<-NULL
  return(Category_Wise_Spend)
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Take an input category and display 
  # the itemized expenses against that category
  
    Past_Expenses_Same_Period <- reactive({
      Create_Last_Years_Expenses(input$daterange[1],input$daterange[2],input$Category)
  })
    Past_Categorywise_Expenses_Same_Period <- reactive({
      datatable(Create_Prior_Years_Categorywise_Expenses(input$daterange[1],input$daterange[2],input$Category),rownames = FALSE,selection='single',filter = "none",caption="Categorywise spend from prior years",options=list(searching = FALSE,paging = FALSE))%>%formatRound(c(2,3,4,5,6,7,8),digits=2)%>%formatStyle(c(1,2,3,4,5,6,7,8),`font-size`='10px')
    })

  
  
  Last_2Years_Start_Date <- reactive({as.Date(paste(as.numeric(format(input$daterange[1],"%Y"))-2,"-",as.numeric(format(input$daterange[1],"%m")),"-",as.numeric(format(input$daterange[1],"%d")),sep=""))})
  Last_2Years_End_Date <- reactive({as.Date(paste(as.numeric(format(input$daterange[2],"%Y"))-2,"-",as.numeric(format(input$daterange[2],"%m")),"-",as.numeric(format(input$daterange[2],"%d")),sep=""))})
  Last_Year_Start_Date <- reactive({as.Date(paste(as.numeric(format(input$daterange[1],"%Y"))-1,"-",as.numeric(format(input$daterange[1],"%m")),"-",as.numeric(format(input$daterange[1],"%d")),sep=""))})
  Last_Year_End_Date <- reactive({as.Date(paste(as.numeric(format(input$daterange[2],"%Y"))-1,"-",as.numeric(format(input$daterange[2],"%m")),"-",as.numeric(format(input$daterange[2],"%d")),sep=""))})
  Total <- reactive({round(sum(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=input$daterange[1]&Itemized_Monthly_Expense$Full_Date<=input$daterange[2]&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][,'Amount']),2)})
  Total_Last_Year <- reactive({round(sum(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_Year_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_Year_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][,'Amount']),2)})
  Total_Last2_Year <- reactive({round(sum(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_2Years_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_2Years_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][,'Amount']),2)}) 
  output$Summary <- renderUI({tags$p("Total Spend on",tags$strong("selected categories"),"is",tags$strong(Total()),tags$br())})
  #output$Summary_Last_Year <- renderUI({tags$p("Same period last year",tags$strong(input$Category),"was",tags$strong(Total_Last_Year()),tags$br())})
  #output$Summary_Last2_Year <- renderUI({tags$p("Same period 2 years back",tags$strong(input$Category),"was",tags$strong(Total_Last2_Year()),tags$br())})

  output$past_Categorywise_Expenses_Same_Period <- DT::renderDataTable(Past_Categorywise_Expenses_Same_Period(),server=FALSE)
  #output$y12 = renderPrint(unlist(Past_Categorywise_Expenses_Same_Period())[input$past_Categorywise_Expenses_Same_Period_rows_selected])
  Details_this_year<-reactive({datatable(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=input$daterange[1]&Itemized_Monthly_Expense$Full_Date<=input$daterange[2]&Itemized_Monthly_Expense$Category%in%unlist(Past_Categorywise_Expenses_Same_Period())[input$past_Categorywise_Expenses_Same_Period_rows_selected],c('Full_Date','Item','Amount')][order(-Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=input$daterange[1]&Itemized_Monthly_Expense$Full_Date<=input$daterange[2]&Itemized_Monthly_Expense$Category%in%unlist(Past_Categorywise_Expenses_Same_Period())[input$past_Categorywise_Expenses_Same_Period_rows_selected],c('Full_Date','Item','Amount')]$Amount),c('Item','Amount')][1:10,],caption="This year", selection='none',options = list(searching = FALSE,paging = FALSE),rownames=FALSE)%>%formatRound(c('Amount'),digits=2)%>%formatStyle(c(1,2),`font-size`='10px')})
  Details_last_year<-reactive({datatable(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_Year_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_Year_End_Date()&Itemized_Monthly_Expense$Category%in%unlist(Past_Categorywise_Expenses_Same_Period())[input$past_Categorywise_Expenses_Same_Period_rows_selected],c('Full_Date','Item','Amount')][order(-Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_Year_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_Year_End_Date()&Itemized_Monthly_Expense$Category%in%unlist(Past_Categorywise_Expenses_Same_Period())[input$past_Categorywise_Expenses_Same_Period_rows_selected],c('Full_Date','Item','Amount')]$Amount),c('Item','Amount')][1:10,],caption="Last Year",selection='none', options = list(searching = FALSE,paging = FALSE),rownames=FALSE)%>%formatRound(c('Amount'),digits=2)%>%formatStyle(c(1,2),`font-size`='10px')})
  
  output$y12 = DT::renderDataTable(Details_this_year(),server=FALSE)
  output$y13 = DT::renderDataTable(Details_last_year(),server=FALSE)
  output$Box<-renderUI(selectizeInput("Category","Enter a Category:",levels(All_Categories),multiple=TRUE,selected=c("Food", "Rent + Maintenance", "Aparrel", "Hair cuts","Car Fuel","Car Maintenance","Newspaper","Electricity","Household items","Family responsibilities","Domestic Help","Gas","Internet","Phone","Cable","Health","Baby","Baby2","House Repair","A beauty expenses","Miscellaneous","Books")))
  output$toptenexpense <- DT::renderDataTable(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=input$daterange[1]&Itemized_Monthly_Expense$Full_Date<=input$daterange[2]&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][order(-Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=input$daterange[1]&Itemized_Monthly_Expense$Full_Date<=input$daterange[2]&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')]$Amount),c('Item','Amount','Full_Date')][1:10,], options = list(searching = FALSE,paging = FALSE),rownames=FALSE)
  output$toptenexpense_last_year <- DT::renderDataTable(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_Year_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_Year_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][order(-Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_Year_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_Year_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')]$Amount),c('Item','Amount','Full_Date')][1:10,], options = list(searching = FALSE,paging = FALSE),rownames=FALSE)
  output$toptenexpense_last_2year <- DT::renderDataTable(Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_2Years_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_2Years_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')][order(-Itemized_Monthly_Expense[Itemized_Monthly_Expense$Full_Date>=Last_2Years_Start_Date()&Itemized_Monthly_Expense$Full_Date<=Last_2Years_End_Date()&Itemized_Monthly_Expense$Category%in%input$Category,c('Full_Date','Item','Amount')]$Amount),c('Item','Amount','Full_Date')][1:10,], options = list(searching = FALSE,paging = FALSE),rownames=FALSE)
  output$expenseplot<-renderPlot({
    pl<- ggplot(Past_Expenses_Same_Period(),aes(x=factor(Year),y=Spend,group=1))+xlab("Year")+ylab("Amount")+geom_point(aes(size=2))+geom_line(color="black",size=1)+geom_text(aes(label=Spend,size=2,vjust=-3))+ggtitle(paste("Trend for same categories in same period prior years"))+theme(plot.title = element_text(lineheight=1.8, face="bold"),legend.position="none",axis.title.x = element_text(face="bold",size=20),axis.title.y = element_text(face="bold",size=20))
    print(pl)
  })
})