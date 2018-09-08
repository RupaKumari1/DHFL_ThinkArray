# shinyHome
# DHFL - Collections: Prediction of Loan Defaulters using Machine Learning
# Rupa Kumari & Md Mehran Abul
# Date: September 8, 2018 

#ui.R

#===============================================================================
#                               SHINY USER INTERFACE                           #
#===============================================================================


# Installing required packages (No need to install if already done)
install.packages("shinydashboard")
install.packages("plotly")
install.packages("DT")

#Load packages and modules
library(shinydashboard)
library(plotly)
library(DT)

# Initiating UI dashboard
dashboardPage(
  dashboardHeader(title = HTML("Defaulter Prediction"), titleWidth = NULL,
                  tags$li(class = "dropdown",style = "margin-top: 5px;",tags$img(height = "40px",src="brand_logo.png")
                  ),
                  
                  # Dropdown menu for notifications
                  dropdownMenu(type = "notifications", badgeStatus = "warning",icon = icon("bell"),
                               notificationItem(icon = icon("users"), status = "info",
                                                paste0(format(round(nrow(homeloan_data) / 1e3, 1), trim = TRUE), "K loans till today")
                               ),
                               
                               notificationItem(icon = icon("money"),status = "success",
                                                paste0("Rs. ",format(round(sum(homeloan_data$loan_amnt) / 1e6, 1), trim = TRUE), "M disbursed till today")
                                                
                               ),
                               notificationItem(icon = icon("warning"), status = "danger",
                                                paste0("Bad loans: ",format(round(per_bad_loan, 2), trim = TRUE),"%; NPA: Rs. " ,format(round(info_Summary$Net[info_Summary$Default_Status == "Defaulter"] / 1e6, 1), trim = TRUE),"M")
                                                
                               )
                               
                  )      
                  
  ),
  
  
  dashboardSidebar(
    # Menu Items
    sidebarMenu(
      menuItem("Status Prediction", tabName = "Prediction",icon = icon("line-chart")),
      menuItem("Loan Status", tabName = "Status",icon = icon("check")),
      menuItem("Loan Details", tabName = "Details",icon = icon("bar-chart")),
      menuItem("Important Links", tabName = "Website",icon = icon("link"),
               menuItem(tags$a(href = 'https://www.dhfl.com/' ,target="_blank",'DHFL Homepage')),
               menuItem(tags$a(href = 'https://www.dhfl.com/home-loans/' ,target="_blank",'Home Loan')),
               menuItem(tags$a(href = 'https://www.dhfl.com/about-us/' ,target="_blank",'About Us'))
      )
    )
  ),
  # Dashboard Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "Prediction",
              tags$head(
                tags$style(HTML("hr {border-top: 2px solid #000000;}"))),
              tags$p(tags$b(style='font-size: 25px;color:#a10000',"Loan Status Prediction")),
              column(selectInput('loan_Id',"Select Loan ID",choices=loan_data_pred$id,selected=loan_data_pred$id[1]),width = 2),
              column(textInput("name", "Customer Name"),width = 2),
              column(textInput("emp_length", "Employment Year"),width = 2),
              column(textInput("annual_inc", "Annual Income"),width = 2),
              column(textInput("purpose", "Loan Purpose"),width = 2),
              column(textInput("home_ownership", "Home Ownership"),width = 2),
              column(textInput("grade", "Grade"),width = 2),
              column(textInput("loan_amnt", "Loan Amount"),width = 2),
              column(textInput("int_rate", "Interest Rate"),width = 2),
              column(textInput("term", "Loan Term"),width = 2),
              column(textInput("predicted", "Status Prediction"),width = 2),
              tags$p(style='font-size: 20px;color:#a10000;margin-left:10px',"Consolidated View"),
              column(
                column(selectInput('Loan_grade',"Select Loan Grade",choices=Pred_Loan_grade,selected=Pred_Loan_grade[1]),width = 6),
                column(selectInput('Pred_status',"Select Predicted Status",choices=Pred_Loan_Status,selected=Pred_Loan_Status[1]),width = 6),
                width = 10),
              column(tags$br(),downloadButton("downloadData", "Download"), width = 2),
              
              column(dataTableOutput('table'),width = 12)
              
      ),
      tabItem(tabName = "Status",
              column(tags$p(tags$b(style='font-size: 25px;color:#a10000',"Loan Status Information")),width = 10),
              column(downloadButton("downloadData_Status", "Download"), width = 2),
              box(selectInput('year',"Select Loan Issue Year",choices=year,selected=year[1]),width = 4),
              box(selectInput('Default_Status',"Select Loan Status",choices=loan_Default_Status),width = 4),
              box(selectInput('status',"Select Status in Details",choices=loan_status),width = 4),
              box(plotlyOutput("linePlot"),width = 5),
              box(plotlyOutput("barPlot", height = 400),width = 7)
      ),
      
      tabItem(tabName = "Details",
              column(tags$p( tags$b(style='font-size: 25px;color:#a10000',"Comprehensive Loan Details")),width = 10),
              column(downloadButton("downloadData_Details", "Download"), width = 2),
              box(selectInput('pymnt_year',"Select Last Payment Year",choices=pymnt_year,selected=pymnt_year[1]),width = 4),
              box(selectInput('issue_year',"Select Issue Year",choices=issue_year),width = 4),
              box(selectInput('criteria',"Select Criteria",choices=criteria,selected=criteria[1]),width = 4),
              box(plotlyOutput("pie_chart", height = 400),width = 4),
              box(plotlyOutput("barPlot_status", height = 400),width = 8)
      )
    )
  )
)