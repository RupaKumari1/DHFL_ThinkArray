# shinyHome
# DHFL - Collections: Prediction of Loan Defaulters using Machine Learning
# Rupa Kumari & Md Mehran Abul
# Date: September 8, 2018 

#server.R

#===============================================================================
#                               SHINYSERVER                                    #
#===============================================================================


# Installing required packages (No need to install if already done)
install.packages("dplyr")
install.packages("plotly")
install.packages("DT")
install.packages("stringr")

#Load packages and modules 
library(plotly)
library(dplyr)
library(DT)
library(stringr)

# server function
function(session,input, output) {
  
  ############################################### Status Prediction Tab ######################
  #Updating text box as per selected loan ID
  observeEvent(input$loan_Id,{
    updateTextInput(session, "predicted", value = "Click on Prediction")
    loan_id <- input$loan_Id
    if(!is.na(loan_id) || !is.null(loan_id)){
      ui_data <- loan_data_pred[loan_data_pred$id==loan_id,]
      updateTextInput(session, "name", value = ui_data$Customer_name)
      updateTextInput(session, "emp_length", value = ui_data$emp_length)
      updateTextInput(session, "annual_inc", value = ui_data$annual_inc)
      updateTextInput(session, "purpose", value = ui_data$purpose)
      updateTextInput(session, "home_ownership", value = ui_data$home_ownership)
      updateTextInput(session, "grade", value = ui_data$grade)
      updateTextInput(session, "loan_amnt", value = ui_data$loan_amnt)
      updateTextInput(session, "int_rate", value = ui_data$int_rate)
      updateTextInput(session, "term", value = ui_data$term)
      updateTextInput(session, "predicted", value = ui_data$status)
    }
  })
  
  #Prediction status value update on selection of Loan grade
  observeEvent(input$Loan_grade,{
    
    input_Loan_grade <- input$Loan_grade
    if(input_Loan_grade != "ALL"){
      loan_data_pred <- loan_data_pred[loan_data_pred$grade==input_Loan_grade,]
    }
    Pred_Loan_Status <- as.character(c("ALL",unique(loan_data_pred$status)))
    updateSelectInput(session, "Pred_status", choices = Pred_Loan_Status)
    
  })
  
  #Table display and download csv file as per selection of Loan grade and Prediction status
  observeEvent(list(input$Loan_grade,input$Pred_status),{
    input_Loan_grade <- input$Loan_grade
    input_Pred_status <- input$Pred_status
    
    if(input_Loan_grade != "ALL"){
      loan_data_pred <- loan_data_pred[loan_data_pred$grade==input_Loan_grade,]
    }
    
    if(input_Pred_status != "ALL"){
      loan_data_pred <- loan_data_pred[loan_data_pred$status==input_Pred_status,]
    }
    
    # Data table creation
    if(!is.null(input_Loan_grade)){
      table_data <- loan_data_pred[c("Customer_name" ,"id" , "loan_amnt" ,"term" ,"emp_length" ,"verification_status","status" )]
      colnames(table_data) <- c("Customer Name" , "Loan ID" , "Loan Amount" ,"LOAN TERM" ,"Employment Year" ,"Verification Status","Status Prediction" )
      
      output$table <- DT::renderDataTable({data = table_data
      datatable(table_data,rownames = FALSE, selection='single',options = list(
        columnDefs = list(list(className = 'dt-left', targets = '_all')),
        pageLength = 5,
        lengthChange = FALSE
      )) %>% formatStyle(
        'Status Prediction',
        target = 'row',
        backgroundColor = styleEqual(c("Defaulter", "Solvent"), c('#F5B7B1','#D5F5E3')))}
      )
      
      # Data table creation on selection of row
      observeEvent(input$table_rows_selected, {
        row_number <- input$table_rows_selected
        row_id <- loan_data_pred[row_number,]$id
        row_data <- loan_data_pred[loan_data_pred$id ==row_id,]
        data_table2 = row_data[c("issue_d" ,"loan_status","purpose","last_pymnt_d","last_pymnt_amnt","State")]
        colnames(data_table2) <- c("Issue Date" ,"Loan Status","Purpose","Last Payment Date","Last Payment Amount","PROPERTY STATE")
        
        showModal(modalDialog(tags$head(
          tags$style(".modal-dialog {width: 90%; height: 60%;}")
        ),
        div(tags$h4(paste0("More Details About Loan")), style = "color: #a10000; font-style: italic;"),
        div(DT::renderDataTable({data = as.data.frame(data_table2)
        datatable(data_table2,rownames = FALSE)}))
        ,easyClose = TRUE,footer = modalButton("Close")))
      })
      #Download csv file
      output$downloadData <- downloadHandler(
        filename = paste0("Loan_Pred.csv"),
        content = function(filename) {
          write.csv(table_data, filename,row.names=FALSE)
        }
      ) 
    }
    
    
  })
  
  ###################################### Loan Status Tab #################################
  #update Loan status on selection of year
  observeEvent(input$year,{
    input_year <- input$year
    if(input$year != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$year == input_year,]
    }
    loan_Default_Status <- c("ALL",sort(as.character(unique(homeloan_data$Default_Status))))
    loan_status <- c("ALL",sort(as.character(unique(homeloan_data$loan_status))))
    
    updateSelectInput(session, "Default_Status", choices = loan_Default_Status)
    updateSelectInput(session, "status", choices = loan_status)
    
  })
  
  
  #update Status in Details on selection of year and Status
  observeEvent(input$Default_Status,{
    input_year <- input$year
    input_Default_Status <- input$Default_Status
    if(input_year != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$year == input_year,]
    }
    
    if(input_Default_Status != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$Default_Status == input_Default_Status,]
    }
    
    loan_status <- c("ALL",sort(as.character(unique(homeloan_data$loan_status))))
    updateSelectInput(session, "status", choices = loan_status)
    
  })
  
  #Plotting graph according to issue year and loan status
  observeEvent(list(input$year,input$Default_Status, input$status),{
    
    input_Default_Status <- input$Default_Status
    
    if(input$year == "ALL"){
      
      yearly_data_Summary <- homeloan_data %>%
        group_by(year) %>%
        summarise(counts = n())
      
      last_pymnt_year_Summary <- homeloan_data %>%
        group_by(last_pymnt_year) %>%
        summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))
      colnames(last_pymnt_year_Summary) <- c("last_year","counts","Net")
      
    } else{
      input_year <- input$year
      homeloan_data2 <- homeloan_data[homeloan_data$year == input_year,]
      homeloan_data1 <- homeloan_data[homeloan_data$last_pymnt_year == input_year,]
      
      yearly_data_Summary <- homeloan_data2 %>%
        group_by(issue_d) %>%
        summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))
      colnames(yearly_data_Summary) <- c("year","counts","Net")
      
      last_pymnt_year_Summary <- homeloan_data1 %>%
        group_by(last_pymnt_d) %>%
        summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))
      colnames(last_pymnt_year_Summary) <- c("last_year","counts","Net")
      
    }
    
    if(input$year != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$year == input_year,]
    }
    
    if(input$Default_Status != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$Default_Status == input_Default_Status,]
    }
    
    data_status <- homeloan_data %>%
      group_by(emp_length,loan_status) %>%
      summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))
    
    data_status <- data_status[!(data_status$emp_length == "n/a"),]
    
    status <- input$status
    if(status != "ALL"){
      data_status <- data_status[data_status$loan_status == status,]
      
    }
    
    #For Line Plot
    output$linePlot <- renderPlotly({
      p <- plot_ly(yearly_data_Summary, x = ~yearly_data_Summary$year, y = ~yearly_data_Summary$counts, 
                   name = 'Issued Loan', 
                   type = 'scatter', mode = 'lines',
                   text = paste0(yearly_data_Summary$year,',',yearly_data_Summary$counts,',','Issued Loan'),
                   hoverinfo = 'text'
      )%>%
        add_trace(data = last_pymnt_year_Summary,x = ~last_pymnt_year_Summary$last_year, y = ~last_pymnt_year_Summary$counts,
                  type = 'scatter', name = "Last Payment",
                  text = paste0(last_pymnt_year_Summary$last_year,',',last_pymnt_year_Summary$counts,',',"Last Payment"),
                  hoverinfo = 'text'
        )%>%
        layout(showlegend = TRUE,legend = list(x = 0.8, y = 1),
               title = "Count of Issued Loans and Last Payments",
               xaxis = list(title = F,showticklabels = TRUE),
               yaxis = list(title = "Count"))
      
    })
    
    
    #For Bar Plot
    if(nrow(data_status) != 0){
      output$barPlot <- renderPlotly({
        xform <- list(title=F,categoryorder = "Quater",categoryarray = emp_len_list[1:11],showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE)
        p <- plot_ly(data =data_status,
                     x = ~emp_length,
                     y = ~Net,
                     type = "bar",
                     name = "Paid",
                     hoverinfo="text",text = paste0(data_status$emp_length,',',data_status$Net,',',data_status$loan_status),
                     color = ~loan_status
        )%>%
          layout(xaxis = xform, showlegend = FALSE,
                 title = "Yearly Loan Amount Across Employee Length",
                 yaxis = list(title = "Yearly Loan Amount")
          )
      })
    }
    
    if(status != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$loan_status == loan_status,]
    }
    #Download csv file
    output$downloadData_Status <- downloadHandler(
      filename = paste0("Loand_Status_Data.csv"),
      content = function(filename) {
        write.csv(homeloan_data, filename,row.names=FALSE)
      }
    ) 
    
  })
  ################################### Loan Details Tab ###################################
  #Updating issue year based on Last Payment year
  observeEvent(input$pymnt_year,{
    input_pymnt_year <- input$pymnt_year
    if(input_pymnt_year == "ALL"){
      issue_year <- c("ALL",sort(as.numeric(unique(homeloan_data$year))))
    }else{
      homeloan_data <- homeloan_data[homeloan_data$last_pymnt_year == input_pymnt_year,]
      issue_year <- c("ALL",sort(as.numeric(unique(homeloan_data$year))))
    }
    updateSelectInput(session, "issue_year", choices = issue_year)
  })
  
  # Plotting Doughnut & Bar graph based on selected year and criteria
  observeEvent(list(input$pymnt_year,input$issue_year,input$criteria),{
    
    select_pymnt_year <- input$pymnt_year
    select_issue_year <- input$issue_year
    select_status_criteria <- input$criteria
    
    if(select_pymnt_year != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$last_pymnt_year == select_pymnt_year,]
    }
    if(select_issue_year != "ALL"){
      homeloan_data <- homeloan_data[homeloan_data$year == select_issue_year,]
    }
    
    selected_data <- homeloan_data[c(select_status_criteria,c("loan_amnt"))]
    colnames(selected_data) <- c("criteria","loan_amnt")
    
    df <- selected_data %>%
      group_by(criteria) %>%
      summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))
    df
    colnames(df) <- c("selected_data","counts","Net")
    
    #For Doughnut Chart
    output$pie_chart <- renderPlotly({
      p <- df %>%
        plot_ly(labels = ~selected_data, values = ~counts,text = ~counts,
                textposition = "inside",
                textinfo = "text",
                hoverinfo= 'label+percent'
                
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Loans Issued Across Categories",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    #For Bar plot
    output$barPlot_status <- renderPlotly({
      p <-df %>%
        plot_ly(x = ~selected_data, y = ~Net, type = 'bar')%>%
        layout(title = "Total Loan Amount Across Categories",
               xaxis = list(title = FALSE),
               yaxis = list(title = "Total Amount"))
      
    })
    
    #Download csv file
    output$downloadData_Details <- downloadHandler(
      filename = paste0("Loan_Details_Data.csv"),
      content = function(filename) {
        write.csv(homeloan_data, filename,row.names=FALSE)
      }
    ) 
  })
  
  
}