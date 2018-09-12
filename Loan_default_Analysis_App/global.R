# shinyHome
# DHFL - Collections: Prediction of Loan Defaulters using Machine Learning
# Rupa Kumari & Md Mehran Abul
# Date: September 8, 2018 

#global.R

# Installing required packages (No need to install if already done)
install.packages("dplyr")
install.packages("data.table")
install.packages("lubridate")
install.packages("stringr")
install.packages("xgboost")
install.packages("magrittr")
install.packages("Matrix")

options(warn=-1)

#Load packages and modules 
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(xgboost)
library(magrittr)
library(Matrix)

#Loading data
homeloan_data <- readRDS("data/home_loan_data.Rds")

#For indian states
State_Summary <- homeloan_data %>%
  group_by(addr_state) %>%
  summarise(counts = n())

State_Summary<-State_Summary[order(State_Summary$counts,decreasing = TRUE),]
Ind_State<-c('UP','MH','BR','WB','MP','TN','RJ','KA','GJ','AP','OR','TG','KL','JH','AS','PB','CT'
             ,'HR','JK','UT','HP','TR','ML','NL','NL','NL','GA','GA','AR','AR','MZ','MZ','SK','SK'
             ,'DL','DL','PY','PY','CH','CH','AN','AN','DN','DN','DD','DD','DD','LD','LD','LD')

State_Summary$State<-Ind_State
State_Summary$counts<-NULL

# Adding Indian States in the main data
homeloan_data<-merge(homeloan_data,State_Summary)
homeloan_data$addr_state<-NULL
names(homeloan_data)[52]<-c("addr_state")


#Removing underscore and making first letter in upper case
homeloan_data$purpose <- sub("_"," ",as.character(homeloan_data$purpose))
homeloan_data$purpose <- paste(toupper(substr(homeloan_data$purpose, 1, 1)), substr(homeloan_data$purpose, 2, nchar(homeloan_data$purpose)), sep="")

#assigning NA to blank cells.
homeloan_data[homeloan_data==""] <- NA
# Defaulter - Charged off, Default, Late (31-120 days), In Grace Period, Late (16-30 days),Does not meet the credit policy. Status:Charged Off
# Not in training - Current, Issued
# Fully Paid - Fully Paid,Does not meet the credit policy. Status:Fully Paid
remove<-c("Current","Issued")
paid<-c("Fully Paid","Does not meet the credit policy. Status:Fully Paid")
default<-c("Charged off","Late (31-120 days)","In Grace Period","Late (16-30 days)","Does not meet the credit policy. Status:Charged Off")


# Creating additional column for Loan Status
homeloan_data$Default_Status<-ifelse(homeloan_data$loan_status %in% remove, "In Progress",
                                     ifelse(homeloan_data$loan_status %in% paid,"Solvent","Defaulter"))

#For notification information in ui.R
info_Summary <- homeloan_data %>%
  group_by(Default_Status) %>%
  summarise(counts = n(),Net = sum(as.numeric(loan_amnt)))

per_bad_loan <- info_Summary$counts[info_Summary$Default_Status == "Defaulter"] /nrow(homeloan_data)*100

#Create year to show data on yearly basis
homeloan_data$year <- gsub('.*-([0-9]+).*','\\1',homeloan_data$issue_d)
homeloan_data$last_pymnt_year <- gsub('.*-([0-9]+).*','\\1',homeloan_data$last_pymnt_d)

#Years used for ui.R variables
issue_year <- c("ALL",sort(as.numeric(unique(homeloan_data$year))))
pymnt_year <- c("ALL",sort(as.numeric(unique(homeloan_data$last_pymnt_year))))
year <- intersect(issue_year,pymnt_year)

# Converting the date columns into date format
homeloan_data$issue_d<-paste("1",homeloan_data$issue_d,sep="-")
homeloan_data$issue_d<-dmy(homeloan_data$issue_d)
homeloan_data$last_pymnt_d <- paste("1",homeloan_data$last_pymnt_d,sep="-")
homeloan_data$last_pymnt_d <- dmy(homeloan_data$last_pymnt_d)

# Default Status, Loan status, employment length and criteria used for visualization purpose 
loan_Default_Status <- c("ALL",sort(as.character(unique(homeloan_data$Default_Status))))
loan_status <- c("ALL",sort(as.character(unique(homeloan_data$loan_status))))
emp_len_list <- c( "< 1 year","1 year", "2 years","3 years", "4 years","5 years","6 years","7 years", "8 years","9 years" ,"10+ years","n/a")
criteria <- list("Loan Status" = "Default_Status","Loan Grade" = "grade", "Loan Term" = "term","Loan Sub-Grade" = "sub_grade","Home Ownership" = "home_ownership","Verification Status" = "verification_status","Loan Purpose" = "purpose","Properly State" = "State")

# Checking for percentage of values available
valid_tbl_cnt <- as.data.frame(sapply(homeloan_data, function(x) sum(!is.na(x))/nrow(homeloan_data)))

# Renaming column name
names(valid_tbl_cnt)[1]<-"%age Available"

# Recording column names with less than 85% data availability
low_val<-ifelse(valid_tbl_cnt$`%age Available`<0.85,row.names(valid_tbl_cnt),NA)
low_val<-low_val[!is.na(low_val)]

# Removing columns with low data availability
homeloan_data<-data.frame(homeloan_data)
homeloan_data<-homeloan_data[, ! names(homeloan_data) %in% low_val, drop = F]

# Dropping irrelevant columns
homeloan_data$emp_title<-NULL
homeloan_data$url<-NULL

# Converting to INR - Assuming 71 as the exchange rate
homeloan_data$funded_amnt<-homeloan_data$funded_amnt*71
homeloan_data$funded_amnt_inv<-homeloan_data$funded_amnt_inv*71
homeloan_data$loan_amnt<-homeloan_data$loan_amnt*71
homeloan_data$annual_inc<-homeloan_data$annual_inc*71
homeloan_data$revol_bal<-homeloan_data$revol_bal*71
homeloan_data$out_prncp<-homeloan_data$out_prncp*71
homeloan_data$total_pymnt<-homeloan_data$total_pymnt*71
homeloan_data$total_rec_int<-homeloan_data$total_rec_int*71
homeloan_data$total_rec_late_fee<-homeloan_data$total_rec_late_fee*71
homeloan_data$recoveries<-homeloan_data$recoveries*71
homeloan_data$collection_recovery_fee<-homeloan_data$collection_recovery_fee*71
homeloan_data$last_pymnt_amnt<-homeloan_data$last_pymnt_amnt*71
homeloan_data$tot_coll_amt<-homeloan_data$tot_coll_amt*71
homeloan_data$tot_cur_bal<-homeloan_data$tot_cur_bal*71

# Creating a copy for Machine Learning
loan_data<-homeloan_data

# Column has string of text
loan_data$title<-NULL

# Grade and Sub grade columns are interrelated
loan_data$sub_grade<-NULL

# Converting categories into factor
loan_data$application_type<-as.factor(loan_data$application_type)
loan_data$initial_list_status<-as.factor(loan_data$initial_list_status)

# Removing V1 column
loan_data$V1<-NULL

# Removing all rows with NA values
loan_data<-loan_data[complete.cases(loan_data),]

# Removing "months" from the term column
loan_data$term<-as.numeric(sub(" .*", "", loan_data$term))

# Converting y/n to 1/0
loan_data$pymnt_plan<-ifelse(loan_data$pymnt_plan=="y",1,0)
loan_data$initial_list_status<-ifelse(loan_data$initial_list_status=="y",1,0)

# The column policy code has only one value
loan_data$policy_code<-NULL

# Redesigning the emp_length column
loan_data$emp_length<-as.character(loan_data$emp_length)
loan_data$emp_length<-ifelse(loan_data$emp_length=="n/a",NA,loan_data$emp_length)
loan_data$emp_length<-ifelse(loan_data$emp_length=="< 1 year","0",loan_data$emp_length)
loan_data$emp_length<-ifelse(loan_data$emp_length=="10+ years","10",loan_data$emp_length)
loan_data$emp_length<-ifelse(is.na(loan_data$emp_length)==TRUE,loan_data$emp_length,
                             as.numeric(sub(" .*", "", loan_data$emp_length)))
loan_data$emp_length<-as.numeric(loan_data$emp_length)

# One hot encoding 
loan_data$ind_application<-ifelse(loan_data$application_type=="INDIVIDUAL",1,0)
loan_data$application_type<-NULL

# High number of levels(50 & 860 respectively) i.e. high cardinality
loan_data$addr_state<-NULL
loan_data$zip_code<-NULL

# Setting 1st of the month as the default date for ease of calculation
loan_data$earliest_cr_line<-paste("1",loan_data$earliest_cr_line,sep = "-")
loan_data$last_credit_pull_d<-paste("1",loan_data$last_credit_pull_d,sep="-")

# Converting the date columns into date format
loan_data$earliest_cr_line<-dmy(loan_data$earliest_cr_line)
loan_data$last_credit_pull_d<-dmy(loan_data$last_credit_pull_d)

# Calculating durations
loan_data$earliest_cr_days<-as.numeric(loan_data$issue_d-loan_data$earliest_cr_line)
loan_data$last_pymt_days<-as.numeric(today()-loan_data$last_pymnt_d)
loan_data$last_credit_pull_days<-as.numeric(today()-loan_data$last_credit_pull_d)

# Dropping the date columns
loan_data$issue_d<-NULL
loan_data$earliest_cr_line<-NULL
loan_data$last_pymnt_d<-NULL
loan_data$last_credit_pull_d<-NULL

# Removed Current & Issued since they are still in progress
loan_data<-loan_data[!(loan_data$loan_status %in% remove),]

# Converted the loan_status to 1/0
loan_data$defaulter<-ifelse(loan_data$loan_status %in% paid, 0,1)
loan_data$loan_status<-NULL

# Removing rows having NAs
loan_data<-loan_data[complete.cases(loan_data),]

# sub_grade or might as well take grade in case place of grade
id_table <- as.data.frame(cbind(loan_data$id,loan_data$member_id))
colnames(id_table) <- c("ID","Member_ID")

# Dropping columns with only one value
loan_data$pymnt_plan<-NULL
loan_data$initial_list_status<-NULL
loan_data$ind_application<-NULL

# Dropping irrelevant columns
loan_data$id<-NULL
loan_data$member_id<-NULL
loan_data$year <-NULL
loan_data$last_pymnt_year<-NULL

# Dropping variable with high cardinality
loan_data$State<-NULL

# Dropping since similar column 'defaulter' is already present
loan_data$Default_Status<-NULL

# Coverting the categorical variables into factors
loan_data$grade<-as.factor(loan_data$grade)
loan_data$home_ownership<-as.factor(loan_data$home_ownership)
loan_data$verification_status<-as.factor(loan_data$verification_status)
loan_data$purpose<-as.factor(loan_data$purpose)

# Generating a correlation matrix
cor_mat<-round(cor(loan_data[,c(1:6,8,10,13:38)]),2)

# Removing highly correlated variables 
loan_data$total_rev_hi_lim<-NULL
loan_data$funded_amnt_inv<-NULL
loan_data$funded_amnt<-NULL

# Rearranging the column sequence
loan_data<-loan_data[,c(36,1:35)]

# Splitting the dataset into train and test 
set.seed(123)

#Splitting in 80:20 ratio
ind<-sort(sample(nrow(loan_data),nrow(loan_data)*0.8))
train<-loan_data[ind,]
test<-loan_data[-ind,]

############################ XGBOOST ########################################

trainm<-sparse.model.matrix(defaulter~.-1,data=train)

train_label<-train[,"defaulter"]
train_matrix<-xgb.DMatrix(data=as.matrix(trainm),label=train_label)

# parameters

nc<-length(unique(train_label))
xgb.params<-list("objective"="multi:softprob","eval_metric"="mlogloss",
                 "num_class"=nc)

# Executing the xgboost algorithm
bst_model<-xgb.train(params = xgb.params,data=train_matrix,nrounds = 100,
                     eta=0.1)

# Prediction
testm<-sparse.model.matrix(defaulter~.-1,data=loan_data)
test_label<-loan_data[,"defaulter"]
test_matrix<-xgb.DMatrix(data=as.matrix(testm),label=test_label)

p<-predict(bst_model,newdata = test_matrix)

pred<-matrix(p,nrow=nc,ncol = length(p)/nc) %>%
  t()%>%
  data.frame()%>%
  mutate(label=test_label,max_prob=max.col(.,"last")-1)

# Adding the prediction results to the dataframe along with the customer IDs
loan_data_new <- cbind(id_table,loan_data,pred$max_prob)
loan_data_pred <- homeloan_data[homeloan_data$id %in% loan_data_new$ID,]

# Converting the predicted status into Defaulter/Solvent
loan_data_pred$status <- ifelse(loan_data_new$`pred$max_prob`== 0,"Solvent","Defaulter")
loan_data_pred$V1 <- NULL

# Adding customer name to make the interface more user friendly
customer_name <- read.csv("data/customer_names.csv")
loan_data_pred$Customer_name <-as.character(customer_name$Full_Name)

# Rearranging column names
loan_data_pred<-loan_data_pred[,c(55,1:54)]

# Variables for ui.R
Pred_Loan_grade <- as.character(c("ALL",sort(unique(loan_data_pred$grade))))
Pred_Loan_Status <- as.character(c("ALL",unique(loan_data_pred$status)))
