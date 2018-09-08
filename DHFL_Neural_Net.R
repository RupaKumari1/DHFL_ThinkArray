# DHFL - Collections: Prediction of Loan Defaulters using Machine Learning
# Rupa Kumari & Md Mehran Abul
# Date: September 8, 2018 

############################### NEURAL NETS ##################################

# Installing required packages (No need to install if already done)
install.packages("data.table")
install.packages("Hmisc")
install.packages("mice")
install.packages("lubridate")
install.packages("Matrix")
install.packages("neuralnet")
install.packages("ROCR")
install.packages("pROC")

library(data.table) # To read big file
library(Hmisc) # For imputation
library(mice) # For missing value treatment
library(lubridate) # For date formatting
library(Matrix) # For sparse matrix
library(neuralnet) # For neural network
library(pROC) # For ROC curve

# Reading the file
# Note: Set the directory where your files are save
setwd("C:\\Users\\Mehran\\Desktop\\Personal\\DHFL\\DHFL round 2")

HL_data<-fread("loan.csv")


# Selecting only the home loan related data
loan_data <-HL_data[HL_data$purpose %in% c("house","home_improvement")]
rm(HL_data)

# Converting empty spaces to NA
loan_data[loan_data==""] <- NA

# Checking for percentage of values available
valid_tbl_cnt <- as.data.frame(sapply(loan_data, function(x) sum(!is.na(x))/nrow(loan_data)))

# Renaming column name
names(valid_tbl_cnt)[1]<-"%age Available"

# Recording column names with less than 85% data availability
low_val<-ifelse(valid_tbl_cnt$`%age Available`<0.85,row.names(valid_tbl_cnt),NA)
low_val<-low_val[!is.na(low_val)]

# Removing columns with low data availability
loan_data<-data.frame(loan_data)
loan_data<-loan_data[, ! names(loan_data) %in% low_val, drop = F]

# Dropping irrelevant columns
loan_data$emp_title<-NULL
loan_data$url<-NULL

# Column has string of text
loan_data$title<-NULL

# Grade and Sub grade columns are interrelated
loan_data$sub_grade<-NULL

# Removing all rows with NA values
loan_data<-loan_data[complete.cases(loan_data),]

# Converting categories into factor
loan_data$application_type<-as.factor(loan_data$application_type)
loan_data$initial_list_status<-as.factor(loan_data$initial_list_status)

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
loan_data$issue_d<-paste("1",loan_data$issue_d,sep="-")
loan_data$earliest_cr_line<-paste("1",loan_data$earliest_cr_line,sep = "-")
loan_data$last_pymnt_d<-paste("1",loan_data$last_pymnt_d,sep = "-")
loan_data$last_credit_pull_d<-paste("1",loan_data$last_credit_pull_d,sep="-")

# Converting the date columns into date format
loan_data$issue_d<-dmy(loan_data$issue_d)
loan_data$earliest_cr_line<-dmy(loan_data$earliest_cr_line)
loan_data$last_pymnt_d<-dmy(loan_data$last_pymnt_d)
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

# Defaulter - Charged off, Default, Late (31-120 days), In Grace Period, Late (16-30 days),Does not meet the credit policy. Status:Charged Off
# Not in training - Current, Issued
# Fully Paid - Fully Paid,Does not meet the credit policy. Status:Fully Paid

remove<-c("Current","Issued")
paid<-c("Fully Paid","Does not meet the credit policy. Status:Fully Paid")
default<-c("Charged off","Late (31-120 days)","In Grace Period","Late (16-30 days)","Does not meet the credit policy. Status:Charged Off")

# Removed Current & Issued since they are still in progress
loan_data<-loan_data[!(loan_data$loan_status %in% remove),]

# Converted the loan_status to 1/0
loan_data$defaulter<-ifelse(loan_data$loan_status %in% paid, 0,1)
loan_data$loan_status<-NULL

# Removing rows having NAs
loan_data<-loan_data[complete.cases(loan_data),]

# Removing columns having only one value
loan_data$pymnt_plan<-NULL
loan_data$initial_list_status<-NULL
loan_data$ind_application<-NULL
loan_data$id<-NULL
loan_data$member_id<-NULL

# Generating the correlation matrix
cor_mat<-round(cor(loan_data[,c(1:6,8,10,13:38)]),2)

# Removing highly correlated variables
loan_data$total_rev_hi_lim<-NULL
loan_data$funded_amnt_inv<-NULL
loan_data$funded_amnt<-NULL
loan_data$out_prncp_inv<-NULL
loan_data$total_pymnt_inv<-NULL
loan_data$total_rec_prncp<-NULL
loan_data$installment<-NULL

# Converting to INR
loan_data$loan_amnt<-loan_data$loan_amnt*71
loan_data$annual_inc<-loan_data$annual_inc*71
loan_data$revol_bal<-loan_data$revol_bal*71
loan_data$out_prncp<-loan_data$out_prncp*71
loan_data$total_pymnt<-loan_data$total_pymnt*71
loan_data$total_rec_int<-loan_data$total_rec_int*71
loan_data$total_rec_late_fee<-loan_data$total_rec_late_fee*71
loan_data$recoveries<-loan_data$recoveries*71
loan_data$collection_recovery_fee<-loan_data$collection_recovery_fee*71
loan_data$last_pymnt_amnt<-loan_data$last_pymnt_amnt*71
loan_data$tot_coll_amt<-loan_data$tot_coll_amt*71
loan_data$tot_cur_bal<-loan_data$tot_cur_bal*71

# Converting categorical variables to factors
loan_data$grade<-as.factor(loan_data$grade)
loan_data$home_ownership<-as.factor(loan_data$home_ownership)
loan_data$verification_status<-as.factor(loan_data$verification_status)
loan_data$purpose<-as.factor(loan_data$purpose)

# Rearranging columns
loan_data<-loan_data[,c(ncol(loan_data),1:(ncol(loan_data)-1))]

# Splitting the dataset into train and test dataset
set.seed(123)
ind<-sort(sample(nrow(loan_data),nrow(loan_data)*0.8))
train<-loan_data[ind,]
test<-loan_data[-ind,]

# To one hot encode all the factor variables
trainm<-sparse.model.matrix(defaulter~.-1,data=train)
testm<-sparse.model.matrix(defaulter~.-1,data=test)

# Storing labels separately
train_label<-train[,"defaulter"]
test_label<-test[,"defaulter"]

################################ NEURAL NETWORK ##################################

# Converting the matrices into data frames
train_df<-data.frame(as.matrix(trainm))

test_df<-data.frame(as.matrix(testm))

# Recombining the labels with the datasets
train_df<-cbind(train_df,train_label)
names(train_df)[42]<-"Defaulter"

test_df<-cbind(test_df,test_label)
names(test_df)[42]<-"Defaulter"

# Creating the formula for neural nets
n <- names(train_df)
f <- as.formula(paste("Defaulter ~", paste(n[!n %in% "Defaulter"], collapse = " + ")))

# Executing Neural Network
set.seed(321)
nn<-neuralnet(f,data=train_df,hidden = 3,err.fct = "ce",linear.output = FALSE)
plot(nn,cex=0.5)

# Running the prediction
output<-neuralnet::compute(nn,test_df[,-ncol(test_df)])
p1<-output$net.result

pred1<-ifelse(p1>0.5,1,0)

# Checking the confusion matrix
tab1<-table(test_df$Defaulter,pred1)

# Classification Rate
sum(diag(tab1))/sum(tab1)*100

# ROC
trainpred<-data.frame(nn$net.result)
names(trainpred)[1]<-"Response"
trainROC<-roc(response=train$defaulter,predictor = trainpred$Response,plot=TRUE,auc=TRUE)
plot(trainROC,main="ROC Curve", col=rainbow(6))
trainROC$auc

###################### End of the R script ########################################

