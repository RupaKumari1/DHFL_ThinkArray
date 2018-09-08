# DHFL - Collections: Prediction of Loan Defaulters using Machine Learning
# Rupa Kumari & Md Mehran Abul
# Date: September 8, 2018 

################################ XGBOOST ##################################

# Installing required packages (No need to install if already done)
install.packages("data.table")
install.packages("Hmisc")
install.packages("mice")
install.packages("lubridate")
install.packages("xgboost")
install.packages("magrittr")
install.packages("Matrix")
install.packages("dplyr")
install.packages("ROCR")

library(data.table) # To read big file
library(Hmisc) # For imputation
library(mice) # For missing value treatment
library(lubridate) # For date formatting
library(xgboost) # For XGboost execution
library(magrittr) # For quick data manipulation
library(Matrix) # For sparse matrix
library(dplyr) # For data manipulation
library(ROCR) # For ROC curve
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

# Executing XGBOOST model

# Creating train dataset
trainm<-sparse.model.matrix(defaulter~.-1,data=train)
train_label<-train[,"defaulter"]
train_matrix<-xgb.DMatrix(data=as.matrix(trainm),label=train_label)

# Creating test dataset
testm<-sparse.model.matrix(defaulter~.-1,data=test)
test_label<-test[,"defaulter"]
test_matrix<-xgb.DMatrix(data=as.matrix(testm),label=test_label)


# Mentioning model parameters
nc<-length(unique(train_label))
xgb.params<-list("objective"="multi:softprob","eval_metric"="mlogloss",
                 "num_class"=nc)
watchlist<-list(train=train_matrix,test=test_matrix)

# Executing xgboost model
bst_model<-xgb.train(params = xgb.params,data=train_matrix,nrounds = 100,
                     watchlist = watchlist,eta=0.1)

# Storing the errors
e<-data.frame(bst_model$evaluation_log)

# Plotting the train and test errors
plot(e$iter,e$train_mlogloss,col="blue")
lines(e$iter,e$test_mlogloss,col="red")

# Feature Importance
imp<-xgb.importance(colnames(train_matrix),model = bst_model)
xgb.plot.importance(imp)
head(imp,10)

# Predicting outcome
p<-predict(bst_model,newdata = test_matrix)
head(p)

# Confusion Matrix
pred<-matrix(p,nrow=nc,ncol = length(p)/nc) %>%
  t()%>%
  data.frame()%>%
  mutate(label=test_label,max_prob=max.col(.,"last")-1)
conf_mat<-table(Actual=pred$label,prediction=pred$max_prob)
conf_mat

# Classification Rate
(sum(diag(conf_mat))/sum(conf_mat))*100


# Get the trained model
model <- xgb.dump(bst_model, with_stats=TRUE)

# Use ROCR package to plot ROC Curve

p1<-predict(bst_model,newdata = train_matrix)
trainpred<-matrix(p1,nrow=nc,ncol = length(p1)/nc) %>%
  t()%>%
  data.frame()%>%
  mutate(label=train_label,max_prob=max.col(.,"last")-1)

xgb.pred <- prediction(trainpred$X2,train$defaulter)
xgb.perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb.perf,
     avg="threshold",
     lwd=1,
     col="red",
     main="ROC Curve",
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
lines(x=c(0, 1), y=c(0, 1), col="black")
roc(response=train$defaulter,predictor = trainpred$X2,plot=TRUE,auc=TRUE)

###################### End of the R script ########################################
