train<-read.csv("train_u6lujuX_CVtuZ9i.csv",na.strings = c(""," ","?","NA",NA))
test<-read.csv("test_Y3wMUE5_7gLdaTN.csv",na.strings = c(""," ","?","NA",NA))
#### reading datasets in R
colSums(is.na(train))
##### checking for missing values in all columns
View(train);View(test)
dim(train);dim(test)
str(train)
####exploring data
length(unique(train$Loan_ID))
library(caret)###machine learning wrapper
library(ggplot2)### data visualisation
library(data.table)### faster data manipulation
library(dplyr)#### data manipulation
library(xgboost)
library(mice)### missing values imputing package
complete<-bind_rows(train,test)
str(complete)
complete<-complete[,-1]

complete$Property_Area<-as.factor(complete$Property_Area)
#####functions for removing highly correlated features
remHighcor <- function(data, cutoff, ...){
  data_cor <- cor(sapply(data, as.numeric), ...)
  data_cor[is.na(data_cor)] <- 0
  rem <- findCorrelation(data_cor, cutoff=cutoff, verbose=T)
  return(rem)
}
remHighcor(complete,0.7)
####functions to data visualize
          
###function to graph variable for continous variables        
tr <- function(a){
      ggplot(data = traindata, aes(x= a, y=..density..)) + 
      geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
      geom_density()            
      }            
tr(complete$ApplicantIncome)            
####function to graph variable against output for categorical variable

all_bar <- function(i){
  ggplot(train,aes(x=i,fill=output$status_group))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}
all_bar(complete$Married)

###dummy variable(one hot encoding) and data manipulation
dmy<-dummyVars("~.",data=complete[,1:11])
trnsf<-data.frame(predict(dmy,newdata=complete[,1:11]))
str(trnsf)
View(trnsf)
trnsf<-nearZeroVar(trnsf)
trnsf[sapply(trnsf, function(x) length(levels(factor(x,exclude=NULL)))>1)]
trnsf<-subset(trnsf,select=-Married.)        
trnsf<-cbind(trnsf,Loan_Status=complete$Loan_Status)
trnsf$Loan_Status<-as.numeric(trnsf$Loan_Status)-1
x_test<-x_test[,-22]
x_train<-trnsf[1:614,]
x_test<-trnsf[615:981,]
x_target<-x_train$Loan_Status
str(x_test)
x_train<-x_train%>%
  mutate(total_inc=CoapplicantIncome+ApplicantIncome)
x_test<-x_test%>%
  mutate(total_inc=CoapplicantIncome+ApplicantIncome)
length(unique(x_train$Loan_Amount_Term))
###feature engineering EMI
x_train<-x_train%>%
  mutate(emi=LoanAmount*0.00833*(1+0.00833)^Loan_Amount_Term/((1+0.00833)^Loan_Amount_Term-1))
x_test<-x_test%>%
  mutate(emi=LoanAmount*0.00833*(1+0.00833)^Loan_Amount_Term/((1+0.00833)^Loan_Amount_Term-1))
x_train<-select(x_train,-Loan_Status,everything())
x_train<-subset(x_train,select = -Dependents.0)
x_test<-subset(x_test,select =-Dependents.0)
#### log and sqrt transformation for skewed data
x_train$ApplicantIncome<-log(x_train$ApplicantIncome)
x_test$ApplicantIncome<-log(x_test$ApplicantIncome)
x_train$LoanAmount<-sqrt(x_train$LoanAmount)
x_test$LoanAmount<-sqrt(x_test$LoanAmount)
####feature engineering 
  x_train<-x_train%>%
  mutate(ratio1=LoanAmount/emi)
x_test<-x_test%>%
  mutate(ratio1=LoanAmount/emi)

xgtrain<-xgb.DMatrix(data=as.matrix(x_train[,1:14]),label=x_target,missing = NA)
xgtest<-xgb.DMatrix(data=as.matrix(x_test[,1:14]),missing = NA)        
          

## xgboost
params <- list()
params$objective <- "binary:logistic"
params$eta <- 0.1
params$max_depth <- 5
params$subsample <- 0.8
params$colsample_bytree <- 0.9
params$min_child_weight <- 2
params$eval_metric <- "auc"


model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 100, nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed(1234))

model_xgb <- xgb.train(params = params, xgtrain, nrounds = 53)



vimp <- xgb.importance(model = model_xgb)
View(vimp)
pred<-predict(model_xgb,xgtest)
submit<-data.table(Loan_ID=test$Loan_ID,Loan_Status=pred)
submit$Loan_Status<-ifelse(submit$Loan_Status>0.5,"Y","N")
write.csv(submit,"submit_loan.csv",row.names = F)



####rank 19 loan prediction 3

