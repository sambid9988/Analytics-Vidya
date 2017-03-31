library(data.table)
library(ggplot2)
library(caret)
library(xgboost)
library(gbm)
library(dplyr)
train<-read.csv("train_63qYitG.csv",na.strings = c(""," ","?","NA",NA))
test<-read.csv("test_XaoFywY.csv",na.strings = c(""," ","?","NA",NA))
View(train)
dim(train)
#131662  14
dim(test)
#87395    13

test$Surge_Pricing_Type<-1
c<-list(train,test)
combin<-rbindlist(c)
str(combin)

###EDA
###function to graph variable for continous variables        
tr <- function(a){
  ggplot(data = combin, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()            
}            

tr(combin$Trip_Distance)
combin$Trip_Distance<-ifelse(combin$Trip_Distance>96.16,96.16,combin$Trip_Distance)
summary(combin)
tr(combin$Customer_Since_Months)
tr(combin$Customer_Rating)
tr(combin$Cancellation_Last_1Month)
tr((combin$Var2))
tr(combin$Var3)
tr((combin$var23_ratio))
all_bar <- function(i){
  ggplot(train,aes(x=i,fill=as.factor(train$Surge_Pricing_Type)))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}
all_bar(as.factor(train$Type_of_Cab))
all_bar(train$Destination_Type)
all_bar(train$Cancellation_Last_1Month)

###5% less values should be put in other

combin[,prop.table(table(Type_of_Cab))]
combin[,prop.table(table(Confidence_Life_Style_Index))]
combin[,prop.table(table(Destination_Type))]
combin[,prop.table(table(Customer_Rating))]
combin[,Type_of_Cab_na:=ifelse(sapply(combin$Type_of_Cab,is.na)==T,1,0)]

combin[,Confidence_Life_Style_Index_na:=ifelse(sapply(combin$Confidence_Life_Style_Index,is.na)==T,1,0)]
combin$Cancellation_Last_1Month<-ifelse(combin$Cancellation_Last_1Month>4,4,combin$Cancellation_Last_1Month)
combin<-mutate(combin,var23_ratio=Var2/Var3)
combin$Var1[is.na(combin$Var1)]<-0
combin<-mutate(combin,var123_add=Var1+Var2+Var3)
combin$Var1<-NULL
combin$Var2<-NULL
combin$Var3<-NULL
##one hot encode
combin$Gender<-as.integer(combin$Gender)-1
combin$Trip_ID<-NULL
str(combin)
all_data<-combin

all_data<-select(combin,Confidence_Life_Style_Index,Destination_Type,Type_of_Cab)


for(i in names(all_data)){
  p <- 5/100
  ld <- names(which(prop.table(table(all_data[[i]])) < p))
  levels(all_data[[i]])[levels(all_data[[i]]) %in% ld] <- "Other"
}
combin$Confidence_Life_Style_Index<-NULL
combin$Destination_Type<-NULL
combin$Type_of_Cab<-NULL
str(all_data)
combin<-bind_cols(combin,all_dum)

str(combin)
combin$Type_of_Cab<-NULL
combin$Destination_Type<-NULL
combin$Confidence_Life_Style_Index<-NULL
dmy<-dummyVars("~.",data=all_data)
all_dum<-data.frame(predict(dmy,newdata=all_data))
str(all_dum)
str(combin)
train<-combin[1:131662,]
test<-combin[131663:219057,]

train$Surge_Pricing_Type<-NULL
train$Confidence_Life_Style_Index_na<-NULL
train$Gender<-NULL
test$Gender<-NULL
test$Confidence_Life_Style_Index_na<-NULL
x_target<-train$Surge_Pricing_Type

str(train)
xgtrain <- xgb.DMatrix(data = as.matrix(train), label = x_target, missing = NA)
xgtest <- xgb.DMatrix(data = as.matrix(test), missing = NA)
params<-list()
params$objective="multi:softmax"
params$booster<-"gbtree"
params$eta<-0.1
params$num_class<-4
params$max_depth<-5
params$subsample <- 0.8
params$colsample_bytree <- 0.8
params$min_child_weight <- 6
params$eval_metric <- "merror"

model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 500, nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed(123))
model_xgb <- xgb.train(params = params, xgtrain, nrounds = 188)
pred<-predict(model_xgb,xgtest)

vimp<-xgb.importance(model=model_xgb)
View(vimp)
xgb.plot.importance(importance_matrix = vimp)
names(train)

submit<-data.table(Surge_Pricing_Type=pred)
write.csv(submit,"submit.csv",row.names = F)
