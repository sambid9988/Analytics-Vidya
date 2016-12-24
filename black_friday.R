library(data.table)
library(dplyr)
library(xgboost)
library(dummies)
###reading files into R
black_train<-fread("train.csv",stringsAsFactors = T)
black_test<-fread("test.csv",stringsAsFactors = T)
###filling the purchase for test so that we can join testset and training set)
black_test[,Purchase:=mean(black_train$Purchase)]
c<-list(black_train,black_test)
combin<-rbindlist(c)
###rm(black_test,black_train)
dim(combin)
combin[,prop.table(table(Stay_In_Current_City_Years))]
str(combin)
###converting gender 
combin[,Gender:=as.numeric(as.factor(combin$Gender))-1]

###converting levels of age(label encoding)
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6
library(dummies)###for encoding

combin$Age<-as.numeric(combin$Age)
####getting the frequency of user and product values
combin[, User_Count := .N, by = User_ID]
combin[, Product_Count := .N, by = Product_ID]
##### getting the mean of purchase by product purchased
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]
combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) ==  "4+"] <- "4"
combin$Stay_In_Current_City_Years<-as.numeric(combin$Stay_In_Current_City_Years)-1
combin<-combin[,-1]
c.train <- combin[1:nrow(black_train),]
c.test<-combin[-(1:nrow(black_train)),]

combin<-select(combin,-Purchase,everything())

#####xgboost
x_target<-c.train$Purchase
xgtrain<-xgb.DMatrix(data=as.matrix(c.train[,1:15]),label=x_target,missing = NA)
xgtest<-xgb.DMatrix(data=as.matrix(c.test[,1:15]),missing = NA)
nrow(c.test)
params <- list()
params$objective <- "reg:linear"
params$eta <- 0.1
params$max_depth <- 5
params$subsample <- 0.5
params$colsample_bytree <- 1
params$min_child_weight <- 2
params$eval_metric <- "rmse"



model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 1000,early.stop.round = 30, nfold = 5,maximize = F)
model_xgb <- xgb.train(params = params, xgtrain, nrounds = 1000)
vimp<-xgb.importance(model=model_xgb)
#### using importance to remove features (lowest gain),removed 5 features

View(vimp)


#####prediction 
pred <- predict(model_xgb, xgtest)
submit<-data.table(outcome=pred)
write.csv(submit,"zeroes2.csv")
