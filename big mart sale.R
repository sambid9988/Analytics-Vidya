bigmart_data<-read.csv("Train_UWu5bXk.csv",na.strings =c("",".","NA"))
test_data<-read.csv("Test_u94Q5KV.csv",na.strings = c("",".","NA"))
colSums(is.na(bigmart_data))
dim(bigmart_data)
dim(test_data)
summary(bigmart_data)
View(bigmart_data)
hist(bigmart_data$Item_Outlet_Sales)
boxplot(bigmart_data$Item_Outlet_Sales)
index<-3101.30+1.5*IQR(bigmart_data$Item_Outlet_Sales)
index
x<-bigmart_data$Item_Outlet_Sales>6501.873
sum(x)
str(bigmart_data)
library(dplyr)


all_data<-bind_rows(bigmart_data,test_data)###main data set
dim(test_data)
dim(bigmart_data)
View(all_data)
colSums(is.na(all_data))
str(all_data)
library(Hmisc)
library(missForest)
#######HMISC Impute
all_data_areg<-aregImpute(~Item_Weight+Outlet_Size,data=all_data,n.impute =5 )
all_data_areg$imputed$Item_Weight
hm_data<-impute.transcan(all_data_areg,imputation = 1,data=all_data,list.out = T,pr=F,check = F)
all_data_hm<-cbind.data.frame(all_data,hm_data)
colSums(is.na(all_data_hm))
View(all_data_hm)
str(all_data_hm)
names(all_data_hm)
all_data_hm<-all_data_hm[,-12]
write.csv(all_data_hm,"all_data_hm.csv")
############MiSS forest
data<-all_data[,-1]
miss_data<-missForest(data,maxiter =2,ntree = 300)
all_data_miss<-(miss_data$ximp)
str(all_data_miss)
View(all_data_miss)
colSums(is.na(all_data_miss))
write.csv(all_data_miss,"all_data_miss.csv")
################
sum_data<-all_data%>%
  group_by(Item_Type)%>%
  summarise(mean(Item_Weight,na.rm=T))
med_data<-all_data
str(med_data)
med_data$Item_Weight[is.na(med_data$Item_Weight) & med_data$Item_Type=="Baking Goods"]<-12.27
write.csv(med_data,"med.data.csv")                            
colSums(is.na(med_data))
med_data$impute<-with(med_data,impute(Outlet_Size,"random"))
##########random input of outlet size
all_data%>%
    group_by(Item_Type)%>%
    summarise(mean(Item_Visibility))
med_data$Item_Visibility[med_data$Item_Visibility==0]<-0.61

x<-mapvalues(med_data$Item_Type,from = c("Baking Goods","Breads","Breakfast","Canned","Dairy","Frozen Foods","Fruits and Vegetables","Hard Drinks","Health and Hygiene","Household","Meat","Others","Seafood","Snack Foods","Soft Drinks","Starchy Foods"),to=c("FD","FD","FD","FD","FD","FD","FD","DR","NC","NC","FD","NC","FD","FD","DR","FD"))
med_data<-select(med_data,-Item_Identifier)
write.csv(med_data,"med.csv")
med_data<-mutate(med_data,year=2013-Outlet_Establishment_Year)
levels(med_data$Item_Fat_Content)
levels(med_data$Item_Fat_Content)[levels(med_data$Item_Fat_Content)=="Low Fat"]<-"lowfat"
##########################feature engineering is done.
library(caret)
library(doParallel)
library(caretEnsemble)
cl <- makeCluster(4)
registerDoParallel(cl)

data<-read.csv("med.csv")
str(data)
data<-data[,-5]
med_data<-dummyVars("~.",data=sale_data)
sale_data<-data.frame(predict(med_data,newdata =sale_data))
str(sale_data)
data<-data[,-1]
data<-mutate(data,item_sold=Item_Outlet_Sales /Item_MRP )
data<-select(trn,-Outlet_Identifier.OUT010:Outlet_Identifier.OUT049) 
sale_data<-data
sale_data$item_sold<-round(sale_data$item_sold)
dim(data)
View(sale_data)
sale_train_data<-slice(sale_data,1:8523)
sale_test_data<-slice(sale_data,8524:14204)
dim(sale_train_data)
str(sale_data)
colSums(is.na(sale_train_data))
sale_data<-sale_data[,-6]
colSums(is.na(sale_data))
write.csv(sale_test_data,"sale_test.csv")
###########################
set.seed(1234)
mycontrol<-trainControl(method="cv",number=10,verboseIter = T )

avg_model<-caretList(Item_Outlet_Sales~.,data=sale_train_data,methodList=c("gbm","cubist"),metric="RMSE",verbose=F)
avg_model
modelCor(resamples(avg_model))
getTrainPerf(avg_model)###1084
prediction_avg<-as.data.frame(predict(avg_model,sale_test_data))
str(sale_test_data)
sale_test_data<-sale_test_data[,-13]
head(prediction_avg)
str(prediction_avg)
prediction_avg<-mutate(prediction_avg,mean_sale=mean(gbm+cubist+gbm+cubist))
write.csv(prediction_avg,"avg.csv")



cb.modl<-train(Item_Outlet_Sales ~.,data=sale_train_data,method="cubist",metric="RMSE",trControl=mycontrol)
cb.modl
gerTrainperf(cb.model)
modelCor(resamples(cb.modl))
prediction<-as.data.frame(predict(cb.modl,))

pred.gb<-predict(ex.modl,sale_test_data)
pred.gb1<-data.frame(pred.cub)
pred.


pred.cub1<-mutate(pred.cub1,item_sales=sale_test_data$Item_MRP*pred.cub1$pred.cub)
write.csv(pred.cub1,"pred.cub_final.csv")

model.sv<-train(Item_Outlet_Sales~.,data=sale_data,method="svmRadial",trControl=mycontrol,metric="RSME")
dotplot(model.list)
plot(modelCor(resamples(model.list)))
dotplot(resamples(model.list))###

glm_ensemble <- caretStack(
  c(model.list$cubist,model.list$svmRadialSigma),
  method="lm",
  metric="RMSE")
glm_ensemble$error
model.list
glm_ensemble
dim(sale_test_data)
str(sale_test_data)

stack.pred<-predict(glm_ensemble,newdata=sale_test_data,type="raw")
stack_pred<-data.frame(stack.pred)
write.csv(stack.pred,"stack.pred2.csv")









xgb_model<-train(Item_Outlet_Sales~.,data=sale_data,method="xgbTree",metric="RMSE",tuneGrid=xgb.grid)
xgb_model
getTrainPerf(xgb_model)####1083.45
str(sale_test_data)
colSums(is.na(sale_test_data))
sale_test_data>-sale_test_data[]
pred<-predict(xgb_model,sale_test_data)
ans<-data.frame(pred)
View(ans)
write.csv(ans,"x.csv")

sale_test_data<-sale_test_data[-1,]
dim(sale_test_data)
names(sale_data)
features<-c("Item_Weight","Item_Fat_Content.lowfat","Item_Fat_Content.regular","Item_Visibility", "Item_MRP","Outlet_Location_Type.Tier.1","Outlet_Location_Type.Tier.2","Outlet_Location_Type.Tier.3","Outlet_Type.Grocery.Store","Outlet_Type.Supermarket.Type1","Outlet_Type.Supermarket.Type2","Outlet_Type.Supermarket.Type3","Outlet_Size.High","Item_type.FD", "Item_type.DR","Item_type.NC","year" )
xbgtrain<-xgb.DMatrix(data=as.matrix(sale_data[,features]),label=sale_data$Item_Outlet_Sales,missing = NA)
xbgtest<-xgb.DMatrix(data=as.matrix(sale_test_data[,features]),missing=NA)

params<-list()
params$Objective<-"binary:logistic"
params$eta<-0.01
params$max_depth<-5
params$subsample<-0.3
params$colsample_bytree<-0.5
params$gamma<-0
params$min_child_weight<-2
params$eval_metric<-"rmse"

model.xgb<-xgb.cv(params=params,xbgtrain,booster="gbtree",nrounds =560,nfold = 10,early.stop.round = 50,prediction = T)
model.xgb$dt
model_xgb<-xgb.train(params = params,xbgtrain,nrounds = 100)
predict_xgb<-predict(model_xgb,xbgtest)
submit<-data.frame(Outcome=predict_xgb)
View(submit)

write.csv(submit,"submit.csv")


#######################



pred1<-predict(gbm_model,sale_test_data)
View(pred1)
ans<-data.frame(pred1)
write.csv(ans,"gbm_model.csv")


















