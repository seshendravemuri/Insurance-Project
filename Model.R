setwd("F:/Capstone/11. Car Insurance")

library(psych)
library(DataExplorer)
library(readxl)##Read xlsx
library(dplyr) ##to scale data
library(corrplot) ##library for correlation
library(lattice) ## for plots
library(caret) ##for confusionMatrix
library(ROCR) ##for auc,KS
library(ineq) ##gini
library(caTools) ##to Split data
library(naivebayes) ##Naive Bayes model for Numeric Predictors
library(e1071) ##For Naise Bayes
library(class) ## For KNN Classifier
library(pscl) ##Maximum likelihood estimation
library(lmtest) ##diagnostic checking in linear regression models
library(purrr) ## for Visualization
library(tidyr)## for Visualization
library(ggplot2) ## Data Visualization
library(GGally)
library(car) ## vif
library(lmtest)
library(plotly)
library(gapminder)
library(tidyverse)
library(ggthemes)
library(DMwR)
library(Boruta)
library(GoodmanKruskal)
library(rpart.plot)
library(randomForest)
library(ModelMetrics)
library(ROCR)
library(ineq)
library(InformationValue)
install.packages("GoodmanKruskal")


insdata = read_xlsx("Carinsurancedatasetnew.xlsx")


insdata$violationpts = as.numeric(apply(insdata[,80:119],1,sum))

insdata$ageofvehicle = 2016 - insdata$Year_1

datanew = subset(insdata, select = -c(1,8:11,25:29,32:51,54:62,64:67,70:73,80:119,121,122,127))

getmeadian <- function(x){
  uniqv <- unique(x)
  uniqv[median(tabulate(match(x,uniqv)))]
}

datanew$CoverageMP[is.na(datanew$CoverageMP)]<- "None"
datanew$CoveragePD_1[is.na(datanew$CoveragePD_1)]<- "None"
datanew$CoveragePIP_CDW[is.na(datanew$CoveragePIP_CDW)]<- "None"
datanew$CoverageUMBI[is.na(datanew$CoverageUMBI)]<- "None"
datanew$CoverageUMPD[is.na(datanew$CoverageUMPD)]<- "None"
datanew$Surcharge1Unit_1[is.na(datanew$Surcharge1Unit_1)]<- "N"
datanew$Surcharge2Unit_1[is.na(datanew$Surcharge2Unit_1)]<- "N"
datanew$Surcharge3Unit_1[is.na(datanew$Surcharge3Unit_1)]<- "N"

datanew$Engine_1[is.na(datanew$Engine_1)] <- getmeadian(datanew$Engine_1)



#Converting the Categorical Variables
datanew$CoverageUMBI = ifelse(datanew$CoverageUMBI == "Accepted",1,0)
datanew$CoverageUMPD = ifelse(datanew$CoverageUMPD == "Accepted",1,0)
datanew$CoverageMP = ifelse(datanew$CoverageMP == "535",1,0)
datanew$CoveragePD_1 = ifelse(datanew$CoveragePD_1 == "1000/1000",1,
                              ifelse(datanew$CoveragePD_1 == "500/500",2,0))
datanew$CoverageLiability = ifelse(datanew$CoverageLiability == "20/40/15",1,
                                   ifelse(datanew$CoverageLiability == "25/50/25",2,
                                          ifelse(datanew$CoverageLiability == "30/60/25",3,0)))
datanew$CoveragePIP_CDW = ifelse(datanew$CoveragePIP_CDW == "2535",1,
                                 ifelse(datanew$CoveragePIP_CDW == "2569",2,0))
datanew$Sex_1 = ifelse(datanew$Sex_1 == "M",1,0)
datanew$Surcharge1Unit_1 = ifelse(datanew$Surcharge1Unit_1 == "Y",1,0)
datanew$Surcharge2Unit_1 = ifelse(datanew$Surcharge2Unit_1 == "Y",1,0)
datanew$Surcharge3Unit_1 = ifelse(datanew$Surcharge3Unit_1 == "Y",1,0)
datanew$MaritalStatus_1 = ifelse(datanew$MaritalStatus_1 == "M",1,0)
datanew$Billing_Term = ifelse(datanew$Billing_Term == "1",12,
                              ifelse(datanew$Billing_Term == "3",3,
                                     ifelse(datanew$Billing_Term =="6",6,0)))



datanew$Sex_1 = as.factor(datanew$Sex_1)
datanew$ClaimStatus = as.factor(datanew$ClaimStatus)
datanew$Zip = as.character(datanew$Zip)
datanew$ClaimFrequency = as.factor(datanew$ClaimFrequency)
datanew$Renewed = as.factor(datanew$Renewed)
datanew$NoLossSigned = as.factor(datanew$NoLossSigned)
datanew$CoverageLiability = as.factor(datanew$CoverageLiability)
datanew$CoverageMP = as.factor(datanew$CoverageMP)
datanew$CoveragePD_1 = as.factor(datanew$CoveragePD_1)
datanew$CoveragePIP_CDW = as.factor(datanew$CoveragePIP_CDW)
datanew$CoverageUMBI = as.factor(datanew$CoverageUMBI)
datanew$CoverageUMPD = as.factor(datanew$CoverageUMPD)
datanew$Surcharge1Unit_1 = as.factor(datanew$Surcharge1Unit_1)
datanew$Surcharge2Unit_1 = as.factor(datanew$Surcharge2Unit_1)
datanew$Surcharge3Unit_1 = as.factor(datanew$Surcharge3Unit_1)
datanew$VehicleInspected_1 = as.factor(datanew$VehicleInspected_1)
datanew$MaritalStatus_1 = as.factor(datanew$MaritalStatus_1)
datanew$Billing_Term = as.factor(datanew$Billing_Term)
datanew$Type = as.factor(datanew$Type)


str(datanew)

b <- Boruta(datanew$ClaimStatus~., data = datanew, doTrace = 2)
plot(b, xlab = "",xaxt = "n")
lz<-lapply(1:ncol(b$ImpHistory),function(i)
  b$ImpHistory[is.finite(b$ImpHistory[,i]),i])
names(lz) <- colnames(b$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at =
       1:ncol(b$ImpHistory), cex.axis = 0.7)

b.df <- attStats(b)
write.csv(b.df,"boruta final.csv")

cleandata = subset(datanew, select = -c(6,10,11,12,13,15,17,18,19,20,22,24,26,27,28,29,33,34,36))

str(cleandata)

prop.table(table(cleandata$ClaimStatus))

#Balancing the dataset by Using SMOTE
smote <- SMOTE(ClaimStatus~., data = as.data.frame(cleandata), perc.over = 1000, pperc.under = 500)

prop.table(table(smote$ClaimStatus))

#Splitting data 
set.seed(1000)
split = createDataPartition(smote$ClaimStatus, p = 0.6, list = FALSE)

smote_train = smote[split,]
smote_test = smote[-split,]


#CrossValidation
#cvfolds = createMultiFolds(smote_train$ClaimStatus, k=5)
#cv.ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 3, index = cvfolds)


#Logistic Regression
smote_train = subset(smote_train, select = -c(2,10))#Since Claim Frequency & Coverage PD are not signficant variables we have Excluded it from dataset
smote_test = subset(smote_test, select = -c(2,10))
model <- glm(smote_train$ClaimStatus~., data = smote_train, family = binomial(link = "logit"))

model

vif(model)

lrtest(model)

pR2(model)

summary(model)


smote_test$predicted = predict(model, newdata = smote_test, type = "response")

smote_test$predicted = ifelse(smote_test$predicted<0.5,"0","1")
smote_test$predicted = as.factor(smote_test$predicted)
confusionMatrix(smote_test$ClaimStatus, smote_test$predicted)

#By observing VIF scores we have observed that Rental & towing are highly correlated, hence we can 
#drop these variables to get better results
attach(smote_train)
model2 <- glm(ClaimStatus~ Premium+Billing_Term+Renewed+Number_of_Driver+AgeUSdriving_1+
                CoverageLiability+Engine_1+MaritalStatus_1+Units+VehicleInspected_1+
                Total_Distance_To_Work+Type+violationpts+ageofvehicle, data = smote_train, 
              family = binomial(link = "logit"))

summary(model2)

vif(model2)

lrtest(model2)

pR2(model2)


smote_train$predicted = predict(model2, newdata = smote_train, type = "response")
smote_train$predicted = ifelse(smote_train$predicted<0.5,"0","1")
smote_train$predicted = as.factor(smote_train$predicted)

confusionMatrix(smote_train$ClaimStatus, smote_train$predicted)

#AUC
s <- predict(model2, newdata = smote_train, type = "response")
sr <- prediction(s, smote_train$ClaimStatus)
srf <- performance(sr, measure = "tpr", x.measure = "fpr")
plot(srf, main = "AUC ROC Curve for Traindata")

auc_train <- performance(sr, measure = "auc")
auc_train <- auc_train@y.values[[1]]

auc_train

#Gini
gini2 = (2*auc_train)-1

gini2


smote_test$predicted = predict(model2, newdata = smote_test, type = "response")

smote_test$predicted = ifelse(smote_test$predicted<0.5,"0","1")
smote_test$predicted = as.factor(smote_test$predicted)
confusionMatrix(smote_test$ClaimStatus, smote_test$predicted)


#AUC
z <- predict(model2, newdata = smote_test, type = "response")
zr <- prediction(z, smote_test$ClaimStatus)
zrf <- performance(zr, measure = "tpr", x.measure = "fpr")
plot(zrf, main = "AUC ROC Curve for Testdata")

auc_test <- performance(zr, measure = "auc")
auc_test <- auc_test@y.values[[1]]

auc_test

#Gini
gini1 = (2*auc_test)-1

gini1

#Random Forrest
set.seed(1000)
rfdata_train = smote[split,]
rfdata_test = smote[-split,]

rfdata_train = subset(rfdata_train, select = -c(2,10))
rfdata_test = subset(rfdata_test, select = -c(2,10))
rfmodel <- randomForest(rfdata_train$ClaimStatus~., ntree = 501,mtry = 5, nodesize = 10,
                        importance = TRUE, data = rfdata_train)
rfmodel

#Tree Calculation based on Error Rate
min(rfmodel$err.rate)

plot(rfmodel,main="")
legend("topright",c("OOB","0","1"),text.col = 1:6,lty = 1:3,col = 1:3)
title(main = "Error Rates Random Forest rfdata_train")

rfmodel$importance

#Tune Random Forest
set.seed(1000)
tunerfmodel=tuneRF(x=rfdata_train[,-which(colnames(rfdata_train)=="ClaimStatus")],
                  y=rfdata_train$ClaimStatus,
                  mtryStart = 9,
                  ntreeTry = 101,
                  stepFactor = 1.2,
                  improve = 0.001,
                  trace = TRUE,
                  plot = TRUE,
                  doBest = TRUE,
                  nodesize=10,
                  importance=TRUE
)
tunerfmodel

varImpPlot(tunerfmodel)

rfdata_train$predict = predict(tunerfmodel, newdata = rfdata_train, type = "response")


confusionMatrix(rfdata_train$ClaimStatus, rfdata_train$predict)



rfdata_test$predict = predict(tunerfmodel, newdata = rfdata_test, type = "response")
confusionMatrix(rfdata_test$ClaimStatus, rfdata_test$predict)

plot(tunerfmodel,main="")
legend("topright",c("OOB","0","1"),text.col = 1:6,lty = 1:3,col = 1:3)
title(main = "Error Rates Random Forest rfdata_train")



#AUC
r <- predict(tunerfmodel, newdata = rfdata_test, type = "prob")

pretty_colours <- c("#F8766D","#00BA38","#619CFF")

classes <- levels(rfdata_test$ClaimStatus)
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(rfdata_test[,1] ==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(r[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}


#NaiveBayes
set.seed(1000)
nb_train = smote[split,]
nb_test = smote[-split,]

nb_train = subset(nb_train, select = -c(2,10))
nb_test = subset(nb_test, select = -c(2,10))

nbmodel <- naiveBayes(nb_train$ClaimStatus~., data = nb_train)
nbmodel
nb_train$predict = predict(nbmodel, newdata = nb_train, type = "class")


confusionMatrix(nb_train$ClaimStatus, nb_train$predict, positive = "1")


nb_test$predict = predict(nbmodel, newdata = nb_test, type = "class")
confusionMatrix(nb_test$ClaimStatus, nb_test$predict, positive = "1")


#Bagging

install.packages(c("xgboost","ipred"))
library(MASS)
library(xgboost)
install.packages("gbm")
install.packages("ipred")
library(ipred)
library(gbm)
library(rpart)

set.seed(1000)
bagdata_train = smote[split,]
bagdata_test = smote[-split,]

bagdata_train = subset(bagdata_train, select = -c(2,10))
bagdata_test = subset(bagdata_test, select = -c(2,10))
baggingmodel <- bagging(bagdata_train$ClaimStatus~., data = bagdata_train, coob = TRUE)

baggingmodel

pred_bag_model <- predict(baggingmodel, bagdata_test)
confusionMatrix(pred_bag_model, bagdata_test$ClaimStatus)

pred_t <- predict(baggingmodel, bagdata_train)
confusionMatrix(pred_t, bagdata_train$ClaimStatus)

#Boosting

boost_train = bagdata_train
boost_test = bagdata_test

boost_ctrl <- trainControl(number = 10)
xgbGrid<-expand.grid(eta = 0.3, max_depth=1,nrounds = 50,gamma = 0,colsample_bytree = 0.6,
                     min_child_weight = 1, subsample = 1)
xgbosstmodel <-  train(ClaimStatus ~., boost_train,trControl = boost_ctrl,
                         tuneGrid = xgbGrid,metric = "Accuracy",method = "xgbTree")
summary(xgbosstmodel)

pred_xgb_t <- predict(xgbosstmodel,boost_train)

confusionMatrix(pred_xgb_t,boost_train$ClaimStatus)

pred_xgb <- predict(xgbosstmodel,boost_test)


confusionMatrix(pred_xgb,boost_test$ClaimStatus)


plot(varImp(xgbosstmodel))


#cart
cartdata_train = smote[split,]
cartdata_test = smote[-split,]

cartdata_train = subset(cartdata_train, select = -c(2,10))
cartdata_test = subset(cartdata_test, select = -c(2,10))

cartParameters=rpart.control(minsplit = 500,cp=0,xval = 10, minbucket = 100 )
cartmodel=rpart(formula = cartdata_train$ClaimStatus~.,data = cartdata_train,
                method = "class",control = cartParameters)
cartmodel

fancyRpartPlot(cartmodel)
printcp(cartmodel)


plotcp(cartmodel)

bestcp = cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]),"CP"]
bestcp

ptree = prune(cartmodel,cp = bestcp, "CP")
ptree

fancyRpartPlot(ptree)
printcp(ptree)

cp <- predict(ptree, newdata = cartdata_train, type = "class")
confusionMatrix(cp, cartdata_train$ClaimStatus)


cp_t <- predict(ptree, newdata = cartdata_test, type = "class")
confusionMatrix(cp_t, cartdata_test$ClaimStatus)

varImp(ptree)



