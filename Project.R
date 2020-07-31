#setting the environment and the directory into R.
setwd("F:/Capstone/11. Car Insurance")

#Importing data
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


#Setting Theme and Background Color
theme_color <- theme(legend.key = element_rect(fill="black"),
                     legend.background = element_rect(color="white", fill="#263238"),
                     plot.subtitle = element_text(size=6, color="white"),
                     panel.background = element_rect(fill="#dddddd"),
                     panel.border = element_rect(fill=NA),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                     panel.grid.minor.y = element_blank(),
                     plot.background = element_rect(fill="#263238"),
                     text = element_text(color="white"),
                     axis.text = element_text(color="white"))

data = read_xlsx("Carinsurancedatasetnew.xlsx")

write.csv(summary(data),"summary.csv")

#Missing Values

m <- dim(data)[1]
n<- dim(data)[2]
mv <- data.frame()
for (i in 1:m){
  mv_i <- data.frame()
  x <- colnames(data)[i]
  valu <- sum(is.na(data[,x])) / n
  name <- x
  df1 <- data.frame(name = name, val = valu)
  mv <- rbind(mv, df1)
}
print(mv)
threshold<- 0.25

print(mv[mv$val > 0.25,c("name","val")],row.names = F)

write.csv(mv,"mv.csv")

write.csv(colSums(is.na(data)),"missinvalues.csv")



write.csv(describe(data),"Des.csv")

write.xlsx(str(data),"str.xlsx")

#clubbing violation points
data$violationpts = as.numeric(apply(data[,80:119],1,sum))

data$ageofvehicle = 2016 - data$Year_1

datanew = subset(data, select = -c(1,8:11,25:29,32:51,54:62,64:67,70:73,80:119,121,122,127))

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

write.csv(datanew, "datanew.csv")


summary(datanew)
str(datanew)
write.csv(describe(datanew),"describe.csv")
colSums(is.na(datanew))
warnings()
colnames(datanew)

colSums(is.na(carinsdatanew))

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


#Univariate Analysis
print("Number of Policies Claimed")
table(datanew$ClaimStatus)

round(prop.table(table(datanew$ClaimStatus)),2)

round(prop.table(table(datanew$Renewed)),2)

round(prop.table(table(datanew$ClaimFrequency)),4)

round(prop.table(table(datanew$Units)),4)

round(prop.table(table(datanew$Billing_Term)),2)

round(prop.table(table(datanew$Type)),4)




boxplot(datanew$ageofvehicle, ylim = c(0,70), col = "yellowgreen",main = "BoxPlot of Age of Vehicle")

plot(datanew$ageofvehicle,datanew$Premium,xlim = c(0,70),col = c("blue","red"),
     xlab = "Age of Vehicle", 
     ylab = "Premium", 
     main = "Scatterplot of Age of Vehcle vs Premium")
plot(datanew$AgeUSdriving_1, datanew$Premium,xlim = c(16,85),ylim = c(0,2900),
     col = c("green","purple"),
     main = "Scatterplot of Age of Driver vs Premium",
     xlab = "Age of Driver",
     ylab = "Premium")



boxplot(datanew$AgeUSdriving_1, ylab = "Age",
        col = "gold", main = "Boxplot of Age of Drivers", notch = TRUE)


ggplot(datanew,aes(datanew$ClaimStatus))+geom_bar(col = "black",
                                                  aes(fill = ..count..))+geom_text(aes(y=..count..,
                                                                                       label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                   stat = 'count',
                                                                                   position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                   size=4)+scale_fill_gradient("Count",
                                                                                                               low = "gold",
                                                                                                               high = "darkblue")+labs(title = "Barplot of ClaimStatus")+xlab("ClaimStatus")+theme_color

ggplot(data = datanew,
       aes(datanew$Premium,fill = datanew$ClaimStatus))+geom_density(alpha = 0.5)+theme(legend.position = "right")

ggplot(datanew,aes(datanew$CoverageLiability))+geom_bar(col = "black",
                                                    aes(fill = ..count..))+geom_text(aes(y=..count..,
                                                                                         label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                     stat = 'count',
                                                                                     position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                     size=4)+scale_fill_gradient("Count",
                                                                                               low = "gold",
                                                                                               high = "darkblue")+labs(title = "Barplot of Coverage Liablity")+xlab("Coverage Liability")+theme_color
ggplot(datanew,aes(datanew$NoLossSigned))+geom_bar(col = "black",
                                                        aes(fill = ..count..))+geom_text(aes(y=..count..,
                                                                                             label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                         stat = 'count',
                                                                                         position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                         size=4)+scale_fill_gradient("Count",
                                                                                                                     low = "gold",
                                                                                                                     high = "darkblue")+labs(title = "Barplot of No Loss Signed")+xlab("No Loss Signed")+theme_color



ggplot(datanew, aes(datanew$CoverageLiability,datanew$ClaimFrequency))+geom_jitter(col = "purple")+labs(title = "Plot of Coverage Liability vs Claim Frequency")+theme_color


ggplot(datanew,aes(datanew$Premium))+geom_histogram(breaks = seq(0,2900,by = 100),
                                                                col = "black",
                                                                aes(fill = ..count..))+scale_fill_gradient("Count",
                                                                                                           low = "gold",
                                                                                                           high = "darkblue")+labs(title = "Histogram of Premium")+xlab("Premium")+theme_color


ggplot(datanew, aes(datanew$VehicleInspected_1,fill = ..count..))+geom_bar()+geom_text(aes(y=..count..,
                                                                                           label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                       stat = 'count',
                                                                                       position = position_dodge(width = 0.9), vjust = -0.25,
                                                                                       size=4)+labs(title = "Vehicle Inspection",xlab="VehicleInspected")+xlab("VehicleInspected")+theme_color
ggplot(datanew, aes(datanew$Sex_1,fill = ..count..))+geom_bar()+geom_text(aes(y=..count..,
                                                                                           label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                       stat = 'count',
                                                                                       position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                       size=4)+labs(title = "Gender stats")+xlab("Gender")+theme_color

ggplot(datanew, aes(datanew$ClaimFrequency,fill=..count..))+geom_bar()+geom_text(aes(y=..count..,
                                                                                     label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                 stat = 'count',
                                                                                 position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                 size=4)+labs(title = "Claim Frequency")+xlab("Claim Frequency")+theme_color


ggplot(datanew, aes(datanew$violationpts,fill=..count..))+geom_bar()+geom_text(aes(y=..count..,
                                                                                   label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                               stat = 'count',
                                                                               position = position_dodge(width = 0.9),vjust = -0.25,
                                                                               size=4)+labs(title = "Violation Points")+xlab("Violation Points")+theme_color

ggplot(datanew, aes(datanew$Units,fill=..count..))+geom_bar()+geom_text(aes(y=..count..,
                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                        stat = 'count',
                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                        size=4)+labs(title = "Number of Vehicles Covered under policy")+xlab("Number of Vehicles")+theme_color

plot_histogram(datanew)
plot_ly(datanew, x = ~Zip)
plot_ly(datanew, x = ~Type, color = "red")
plot_ly(datanew, x = ~Number_of_Driver, type = "histogram")
plot_ly(datanew, x= ~Premium,y = datanew$Type)
plot_ly(datanew, x = ~Total_Distance_To_Work,type = "bar")




#Bivariate Analysis
ggplot(datanew,aes(datanew$Type, datanew$Units))+geom_boxplot(col = c("black"), 
                                                              fill = c(rainbow(9)))+labs(title = "Number of Vehicles Covered under policy")+theme_color+xlab("Type")+ylab("Units")

ggplot(datanew,aes(datanew$Type, datanew$Premium))+geom_boxplot(col = c("black"), 
                                                                fill = c(rainbow(9)))+labs(title = "Premium vs Type of Insurance")+theme_color+xlab("Type")+ylab("Premium")


ggplot(datanew,aes(datanew$Units, datanew$Premium))+geom_point(col = c("black"))+labs(title = "Premium vs No. Units coveres")+theme_color+xlab("Number of Units")+ylab("Premium")

ggplot(datanew, aes(datanew$Billing_Term, fill = datanew$Premium))+geom_bar(col = c("black"), fill = c("gold","yellowgreen","lightblue"))+labs(title = "Premium vs Billing Term")+theme_color+xlab("Billing Term")+ylab("Premium")

ggplot(datanew, aes(datanew$ageofvehicle, fill = datanew$Premium))+geom_bar(col = c("black"))+labs(title = "Premium vs Age of Vehicle")+theme_color+xlab("Age of Vehicle")+ylab("Premium")

ggplot(datanew, aes(datanew$Renewed, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Renewd")+geom_text(aes(y=..count..,
                                                                                                                                                     label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                 stat = 'count',
                                                                                                                                                 position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                 size=4)+xlab("Renewed")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$Billing_Term, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Billing Term")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9), vjust = -0.25,
                                                                                                                                                            size=4)+xlab("Billing Term")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$ClaimFrequency, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Claim Frequency")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                            size=4)+xlab("Claim Frequency")+ylab("Claimstatus")+theme_color



ggplot(datanew, aes(datanew$Amendment, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Amendment")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                            size=4)+xlab("Amendment")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$NoLossSigned, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs No Loss Signed")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                            size=4)+xlab("No Loss Signed")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$CoverageLiability, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoverageLiability")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                            size=4)+xlab("Coverage Liability")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$CoverageMP, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoverageMP")+geom_text(aes(y=..count..,
                                                                                                                                                                label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                            stat = 'count',
                                                                                                                                                            position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                            size=4)+xlab("CoverageMP")+ylab("Claimstatus")+theme_color
ggplot(datanew, aes(datanew$CoveragePD_1, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoveragePD_1")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("CoveragePD_1")+ylab("Claimstatus")+theme_color
ggplot(datanew, aes(datanew$CoveragePIP_CDW, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoveragePIP_CDW")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("CoveragePIP_CDW")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$CoverageUMBI, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoverageUMBI")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("CoverageUMBI")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$CoverageUMPD, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs CoverageUMPD")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("CoverageUMPD")+ylab("Claimstatus")+theme_color
ggplot(datanew, aes(datanew$DriverAssigned_1, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs DriversAssigned")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("Drivers Assigned")+ylab("Claimstatus")+theme_color
ggplot(datanew, aes(datanew$violationpts, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs ViolationPoints")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("Violation Points")+ylab("Claimstatus")+theme_color


ggplot(datanew, aes(datanew$Units, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Units")+geom_text(aes(y=..count..,
                                                                                                                                                            label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                                        stat = 'count',
                                                                                                                                                        position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                                        size=4)+xlab("Units")+ylab("Claimstatus")+theme_color

ggplot(datanew, aes(datanew$Engine_1, datanew$Premium))+geom_point()

ggplot(datanew, aes(datanew$MaritalStatus_1, fill = ClaimStatus)) + geom_bar(position = "dodge")+labs(title = "ClaimStatus vs Marital Status")+geom_text(aes(y=..count..,
                                                                                                                                                  label=paste0(round(prop.table(..count..),2)*100,'%')),
                                                                                                                                              stat = 'count',
                                                                                                                                              position = position_dodge(width = 0.9),vjust = -0.25,
                                                                                                                                              size=4)+xlab("Martial Status")+ylab("Claimstatus")+theme_color



datarf = datanew
datarf$ClaimStatus = as.numeric(datarf$ClaimStatus)
datarf$ClaimFrequency = as.numeric(datarf$ClaimFrequency)
datarf$Renewed = as.numeric(datarf$Renewed)
datarf$NoLossSigned = as.numeric(datarf$NoLossSigned)
datarf$CoverageLiability = as.numeric(datarf$CoverageLiability)
datarf$CoverageMP = as.numeric(datarf$CoverageMP)
datarf$CoveragePD_1 = as.numeric(datarf$CoveragePD_1)
datarf$CoveragePIP_CDW = as.numeric(datarf$CoveragePIP_CDW)
datarf$CoverageUMBI = as.numeric(datarf$CoverageUMBI)
datarf$CoverageUMPD = as.numeric(datarf$CoverageUMPD)
datarf$Surcharge1Unit_1 = as.numeric(datarf$Surcharge1Unit_1)
datarf$Surcharge2Unit_1 = as.numeric(datarf$Surcharge2Unit_1)
datarf$Surcharge3Unit_1 = as.numeric(datarf$Surcharge3Unit_1)
datarf$VehicleInspected_1 = as.numeric(datarf$VehicleInspected_1)
datarf$MaritalStatus_1 = as.numeric(datarf$MaritalStatus_1)

corrdata = cor(datarf[sapply(datarf,is.numeric)])
corrplot(cor(datarf[sapply(datarf, is.numeric)]))


modeldata = subset(datanew,select = -c(6,22,24,33,34))
model <- glm(datanew$ClaimStatus~.,family = binomial(link = "logit"),data = modeldata, maxit = 100)

summary(model)

vif(model)

ld.vars <- attributes(alias(model)$Complete)$dimnames[[1]]
ld.vars

write.csv(vif(model), "vif.csv")

set.seed(123)
boruta <- Boruta(datanew$ClaimStatus~.,data = datanew, doTrace = 2)

plot(boruta, xlab = "",xaxt = "n")
lz<-lapply(1:ncol(boruta$ImpHistory),function(i)
  boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i])
names(lz) <- colnames(boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at =
       1:ncol(boruta$ImpHistory), cex.axis = 0.7)

print(boruta) 

write.csv(boruta$finalDecision, "boruta.csv")


#Balancing the data

