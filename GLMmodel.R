library(ggplot2)
library(lattice)
library(caret)
library(e1071)
Data<- Telco_customer_churn

for (i in 1:(ncol(Data)-1)) {
  if (is.character(Data[, i])==TRUE){
    for(j in 1:nrow(Data)){
      ascis <- as.numeric(charToRaw(Data[j, i]))
      Data[ j, i] <- sum(ascis)
    }
  }
  Data[,i] <- as.numeric(Data[,i])
}
Data[,ncol(Data)] <-(as.factor(Data[,ncol(Data)]))
set.seed(15)
datasplit<- createDataPartition(Data$Churn.Value, p=0.70, list=FALSE)
trainingset<- Data[datasplit,]
testingset<- Data[-datasplit,]


glmmodel<-glm(Churn.Value ~ ., data = trainingset, family = binomial, control =  list(maxit=10000))
summary(glmmodel)
prediction<- predict(glmmodel, newdata= testingset, type= "response")
confmat<- table(testingset$Churn.Value, prediction >0.5)
confmat

FeatureSelectedData<-subset(Data, select = c(Tenure.Months,Contract, Online.Security, Tech.Support, Internet.Service, Online.Backup,Device.Protection,Total.Charges,Streaming.Movies,Monthly.Charges,Streaming.TV,Dependents,Paperless.Billing,Partner,CLTV,Payment.Method,Senior.Citizen, Multiple.Lines, City, Gender, Churn.Value))
set.seed(15)
#split data with features
datasplit_feature<- createDataPartition(FeatureSelectedData$Churn.Value, p=0.70, list=FALSE)
trainingset_feature<- FeatureSelectedData[datasplit_feature,]
testingset_feature<- FeatureSelectedData[-datasplit_feature,]
glmmodel_features <- glm(Churn.Value ~ Tenure.Months + Contract + Online.Security + Tech.Support + Internet.Service  + Online.Backup + Device.Protection + Total.Charges + Streaming.Movies + Monthly.Charges + Streaming.TV,Dependents + Paperless.Billing + Partner + CLTV + Payment.Method + Senior.Citizen + Multiple.Lines + City + Gender, data = trainingset_feature,  family = binomial)
summary(glmmodel_features)
prediction_feature<- predict(glmmodel_features, newdata= testingset_feature, type= "response")
confmat_feature<- table(testingset_feature$Churn.Value, prediction_feature >0.5)
confmat_feature
Accuracy<- (949+488)/(949+599+72+488)
Sensitivity<- 488/(488+72)
Specificity<-949/(949+599)
