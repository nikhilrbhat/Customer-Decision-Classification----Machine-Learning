
#Getting car Data
carData=read.csv("car.data.csv")
summary(carData)
str(carData)

library(caTools)

set.seed(8008)

#Splitting Cardata in 80:20 ratio for training and testing purposes
split= sample.split(carData$shouldBuy, SplitRatio = 0.8)
trainCarData= subset(carData, split == TRUE)
testCarData= subset(carData, split == FALSE)

#viewing dependent and independent variables in train data
independent_train=trainCarData[,1:6]
dependent_train=trainCarData[,7]

#viewing dependent and independent variables in test data
independent_test=testCarData[,1:6]
dependent_test=testCarData[,7]

#-------------------------------------------------------------------------------------------------
#install.packages("rpart")
#install.packages("pROC")
#install.packages("rpart.plot")
library(rpart)
library(pROC)
library(rpart.plot)

#Forming Car Decision Tree with train data

#-------------------------------------------------------------------------------------------------
#minsplit = 10
carTree = rpart(shouldBuy~.,data=trainCarData,method="class",control=rpart.control(minsplit=10))
carTree
#Checking Test Data on Car Tree
predCar=predict(carTree,newdata=testCarData[,-7],type="class")
#creating confusion matrix
treeCM=table(testCarData[,7],predCar)
treeCM
#Manually Cross-Checking actual dependent variables
summary(dependent_test)
#Finding Accuracy
sum(diag(treeCM))/sum(treeCM)

#-------------------------------------------------------------------------------------------------
#minsplit = 50
carTree = rpart(shouldBuy~.,data=trainCarData,method="class",control=rpart.control(minsplit=50))
carTree
#Checking Test Data on Car Tree
predCar=predict(carTree,newdata=testCarData[,-7],type="class")
#creating confusion matrix
treeCM=table(testCarData[,7],predCar)
treeCM
#Manually Cross-Checking actual dependent variables
summary(dependent_test)
#Finding Accuracy
sum(diag(treeCM))/sum(treeCM)

#-------------------------------------------------------------------------------------------------
#minsplit = 75
carTree = rpart(shouldBuy~.,data=trainCarData,method="class",control=rpart.control(minsplit=75))
carTree
#Checking Test Data on Car Tree
predCar=predict(carTree,newdata=testCarData[,-7],type="class")
#creating confusion matrix
treeCM=table(testCarData[,7],predCar)
treeCM
#Manually Cross-Checking actual dependent variables
summary(dependent_test)
#Finding Accuracy
sum(diag(treeCM))/sum(treeCM)

#-------------------------------------------------------------------------------------------------
#minsplit = 100
carTree = rpart(shouldBuy~.,data=trainCarData,method="class",control=rpart.control(minsplit=100))
carTree
#Checking Test Data on Car Tree
predCar=predict(carTree,newdata=testCarData[,-7],type="class")
#creating confusion matrix
treeCM=table(testCarData[,7],predCar)
treeCM
#Manually Cross-Checking actual dependent variables
summary(dependent_test)
#Finding Accuracy
sum(diag(treeCM))/sum(treeCM)

#-------------------------------------------------------------------------------------------------
#minsplit = 10
carTree = rpart(shouldBuy~.,data=trainCarData,method="class",control=rpart.control(minsplit=10))
#Checking Test Data on Car Tree
predCar=predict(carTree,newdata=testCarData[,-7],type="class")
#creating confusion matrix
treeCM=table(testCarData[,7],predCar)
#Finding Accuracy
accuracy <- sum(diag(treeCM))/sum(treeCM)


#Plotting Car Decision Tree
rpart.plot(carTree)
prp(carTree)

predCar=predict(carTree,newdata=testCarData[,-7],type="prob")
#
r1 <- multiclass.roc(dependent_test,predCar[,1])
auc1 <- auc(r1)
print(auc1)
#Plotting ROC
plot(roc(dependent_test,predCar[,1]))

x1<-paste("Decision Tree : minsplit = 10",",","AUC= ",round(auc1,6),",","Accuracy=",round(accuracy,5))
plot(roc(dependent_test,predCar[,1]),main=x1)

#-------------------------------------------------------------------------------------------------
#install.packages('randomForest')

#Random forest for n=500
library(randomForest)
#creating random forest 
random_f_500=randomForest(independent_train, dependent_train, ntree=500) 
#predicting test data by passing through random forest
random_fp_500=predict(random_f_500,independent_test) 
#combining to check predicted values with dependent variables in test data
checkData=cbind(random_fp_500, dependent_test) 
#checkData
#creating confusion matrix
random_confM_500=table(random_fp_500,dependent_test) 
random_confM_500 
#finding accuracy
sum(diag(random_confM_500))/sum(random_confM_500) 
accuracy_500<-sum(diag(random_confM_500))/sum(random_confM_500)
#plotting the random forest 
rfProb_500=predict(random_f_500,independent_test,type="prob")
r_500<- roc(dependent_test,rfProb_500[,1])
auc_500<-auc(r_500)
x_500<-paste("Random Forest : ntree = 500",",","AUC= ",round(auc_500,6),",","Accuracy=",round(accuracy_500,5))
plot(r_500,main=x_500)


#-------------------------------------------------------------------------------------------------
#Random forest for n=100
#creating random forest 
random_f_100=randomForest(independent_train, dependent_train, ntree=100) 
#predicting test data by passing through random forest
random_fp_100=predict(random_f_100,independent_test) 
#combining to check predicted values with dependent variables in test data
checkData=cbind(random_fp_100, dependent_test) 
#checkData
#creating confusion matrix
random_confM_100=table(random_fp_100,dependent_test) 
random_confM_100 
#finding accuracy
sum(diag(random_confM_100))/sum(random_confM_100) 
accuracy_100<-sum(diag(random_confM_100))/sum(random_confM_100)
#plotting the random forest 
rfProb_100=predict(random_f_100,independent_test,type="prob")
r_100<- roc(dependent_test,rfProb_100[,1])
auc_100<-auc(r_100)
x_100<-paste("Random Forest : ntree = 100",",","AUC= ",round(auc_100,6),",","Accuracy=",round(accuracy_100,5))
plot(r_100,main=x_100)

#-------------------------------------------------------------------------------------------------
#Random forest for n=50
#creating random forest 
random_f_50=randomForest(independent_train, dependent_train, ntree=50) 
#predicting test data by passing through random forest
random_fp_50=predict(random_f_50,independent_test) 
#combining to check predicted values with dependent variables in test data
checkData=cbind(random_fp_50, dependent_test) 
#checkData
#creating confusion matrix
random_confM_50=table(random_fp_50,dependent_test) 
random_confM_50 
#finding accuracy
sum(diag(random_confM_50))/sum(random_confM_50) 
accuracy_50<-sum(diag(random_confM_50))/sum(random_confM_50)
#plotting the random forest 
rfProb_50=predict(random_f_50,independent_test,type="prob")
r_50<- roc(dependent_test,rfProb_50[,1])
auc_50<-auc(r_50)
x_50<-paste("Random Forest : ntree = 50",",","AUC= ",round(auc_50,6),",","Accuracy=",round(accuracy_50,5))
plot(r_50,main=x_50)

#-------------------------------------------------------------------------------------------------
#Random forest for n=10
#creating random forest 
random_f_10=randomForest(independent_train, dependent_train, ntree=10) 
#predicting test data by passing through random forest
random_fp_10=predict(random_f_10,independent_test) 
#combining to check predicted values with dependent variables in test data
checkData=cbind(random_fp_10, dependent_test) 
#checkData
#creating confusion matrix
random_confM_10=table(random_fp_10,dependent_test) 
random_confM_10 
#finding accuracy
sum(diag(random_confM_10))/sum(random_confM_10) 
accuracy_10<-sum(diag(random_confM_10))/sum(random_confM_10)
#plotting the random forest 
rfProb_10=predict(random_f_10,independent_test,type="prob")
r_10<- roc(dependent_test,rfProb_10[,1])
auc_10<-auc(r_10)
x_10<-paste("Random Forest : ntree = 10",",","AUC= ",round(auc_10,6),",","Accuracy=",round(accuracy_10,5))
plot(r_10,main=x_10)

#-------------------------------------------------------------------------------------------------
#Random Forest K-fold cross validation
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)

#Setup control for cross validation with 10 folds and 3 iterations
ctrl = trainControl(method="repeatedcv", number = 10, repeats = 3,savePredictions = T)
#Using random forest method and k-fold cross validation with Accuracy as metric and ntree = 500
randomForest_default = train(carData[,1:6], carData[,7],method = "rf",ntree=500,metric = "Accuracy", trControl = ctrl)
confusionMatrix(randomForest_default)
print(randomForest_default)
#Print important variables
varImp(randomForest_default)
#Plot important variables as graph
ggplot(varImp(randomForest_default))
