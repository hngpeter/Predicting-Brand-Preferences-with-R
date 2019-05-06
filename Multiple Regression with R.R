#Multiple Regression 

library(readr)
library(caret)
library(varImp)
library(dplyr)

existing_products = read_csv("existingproductattributes2017.csv")
new_products = read_csv("newproductattributes2017.csv")

View(readyData)

# dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing_products)
readyData <- data.frame(predict(newDataFrame, newdata = existing_products))

newDataFrame_newpr <- dummyVars(" ~ .", data = new_products)
readyData_newpr <- data.frame(predict(newDataFrame_newpr, newdata = new_products))

#Explore data
attributes(readyData)
str(readyData)
summary(readyData)

#Pre-processing 
is.na(readyData)
anyNA(readyData)
sum(is.na.data.frame(readyData))

readyData$BestSellersRank <- NULL

corrData <- cor(readyData)
corrData
write.csv(corrData, file = "corrData.csv")

library(corrplot)
corrplot(corrData)
names(readyData)

DataToWorkWith = readyData %>% select("Volume", "x4StarReviews", "PositiveServiceReview", "x2StarReviews", "NegativeServiceReview", "ProductTypeLaptop", "ProductTypePC", "ProductTypeNetbook", "ProductTypeSmartphone")

#Slicing

inTraining <- createDataPartition(y = DataToWorkWith$Volume, p = .75, list = FALSE)
training <- DataToWorkWith[inTraining,]
testing <- DataToWorkWith[-inTraining,]

#Multiple Linear Model 

set.seed(3332)
LinearModel<- lm(Volume~ x4StarReviews +PositiveServiceReview + x2StarReviews + NegativeServiceReview + ProductTypeLaptop + ProductTypePC + ProductTypeNetbook + ProductTypeSmartphone, DataToWorkWith)
summary(LinearModel)

testing$predicted_Volume <- predict(LinearModel, testing)
postResample(testing$Volume, testing$predicted_Volume)


PC = filter(readyData_newpr, ProductTypePC == 1)
Laptop = filter(readyData_newpr, ProductTypeLaptop == 1)
Netbook = filter(readyData_newpr, ProductTypeNetbook == 1)
Smartphone = filter(readyData_newpr, ProductTypeSmartphone == 1)

predict(LinearModel, PC)
predict(LinearModel, Laptop)
predict(LinearModel, Netbook)
predict(LinearModel, Smartphone)



#SVM 
#install.packages("e1071")
library(e1071)

set.seed(3012)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
SVMGrid <- expand.grid(mtry=c(1,2,3,4,5:10))
SVM <- train(Volume~., data = training, mode ="svmLinear", preProcess = c("center","scale"), trControl=fitControl, tuneLength = 5)
SVM

testing$pred_volume_withSVM = predict(SVM, testing)
postResample(testing$Volume, testing$pred_volume_withSVM)

plot(SVM)

predict(SVM, PC)
predict(SVM, Laptop)
predict(SVM, Netbook)
predict(SVM, Smartphone)

#Random Forest
set.seed(3012)
RFfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #10 fold cross validation
rfGrid <- expand.grid(mtry=c(1,2,3,4,5)) #dataframe for manual tuning of mtry 
system.time(rfFitm1 <- train(Volume~., data = training, method = "rf", trControl=RFfitControl, tuneGrid=rfGrid))

rfFitm1

testing$predicted_volume_RF = predict(rfFitm1, testing)
postResample(testing$Volume, testing$predicted_volume_RF)

summary(DataToWorkWith)
head(DataToWorkWith)
#no need for nomarlizastion 

predict(rfFitm1, PC)
predict(rfFitm1, Laptop)
predict(rfFitm1, Netbook)
predict(rfFitm1, Smartphone)

#KNN 
#install.packages("class")
#install.packages("ISLR")
library(class)
library(ISLR)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Volume ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 10)
knnFit

testing$predicted_volume_KNN = predict(knnFit, testing)
postResample(testing$Volume, testing$predicted_volume_KNN)

predict(knnFit, PC)
predict(knnFit, Laptop)
predict(knnFit, Netbook)
predict(knnFit, Smartphone)
