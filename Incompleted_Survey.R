#install.packages("caret", dependencies = c("Depends", "Suggests"))

#install.packages('ggplot2', dependencies = TRUE)

#install.packages("ModelMetrics")
#install.packages("generics")
#install.packages("gower")
#install.packages("inum")
#install.packages("varImp")

library(caret)
library(readr)
library(varImp)

#Explore data
Complete_Survey <- read_csv("CompleteResponses.csv")
attributes(Complete_Survey)
summary(Complete_Survey)
str(Complete_Survey)
hist(Complete_Survey$brand)
plot(Complete_Survey$brand, Complete_Survey$salary)

#Pre-processing 
is.na(Complete_Survey)
sum(is.na.data.frame(Complete_Survey)) #no NAs in the data set 

Complete_Survey$elevel<-as.numeric(Complete_Survey$elevel)
Complete_Survey$car<-as.numeric(Complete_Survey$car)
Complete_Survey$zipcode<-as.numeric(Complete_Survey$zipcode)
Complete_Survey$brand<-as.numeric(Complete_Survey$brand)
str(Complete_Survey)

Correlation_Matrix = cor(Complete_Survey)
Correlation_Matrix

Complete_Survey$elevel<-as.factor(Complete_Survey$elevel)
Complete_Survey$car<-as.factor(Complete_Survey$car)
Complete_Survey$zipcode<-as.factor(Complete_Survey$zipcode)
Complete_Survey$brand<-as.factor(Complete_Survey$brand)
str(Complete_Survey)

plot(Complete_Survey$brand, Complete_Survey$salary) 
plot(Complete_Survey$brand, Complete_Survey$age)
plot(Complete_Survey$brand, Complete_Survey$elevel)
plot(Complete_Survey$brand, Complete_Survey$credit) #looking for outliers 

#Analyse 
#Training and Test 

# 1.Decision Tree c5.0 

set.seed(998)

inTraining <- createDataPartition(Complete_Survey$brand, p = .75, list = FALSE)
DTtraining <- Complete_Survey[inTraining,]
DTtesting <- Complete_Survey[-inTraining,]

library(C50)
vars = c("salary", "age", "elevel", "car", "zipcode", "credit")
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
tree_mod <- train(brand~., data = DTtraining, mode =C5.0(x = DTtraining[, vars], y = DTtraining$brand), trControl=fitControl, tuneLength = 2)
tree_mod
plot(tree_mod)

varImp(tree_mod)

DTtesting$predicted_brand <- predict(tree_mod, DTtesting)
postResample(DTtesting$brand, DTtesting$predicted_brand)


Prediction_for_survey_withDT <- predict(tree_mod, inComplete_Survey)
Prediction_for_survey_withDT

confusionMatrix(DTtesting$brand, DTtesting$predicted_brand, positive = "1" )

inComplete_Survey$brand = Prediction_for_survey_withDT
inComplete_Survey$DTPrediction = NULL

write.csv(Prediction_for_survey_withDT, file = "PredictionForSurveywithDT.csv")


# 2.Automatic grid - Random Forest
set.seed(998)

inTraining <- createDataPartition(Complete_Survey$brand, p = .75, list = FALSE)
training <- Complete_Survey[inTraining,]
testing <- Complete_Survey[-inTraining,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

rfFit1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)

rfFit1


# 3.Manual tuning - Random Forest
library(randomForest)
set.seed(123)

inTrainingRF <- createDataPartition(Complete_Survey$brand, p = .75, list = FALSE)
RFtraining <- Complete_Survey[inTrainingRF,]
RFtesting <- Complete_Survey[-inTrainingRF,]

RFfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #10 fold cross validation

rfGrid <- expand.grid(mtry=c(1,2,3,4,5)) #dataframe for manual tuning of mtry 

system.time(rfFitm1 <- train(brand~., data = RFtraining, method = "rf", trControl=RFfitControl, tuneGrid=rfGrid))

rfFitm1

varImp(rfFitm1)

RFtesting$predicted_brand = predict(rfFitm1, RFtesting)
postResample(RFtesting$brand, RFtesting$predicted_brand)

Prediction_for_survey_withRF <- predict(rfFitm1, inComplete_Survey)
Prediction_for_survey_withRF

confusionMatrix(RFtesting$brand, RFtesting$predicted_brand, positive = "1" )


#Prediction 

inComplete_Survey <- read_csv("SurveyIncomplete.csv")

inComplete_Survey$elevel<-as.factor(inComplete_Survey$elevel)
inComplete_Survey$car<-as.factor(inComplete_Survey$car)
inComplete_Survey$zipcode<-as.factor(inComplete_Survey$zipcode)
inComplete_Survey$brand<-as.factor(inComplete_Survey$brand)

write.csv(Prediction_for_survey, file = "PredictionForSurvey.csv")

#Prediction assesment 
defaultSummary(data, lev = NULL, model = NULL)
twoClassSummary(data, lev = NULL, model = NULL)
mnLogLoss(data, lev = NULL, model = NULL)
multiClassSummary(data, lev = NULL, model = NULL)
prSummary(data, lev = NULL, model = NULL)

#Analyses
summary(Complete_Survey$age) 

#install.packages("mosaic")
#install.packages("backports")
#install.packages("promises")
#install.packages("mime")
library(mosaic)
tally(~age, Complete_Survey, margins = TRUE)
tally(~age, Complete_Survey, margins = TRUE, format = "perc")
tally(~age, Complete_Survey, margins = TRUE)
age_brand = tally(~age+brand, Complete_Survey, margins = TRUE)

Complete_Survey$elevel = as.numeric(Complete_Survey$elevel)
tally(~age+brand|elevel>2, Complete_Survey, margins = TRUE, format = "perc")
tally(~zipcode+elevel, Complete_Survey, margins = TRUE, format = "perc")
tally(~brand+elevel, Complete_Survey, margins = TRUE)
tally(~brand+car, Complete_Survey, margins = TRUE, format = "perc")
age_brand = tally(~brand+age, Complete_Survey, margins = TRUE)
car_brand = tally(~brand+car, Complete_Survey, margins = TRUE)
salary_brand = tally(~brand+salary, Complete_Survey, margins = TRUE)

tally(~brand|salary =c(50000:130000), Complete_Survey, margins = TRUE, format = "perc")
tally(~salary>84950|credit>=250607, Complete_Survey, margins = TRUE, format = "perc")
tally(~car|age>=30, Complete_Survey, margins = TRUE, format = "perc")

write.csv(age_brand, file = "age_brand.csv")
write.csv(car_brand, file = "car_brand.csv")
write.csv(salary_brand, file = "salary_brand.csv")

summary(Complete_Survey$salary)
summary(Complete_Survey$credit)
summary(Complete_Survey$age)

ggdotplot(Complete_Survey, brand, salary)
library(ggplot2)
ggdotplot(Complete_Survey, brand, salary)
library(ggpubr)
install.packages("ggpubr")
ggdotplot(Complete_Survey, x= "brand", y= "salary", color = "brand", palette = "Blues", binwidth = 750)
install.packages("labeling")
