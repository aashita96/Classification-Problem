setwd("C:/Users/sharaa-cont/Documents/Classification Problem")
library(readxl)

#data= read_xlsx("echocardiogram_1.xlsx",sheet="echocardiogram (2)")
data=read.csv("final.csv")
str(data)

data_1 = data[,c(3:7,9,10)]
colnames(data_1)


#Split into train-test
set.seed(123)
sample = sample.split(data_1$Target,SplitRatio = 0.70)
train.data = data_1[sample == TRUE,]
test.data = data_1[sample == FALSE,]

#Logistic Regression - 1
model1 = glm(train.data$Target~.,family="binomial",train.data)
summary(model1)
as.data.frame(table(data$Target))

pred1 = predict(model1,test.data,type="response")
library("InformationValue")
optCutOff=optimalCutoff(test.data$Target,pred1)
#optCutOff=optimalCutoff(train$Default_Flag,pred_train_scale)

InformationValue::confusionMatrix(test.data$Target,pred1,threshold = optCutOff)
InformationValue::plotROC(test.data$Target,pred1)


vif(model1)
vif(model2)

#Over Sampling

#install.packages("DMwR")
library("DMwR")
data_1$Target = as.factor(data_1$Target)
data_2 = SMOTE(Target~.,data_1,perc.under = 1000, k = 5)
as.data.frame(table(data_2$Target))
as.data.frame(table(data_1$Target))

summary(data_2)
nrow(data_2)

#Split train-test
set.seed(123)
library(caTools)
sample = sample.split(data_2$Target,SplitRatio = 0.70)

train.data1 = data_2[sample == TRUE,]
test.data1 = data_2[sample == FALSE,]

nrow(train.data1)
nrow(test.data1)

#Logistic Regression - 2
model2 = glm(train.data1$Target ~ .,train.data1, family="binomial")
summary(model2)

pred2 = predict(model2,test.data1,type="response")
pred_train = predict(model2,train.data1,type= "response")

library("InformationValue")
optCutOff1=optimalCutoff(test.data1$Target,pred2)
optCutOff2=optimalCutoff(train.data1$Target,pred_train)

InformationValue::confusionMatrix(test.data1$Target,pred2,threshold = optCutOff1)
InformationValue::confusionMatrix(train.data1$Target,pred_train,threshold = optCutOff2)

InformationValue::plotROC(test.data1$Target,pred2)
plotROC(train.data1$Target, pred_train)

pred_logit = ifelse(pred_train < optCutOff2,0,1)
pred_test = ifelse(pred2 < optCutOff1,0,1)

Accuracy(pred_logit,train.data1$Target)
Sensitivity(pred_logit,train.data1$Target)
Specificity(pred_logit,train.data1$Target)

Accuracy(pred_test,test.data1$Target)
Sensitivity(pred_test,test.data1$Target)
Specificity(pred_test,test.data1$Target)




#Decision tree
library(rpart)
rpart.plot::rpart.plot(model3)
model3 = rpart(train.data1$Target ~., data = train.data1, method = 'class')
summary(model3)

pred_dt = predict(model3, test.data1, type = "class")
pred_dt_train = predict(model3,train.data1,type = "class")

table_mat = table(test.data1$Target, pred_dt)
accuracy_dt = sum(diag(table_mat)) / sum(table_mat)

library(caret)

#calculate sensitivity
senstivity_dt = sensitivity(pred_dt, test.data1$Target)
senstivity_dt_train = sensitivity(pred_dt_train, train.data1$Target)

#calculate specificity
specificity_dt = specificity(pred_dt, test.data1$Target)
specificity_dt_train = specificity(pred_dt_train, train.data1$Target)

Accuracy(pred_dt_train,train.data1$Target)
Accuracy(pred_dt, test.data1$Target)

ConfusionMatrix(pred_dt_train, train.data1$Target)
ConfusionMatrix(pred_dt,test.data1$Target)


#Random Forest
library(randomForest)
model4 = randomForest(train.data1$Target~., data = train.data1)
summary(model4)

pred_rf1 = predict(model4, test.data1)
pred_rf2 = predict(model4, train.data1)

Accuracy(pred_rf1,test.data1$Target)

Accuracy(pred_rf2,train.data1$Target)

Specificity(pred_rf2,train.data1$Target)
Specificity(pred_rf1,test.data1$Target)

Sensitivity(pred_rf2,train.data1$Target)
Sensitivity(pred_rf1,test.data1$Target)


ConfusionMatrix(pred_rf2,train.data1$Target)
ConfusionMatrix(pred_rf1,test.data1$Target)
