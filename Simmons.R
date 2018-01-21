library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(stringr)
library(car)
library(ROCR)
library(MASS)
library(gmodels)
library(dummies)
library(Hmisc)

Simmons_Cust_data <- read.csv("Logit-Simmons.csv")
View(Simmons_Cust_data)
str(Simmons_Cust_data)
describe(Simmons_Cust_data)
#training data
set.seed(220)
split <- sample.split(Simmons_Cust_data, SplitRatio = 0.8)


#Information value to determine strong predictor variables

IV_test <- create_infotables(Simmons_Cust_data[,-1],y="Purchase",ncore = 2)
IV_test$Tables
IV_test$Summary


# Split data set into Training and Test Data set
train<- subset(simmons, split == TRUE)
dim(train)


test<- subset(simmons, split == FALSE)
dim(test)

# logistic Regression model for Predicting customer who is likely to spend 200$ worth of items of more

model1 <- glm(Purchase~Spending+Card, data=train, family=binomial)
summary(model1)
Vif(model1)

# Stepwise selection of variables
best_model <- step(model1,direction = "both", data=train,family=binomial)
summary(best_model)

predTrain <- predict(model1, type = "response") # in-sample accuracy
table(train$Purchase, predTrain >= 0.5)

predTest <- predict(model1, newdata = test, type = "response") # out-sample accuracy
table(test$Purchase, predTest >= 0.5)

# Model evaluation
#1  ROC Curve
ROCRpred <- prediction(predTrain, train$Purchase)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

auc.tmp <- performance(ROCRpred,"auc"); 
auc <- as.numeric(auc.tmp@y.values)
auc

#2 C-statistic

train$predicted_prob = predict(model1,  type = "response")
rcorr.cens(train$predicted_prob,train$Purchase)

test$predicted_prob = predict(model1, newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Purchase)

#KS-statistic

model_score <- prediction(train$predicted_prob,train$Purchase)
model_perf <- performance(model_score, "tpr", "fpr")
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
ks = max(ks_table)
which(ks_table == ks)
ks

model_score_test <- prediction(test$predicted_prob,test$Purchase)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks1=max(ks_table_test)
which(ks_table_test == ks1)
ks1