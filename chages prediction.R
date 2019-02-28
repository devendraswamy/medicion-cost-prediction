#Objective:Hi! Welcome. The data comes from 'Medical Cost Personal Datasets'.
##Here, I am interested in finding any correlation of medical charges with features listed in the given dataset and i excited to see the results.

#remove the working space
rm(list = ls(all=T))

#check the current working directory
getwd()

#set working directory
setwd("C:/Users/sharathm/Desktop/projects/medicine cost predection")

#read the dataset(.csv file)
cost=read.csv("insurance.csv",header = T)

#check for duplicates , if dup present omit that values
cost1=anyDuplicated(cost)

#here 582 duplicates are present in dataset , so omit duplicates
cost2=cost[!duplicated(cost),]

#check the NA values
sum(is.na(cost2))

#check the attributes structure
str(cost2)

#check summary
summary(cost2)

# Splits the plotting pane 2*2
par(mfrow = c(1,2)) 

#visulaise the relation between target varible to independent varibles
plot(cost2$age, cost2$charges,col="green", xlab = "age", ylab = "charges", main = "age vs charges")

plot(cost2$bmi, cost2$charges,col="red", xlab = "bmi", ylab = "charges", main = "bmi vs charges")

#install caret library for splitting the data
library(caret)

#set the set.seed for picking random observations from dataset
set.seed(123) 

#split the data for training and validation
trainrows<-createDataPartition(cost2$charges,p=0.7,list = F)
train<-cost2[trainrows,]
test<-cost2[-trainrows,]

#build the ,odel
model1 <- lm(charges~., data = train)

#chech the summary of model output
summary(model1)

# Splits the plotting pane 2*2
par(mfrow = c(2,2))
# Plot the residual plots
plot(model1) 

#intall "MASS" package  
library(MASS)

#perform the "stepAIC" for getting most significant varibles 
step1=stepAIC(model1,direction = "both")

#perform the model again with least stepAIC varibles
model2 <- lm(charges~age + bmi + children + smoker, data = train)

#check summary
summary(model2)

# Splits the plotting pane 2*2
par(mfrow = c(2,2))

# Plot the residual plots
plot(model2)


library(DMwR)

#predictions 
pred=predict(model2,train)
pred

regr.eval(train$charges,model2$fitted.values)

#model building on test data
model3=lm(charges~., data = test)

# check summary
summary(model3)

## Splits the plotting pane 2*2
par(mfrow = c(2,2))

#Plot the residual plots
plot(model2)

#perform STEPAIC for getting singificant values
step2=stepAIC(model3,direction = "both")

# build model again with given STEPAIC varibles
model4=lm(charges~age + bmi + children + smoker, data = test)

#check summary
summary(model4)

## Splits the plotting pane 2*2
par(mfrow = c(2,2))

#Plot the residual plots
plot(model4)

#predictions
pred1=predict(model4,test)
pred

regr.eval(test$charges,model4$fitted.values)


