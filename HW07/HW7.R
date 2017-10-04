# The dataset credit contains information about the bank customers.
# It is used to identify whether a customer who applied for the
# loan will default or not.

#1. Load credit data into R. Make sure the categorical
# variables are factored. Create testing and training 
# datasets, so that 80% of data goes to train and the rest
# goes to test.Make sure the proportions of the dependent 
# variable are fixed. Set the seed to 2016. (7)
credit<-read.csv("Credit.csv")
credit$default<-factor(credit$default, levels=c(0,1), labels=c("No", "Yes"))
credit$ed<-factor(credit$ed, levels=c(1,2,3,4,5), labels=c("didn't complete high school","high school degree","college degree","undergraduate", "postgraduate"))
library(lattice)
library(ggplot2)
library(caret)
set.seed(2016)
trainIndex<-createDataPartition(credit$default, p=.8, list=F)
train<-credit[trainIndex,]
test<-credit[-trainIndex,]
#Check the proportions
prop.table(table(credit$default) )
prop.table(table(train$default) )
prop.table(table(test$default) )

#2. Create a naive bayes model on the dataset. Set 
# Laplace equal to 1. What is the accuracy of the model? (7)
library(e1071)
model<-naiveBayes(default~., data=train, laplace=1)

#model$tables
#model$apriori
pred<-predict(model, newdata=test)
confusionMatrix(pred, test$default)
# Accuracy : 0.741 
#Sensitivity : 0.9029          
#Specificity : 0.2778 


#3. Plot the ROC curve, make sure you have the colors of the
# thresholds on the curve. Give explanation to the coloring of
# the curve: what does it show?? What is the AUC? (7)
library(ROCR)
pred1<-predict(model, newdata=test, type="raw")
pred_test<-prediction(pred1[,2], test$default,label.ordering =c("No", "Yes"))

perf<-performance(pred_test,"tpr","fpr")
plot(perf, colorize=T)

##The color at each point on the ROC curve represents a sensitivity/specificity pair corresponding 
#to a particular decision threshold
#The closer the ROC curve is to the upper left corner, the higher will be the overall
#accuracy
#What we are interested in, is to make the true positive rate as high as possibly 
#and decrease false positive rate
#This can happen when treshold is almos 0.2 

performance(pred_test, "auc")@y.values
#AUC:0.7208738

#4. Given that someone defaulted, what is the probability that
# he/she has postgarduate degree? (7)
model<-naiveBayes(default~ed, data=train, laplace=1)
model$tables
#Given that someone defaulted,the probability that he/she has postgarduate
#degree is 0.01315789

#5.Take any of the classification methods that we studied so
# far and build a model using it. Compare that model with the
# Naive Bayes model. Which one does better? Comment. (7)
library(rpart)
fit<-rpart(default~., data=train, method="class")
library(rpart.plot)
library(rattle)
prp(fit, type=1, extra=4, faclen=0,tweak=1.2, main="Decision Tree for voting")

pred_class<-predict(fit, test,type="class")
table( Prediction=pred_class,Actual=test$default)
confusionMatrix(pred_class,test$default, positive="Yes")
#Accuracy : 0.7842
#Sensitivity : 0.6667          
#Specificity : 0.8252 

#Classification using Decision Tree method is doing better, as the accuracy is higher
#Than the accuracy using naive bayes method.
##The Naive Bayes is based on assumption. The assumption is that the variables are
#independent, this may cause the loss of accuracy
#The Sensitivity using Decision Tree method, is not doing better than the Sensitivity of
#using Naive Bayes, however the Specificity is doing much better using the decision tree.

#6. Load the scoring datset into R. Our goal will be to give
# credit scores (defualting probabilities) to the potential
# customers. Predict the scores with the Naive Bayes model. (8)
scoring<-read.csv("Scoring.csv")
scoring$ed<-factor(scoring$ed, levels=c(1,2,3,4,5), labels=c("didn't complete high school","high school degree","college degree","undergraduate", "postgraduate"))

model<-naiveBayes(default~., data=train, laplace=1)
pred1<-predict(model, newdata=scoring, type="raw")
pred<-predict(model, newdata=scoring)
pred1
scoring$default<-pred
scoring$default_ProbNO<-pred1[,1]
scoring$default_ProbYES<-pred1[,2]
table(scoring$default)

#7. Identify the top 25% of customers that are least risky.
# Describe them with the variables you have in the scoring dataset.(7) 
###Least risky= Default NO
least_Risky<-scoring[scoring$default=="No",]
##Subset the highest 38 costumers. since we need 25% of all customers and we 
#have in total 150 costumers so 150*25 /100 = 37.5 , i will take 38

#I found an interesting package that will sort my data, according to one variable
#It will sort in increasing order, and by putting '-' it will sort by decreasing order
#install.packages("plyr")
library(plyr)
least_Risky <- arrange(least_Risky,-default_ProbNO )
least_Risky <- least_Risky[1:38,]
summary(least_Risky)
hist(scoring$age)
hist(least_Risky$age)
mean(least_Risky$age)
plot(least_Risky$age, least_Risky$income)
hist(least_Risky$income)
mean(least_Risky$income)

barplot(table(least_Risky$ed), main="Least Risky costumers")
table(least_Risky$ed)
#Those are the costumers that 70% of them, their education is didn't complete high school,
#and then 23% have high school degree
#Mostly, their age is between 35 and 40
#Mostly, their average income is 60.60526$ 
mean(least_Risky$address)
mean(least_Risky$employ)
#In average, they have been working in their current position for 16 years
#and in average they have the same address for 15 years
hist(least_Risky$creddebt)
mean(least_Risky$creddebt)
#The average of Least risky costumers credit card debt in thousands is 1 thousand dollar

# Bonus point
#8. Compare top 25% of risky customers (Quartile 4) with bottom 25% of risky customers (Quartile 1). 
# What are the main differences you see? Generate 1-2 tables and graphs for the analysis. (10 points)
###risky=Default=Yes
#Most_Risky<-scoring[scoring$default=="No",]
Most_Risky <- arrange(scoring,scoring$default_ProbNO )
Most_Risky <- Most_Risky[1:38,]
summary(Most_Risky)
summary(least_Risky)

table(least_Risky$ed)
table(Most_Risky$ed)
par(mfrow=c(1,2))
barplot(table(least_Risky$ed), main="Least Risky costumers")
barplot(table(Most_Risky$ed), main="Most Risky costumers" )
##With looking to the barplot and table of Least Risky people, most of them 70% didn't complete high school 
#and small amount 23% have high school degree
##Wherease looking to the barplot and table Most Risky people, 39% didn't complete high school
#and 31% have  high school degree , and 15% have college degree, and 13% have undergraduate degree
#This means that the level of education plays a  role in defaulting


age_income_least_risky<-aggregate(least_Risky[,c("age", "income")], by=list(least_Risky$ed), FUN="mean", na.rm=T)
age_income_most_risky<-aggregate(Most_Risky[,c("age", "income")], by=list(Most_Risky$ed), FUN="mean", na.rm=T)

age_income_least_risky
age_income_most_risky

par(mfrow=c(1,2)) 
hist(least_Risky$income)
hist(Most_Risky$income)
mean(least_Risky$income)
mean(Most_Risky$income)
#The average income for most risky costumers is higher than the average income of least risky costumers
#This is interesting for me, as i tought people with low income will tends more to default,
#but the analysis says that the higher income costumers tends more to default the loan :D
#I can give some assumptions to this phenomena, costumers with high incomes, tend to get
#more expensive stuff with thinking that they can afford anything, BUT SURPRISE they
#can't :D 

par(mfrow=c(1,2)) 
hist(least_Risky$creddebt)
hist(Most_Risky$creddebt)
mean(least_Risky$creddebt)
mean(Most_Risky$creddebt)
#The average of Least risky costumers credit card debt in thousands is 1 thousand dollar
#The average of Most risky costumers credit card debt in thousands is 3.9 thousand dollars
#The Most risky costumers debt is almost 4 times the least risky costumers debt, this somehow explains
#why list risky people doesn't tend to do default loan, as they can only spend one thousand
#dollar per month and they can afford that.


par(mfrow=c(1,2)) 
hist(least_Risky$employ)
hist(Most_Risky$employ)
mean(least_Risky$employ)
mean(Most_Risky$employ)
#Least risky costumers in average have been 16 years in the same employee position
#wherease most risky costumers in average have been 8 years in the same employee position
#This also plays a big role in determining the defaulting on loan, as the stable work is 
#as people don't default on loan (maybe they are afraid of loosing their jobs, or they have
#a stable work and they can afford what they buy)