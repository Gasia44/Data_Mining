# Decision trees
# The dataset voting contains information about the voting
# patterns of Democrat and Republican parties in US Congress.
# You can find more information in the description file.

#1. Load the voting_test and voting_train datasets into R.
# Get rid of the spaces and other symbols in the column 
# names using function make.names. You shold get names like
# this: el.salvador.aid. Look at the help of the function
# for the details. (6.25)
load("voting_test.rda")
load("voting_train.rda")
?make.names
names(v_test)
names(v_test)<-make.names(names(v_test) , unique = TRUE)
names(v_test)
names(v_train)<-make.names(names(v_train) , unique = TRUE)
names(v_train)

#2. Create a decision tree with the library rpart on the 
# training set. Use the variable Class as dependent
#variable and all other variables as independent. (6.25)
library(rpart)
fit<-rpart(Class~., data=v_train, method="class")

#3. Plot the decision tree with the library prp and 
# rattle. Make sure you are getting clear and legible 
# plot without overlapping nodes. (6.25)
library(rpart.plot)
library(rattle)
prp(fit, type=1, extra=4, faclen=0,tweak=1., main="Decision Tree for voting")

fancyRpartPlot(fit, tweak=1.)
names(v_test)

#4. Make prediction on the testing set. Report the accuracy
# of your model. Take republican as a positive class. (6.25)
library(ggplot2)
library(lattice)
library(caret)
pred_class<-predict(fit, v_test,type="class")
table( Prediction=pred_class,Actual=v_test$Class)
confusionMatrix(pred_class, v_test$Class, positive="republican")
#Accuracy=0.8056 

#5. Now start playing with differnet parameters of your
# tree (cp, minbucket, minsplit). Your goal is to get a 
# better model than the first one, i.e. the accuracy
# of your model should be higher than the initial one. (6.25)

fit1<-rpart(Class~., data=v_train, method="class", minsplit=9, minbucket=6, cp=0.01)
pred_class1<-predict(fit1, v_test, type="class")
confusionMatrix(pred_class1, v_test$Class, positive="republican")
# Accuracy : 0.8056  

fit1<-rpart(Class~., data=v_train, method="class", minsplit=15, minbucket=4, cp=0.001)
pred_class1<-predict(fit1, v_test, type="class")
confusionMatrix(pred_class1, v_test$Class, positive="republican")
#Accuracy : 0.7963

fit1<-rpart(Class~., data=v_train, method="class", minsplit=5, minbucket=4, cp=0.01)
pred_class1<-predict(fit1, v_test, type="class")
confusionMatrix(pred_class1, v_test$Class, positive="republican")
#Accuracy :  0.8056 

fit1<-rpart(Class~., data=v_train, method="class", minsplit=6, minbucket=2, cp=0.001)
pred_class1<-predict(fit1, v_test, type="class")
confusionMatrix(pred_class1, v_test$Class, positive="republican")
#Accuracy : 0.8148 

#I tried many combinations, and I got Accuracy=0.8148 
#with using: minsplit=6, minbucket=2, cp=0.001
#which is higher than the accuracy of the initial model


#6. Is the second model doing better job in predicting
# affiliation for democratic or republican party? Explain.
# (6.25)
##Initial model:
  #Sensitivity : 0.7619          
  #Specificity : 0.8333 
##The second model:
  #Sensitivity : 0.7381            
  #Specificity : 0.8636

####Sensitivity is the ability of the test to correctly identify those who
#are republican 
#The second model is not doing a better job in predicting affiliation for 
#republican, as the Sensitivity for the second model is less than the initial one.
####Specificity is the ability of the test to correctly identify those who
#are democratic 
#The second model is doing a better job in predicting affiliation for 
#democratic, as the Specificity for the second model is higher than the initial one.


#7. What are the rules to classify a congressmen as a 
#democrat? (6.25)
library(rattle)
asRules(fit)
#Most important rule is Rule number 4 as it has the most coverage
#Rule number: 4 [Class=democrat cover=168 (51%) prob=0.02]
#adoption.of.the.budget.resolution=un,y
#education.spending=n,un
#This means that adoption.of.the.budget.resolution should be un or y
#And education.spending should be n or un, in order to classify a
#congressmen as a democrat
##Also using the second model fit1, I got the same summary above

#8. Using your last model, plot the ROC and calculate
# the area under the curve. Use republican as the class
# of interest. (6.25)
pred_prob<-predict(fit1,v_test, type="prob")
pred<-prediction(pred_prob[,2],v_test$Class)
perf<-performance(pred,"tpr","fpr")
performance(pred,"auc")@y.values
#the area under the curve: 0.8632756
plot(perf)

#9.BONUS! (10)
# Using library caret, come up with the most optimal
# value of cp to use it in the decision tree for  training data for voting
# problem. You need to do cross validation 
# in order to find that value. We had this kind of problem
# in KNN homework. Report your findings.

#Please wait a couple of minutes until you get the result :D
#I am testing for 1000 different cp, from range 0.0001 until 0.1 with increment 0.0001
set.seed(2016)
ctrl <- trainControl(method="repeatedcv",number=10, repeats=3) 
set.seed(2016)
try <- train(Class ~ ., data = v_train, method = "rpart",
                  trControl = ctrl, tuneGrid=expand.grid(cp=seq(0.0001,0.1,0.0001)))
try$bestTune

#I got that best cp=0.0131
#Let me see the accuracy using cp=0.0131
fit2<-rpart(Class~., data=v_train, method="class", cp=0.0131)
pred_class2<-predict(fit2, v_test, type="class")
confusionMatrix(pred_class2, v_test$Class, positive="republican")

#And here is the shocking part :(
#After doing all of those steps to get the best cp 
#The accuracy didn't change, it is the same as the initial model accuracy

#Let me determine the Initial best cp:
# Check xerror column
# It corresponds to 4 splits and cp = 0.01
printcp(fit) 
min.xerror <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
min.xerror

#Now with looking closer to the results of my experiment (Cross Validation)
try$results
# We can see that starting from cp=0.0001 until cp=0.0131
#The accuracy is the same(maximum), so taking any value of them will give
#the same results, and the initial cp=0.01 is included in that interval
#So everything is fine, I can submit my homework peacefully :)

prp(fit2, type=1, extra=4, faclen=0,tweak=1.0, main="Decision Tree for voting")
fancyRpartPlot(fit2, tweak=1.)


