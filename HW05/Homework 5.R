# Homework 5- KNN
# The dataset Cancer.csv contains data about the tumor cells for 
# 100 patients. The independent variables are different
# characteristics of the tumor cells. The dependant variable
# is diagosis_result, which has two classes: B and M (benign and
# malignant). Our goal is to create the model to identify whether
# the patient has type B or type M cancer. 
# Attention!!!- while solving the problems, make sure to set the 
# seed to 2016 every time when you are using a function that
# does any random operation. 

#1. Load the data to R and scale it. (5.5)
cancer<-read.csv("Cancer.csv")

#2. Set seed to 2016. Create testing and training sets. 
# 80% of data should go to train, the rest to test.
# Make sure the proportions of the categorical variable are 
# not changed (some small variation is ok). (5.5)
library(ggplot2)
library(caret)
library(class)
scaled<-scale(cancer[,1:8])
Data<-as.data.frame(cbind(scaled,
                          Diagnosis_result=cancer$diagnosis_result))
Data$Diagnosis_result<-factor(Data$Diagnosis_result, levels=c(1,2), labels=c("B","M"))
set.seed(2016)
trainIndex <- createDataPartition(Data$Diagnosis_result, p = .8, list = FALSE)

train <- Data[ trainIndex,]
test  <- Data[-trainIndex,]
#For checking the proportions of the categorical variable: 
prop.table(table(train$Diagnosis_result))
prop.table(table(test$Diagnosis_result))
prop.table(table(Data$Diagnosis_result))


#3. Using library caret, identify the optimal number of K's.
# Do repeated k-fold cross validation.
# Use the accuracy for defining which number is the best.
# Also, do not forget to set seed to 2016. (5.5)
#install.packages("caTools")
library(caTools)
library(caret)
library(class)
set.seed(2016)
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3) 
set.seed(2016)
knnfit<-train(Diagnosis_result~., data=train, method="knn", trControl=ctrl, tuneLength=12)
knnfit$results
knnfit$bestTune
plot(knnfit)
#By using Accuracy i found that k=7 is the best number.

#4. Now do the same analysis, but use AUC (area under the curve)
# for identifying which number of K's is the best. 
# Is that number the same as in the prvious case?
# Again, set the seed to 2016. (5.5)
set.seed(2016)
ctrl1 <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE,
                     summaryFunction = twoClassSummary) 
set.seed(2016)
knnFit1 <- train(Diagnosis_result ~ ., data = train, method = "knn", trControl = ctrl1, 
                tuneGrid=expand.grid(k=5:15))

plot(knnFit1)
knnFit1$results
knnFit1$bestTune

knn_class <- predict(knnFit1, newdata = test)
confusionMatrix(knn_class, test$Diagnosis_result, positive="M")
knn_probs <- predict(knnFit1, newdata=test, type="prob")
#install.packages("ROCR")
library(ROCR)
P_Test <- prediction(knn_probs[,2], test$Diagnosis_result) 
perf <- performance(P_Test,"tpr","fpr")
plot(perf)
performance(P_Test, "auc")@y.values


#This time i got that knn=15 is the best number
#The two different methods didn't give the same result

#5. Based on the results in the previous two problems, choose
# the most optimal number of K's. Run Kmeans classification
# using the library class. (5.5)
#The most optimal number of k is 7, because when i did the analysis using AUC, the 
#different in ROC between 7 and the best number 15 is only 0.0019444, wherease 
#while analysing with using caret, the different in accuracy between the best
#number knn 7 and the best number knn=15 that i got for AUC method is: 0.0282407
#Since the difference between k=7 and k=15 using AUC is small, so i can take k=7
#so i use knn=7
library(class)
set.seed(2016)
Model1<-knn(train[,1:8], test[,1:8], train$Diagnosis_result, k=7, prob = T)
Model1
attr(Model1,"prob")

#6. What is the accuracy of that model? Use the confusionMatrix
# to get the number. The positive class is M. (5.5)
confusionMatrix(Model1, test$Diagnosis_result, positive="M")
#Accuracy : 0.8947


#7. Look at the sensitivity and specificty sof the models. 
# Explain their meanings within the context of the problem.
# How will you deal with the classification threshold to balance
# the risk of having false negative and false positive results?
# Explain. (5.5)
table(test$Diagnosis_result, Model1)
#Sensitivity : 0.9167         
#Specificity : 0.8571
#Sensitifity is the ability of the test to correctly identify those patients with
#the diagosis result malignant.
#Specificity is the ability of the test to correctly identify those patients with
#the diagosis result benign
#Below i explain what does maignant and benign tumors mean, please look below :) 
#Models accuracy can change if we change the cut off value

#Because in malignant tumor we have that the Cells in these tumours can invade nearby
#tissues and spread to other parts of the body (spread to other organs and bones where they 
#can continue to grow and form another tumour at that site)
#So if we had a malignant tumor and we predicted it to be as benign tumor
#we may loose the patient as the doctors won't act quickly
#Wherease if the patient has a benign tumor and we predicted it to be manignent tumor
#we will make that patient as a priority to the doctors, so that that patient
#will enter surgery as fast as possible (which is beneffical to the patient :D)
#In summary we need to increase the accuracy of correctly identify those patients with
#those patients with diagosis result malignant.
#the diagosis result malignant.
#So we need to consider a treshold which will increase sensitivity and
#decrease specificity.
#We should lower the threshold for benign cases, for example take it 0.4 or 0.3 
#so if we set the probability of having malignant cancer is larger than 0.4, 
#we should predict it as malignant



#8. Now solve the same classification problem using logistic
# regression. What is the accuracy of your model? (5.5)
Model2<-glm(Diagnosis_result~., data=train, family ="binomial" )
PredTest<-predict(Model2,newdata = test, type="response")
PredTest<-ifelse(PredTest>0.50, "M","B")
#table(PredTest, test$Diagnosis_result, dnn = c("M","B"))
confusionMatrix(PredTest, test$Diagnosis_result, positive="M")
# Accuracy : 0.8421 

#9. Which classsification model works better? 
# Compare several accuracy measures from confusion matrix and 
# give your thoughts. (6)

#knn classification works better as the accuracy is: 0.8947
#wherease the accuracy using logistic regression is: 0.8421   
#Let's compare Sensitivity:
#Using Knn classification we are getting: 0.9167
#Wherease using logistic regression we get: 0.8333
#This means with using kmeans classification we more correctly identify that the person has 
#diagosis result malignant than using logistic regression  
#Let's compare Specificity:
#Using Knn classification we are getting: 0.8571 
#Wherease using logistic regression we get: 0.8571

#My thoughts are:
#With doing Logistic regression we see that we don't have a lot of significant variables
#which means we should modify our model and do some assumptions
#wherease using Knn classification it is more automated based on many trials



