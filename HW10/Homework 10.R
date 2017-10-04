# The dataset Cancer.csv contains data about the tumor cells
# for 100 patients. The independent variables are different
# characteristics of the tumor cells. The dependant variable
# is diagosis_result, which has two classes: B and M (benign
# and malignant). Our goal is to create the model to identify
# whether the patient has type B or type M cancer.

# 1. Load data. Prepare training and testing sets (80%/20%).
# Make all the required preprocessing for building the
# neuralnetwork model. (10)
library(caret)
cancer<-read.csv("cancer.csv")

##As neuralnetwork need binary dependent variable:
## Dummy variable transform factors into binary
dmy1 <- dummyVars(" ~diagnosis_result ", data = cancer, fullRank=T)
trsf1 <- data.frame(predict(dmy1, newdata = cancer))
cancer$diagnosis_result<-trsf1$diagnosis_result.M

set.seed(2016)
TrainIndex<-createDataPartition(cancer$diagnosis_result, p=0.8, list=FALSE)
train<-cancer[TrainIndex,]
test<-cancer[-TrainIndex,]



# 2. Build the neural network model. Play with the parameters
# in order to get as high accuracy as possible. Report the
# final accuracy. Plot the model, make sure you do not have
# overlapping objects in the plot.  (20)
library(neuralnet)
paste( names(train[,-ncol(train)]), collapse="+")
fm<-formula(diagnosis_result~radius+texture+perimeter+area+smoothness+compactness+symmetry+fractal_dimension)

##Scaling the data
scaled<-scale(train[,-ncol(train)])
scaled<-data.frame(diagnosis_result=train[,ncol(train)],as.data.frame(scaled))
scaledTest<-scale(test[,-ncol(test)])

set.seed(2016)
model<-neuralnet(fm, data=scaled, hidden=2, linear.output = F, err.fct="ce", rep=4,
                 stepmax =1000)
plot(model, arrow.length = 0.2) 

set.seed(2016)
cmtp<-compute(model, scaledTest, rep=1)

library(ROCR)
ptest<-ROCR::prediction(cmtp$net.result[,1], test$diagnosis_result)
perf<-ROCR::performance(ptest,"tpr", "fpr")
performance(ptest, "auc")@y.values
plot(perf, colorize=T)
##After playing with parameters I got this one as the best:
#0.8452380952

# 3. Use one of the classification model that we covered so
# far. Cross validate it using caret. Out of the two 'cross
# validated' models, which one is doing better? (20)
scaled$diagnosis_result<-as.factor(scaled$diagnosis_result)
train$diagnosis_result<-as.factor(train$diagnosis_result)
test$diagnosis_result<-as.factor(test$diagnosis_result)

##Not this one
##cross validation for Neural Network
##set.seed(2016)
##control <- trainControl(method="repeatedcv", number=10, repeats=3)
##neuralnetcv = train(diagnosis_result~., data = scaled, 
 ##             method="neuralnet",
  ##            trControl=control)
##neuralnetcv$bestTune

library(randomForest)

set.seed(2016)
model<-randomForest(diagnosis_result~., data=train, ntree=50)

set.seed(2016)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(2016)
metric <- "Accuracy"
rfcv <- train(diagnosis_result~., data=train, method="rf", metric=metric, 
                       trControl=control,ntree=200)
print(rfcv)
plot(rfcv)
rfcv$bestTune

set.seed(2016)
model<-randomForest(diagnosis_result~., mtry=2, data=train, ntree=200)

set.seed(2016)
pr<-predict(model, newdata =test)

confusionMatrix(pr, test$diagnosis_result, positive="1")
plot(model)
#Accuracy : 0.7

pr2<-predict(model, newdata =test, type="prob")
library(ROCR)
ptest<-ROCR::prediction(pr2[,2], test$diagnosis_result)
perf<-ROCR::performance(ptest,"tpr", "fpr")
performance(ptest, "auc")@y.values
plot(perf, colorize=T)
##AUC: 0.869047619

##Since we need to compare, so we need both to be AUC,
##After cross validation, Random Forest gave a little better results



# BONUS Find data with at least one catrgorical independent
# variable and build a neural network on it. What are the
# AUC, accuracy, sensitivity and specificity of your model?
# Explain their meanings.  (10)

student<-read.csv("fl_student_survey2.csv")
summary(student)

dmy1 <- dummyVars(" ~affirmative_action_support+abortion_legalize+
                  vegetarian+gender", data = student, fullRank=T)
trsf1 <- data.frame(predict(dmy1, newdata = student))
scaled<-student

scaled<-scaled[,-c(2,12,15,16)]
scaled<-scale(scaled[,])
student2 = cbind(scaled, trsf1)

set.seed(2016)
TrainIndex<-createDataPartition(student2$gender, p=0.8, list=FALSE)
train_student<-student2[TrainIndex,]
test_student<-student2[-TrainIndex,]
test_student_use<-test_student[,-ncol(test_student)]

library(neuralnet)
paste( names(train_student[,-ncol(train_student)]), collapse="+")
fm<-formula(gender.m~subject+age+high_sch_GPA+college_GPA+distance_home+distance_residence+TV+sports+newspapers+AIDS+political_ideology+religiosity+affirmative_action_support.y+abortion_legalize.y+vegetarian.y)

set.seed(2016)
model_student<-neuralnet(fm, data=train_student, hidden=2, linear.output = F, err.fct="ce", rep=4,
                 stepmax =10000)
plot(model_student, arrow.length = 0.2) 

set.seed(2016)
cmtp<-compute(model_student, test_student_use, rep=1)

library(ROCR)
ptest<-ROCR::prediction(cmtp$net.result[,1], test_student$gender.m)
perf<-ROCR::performance(ptest,"tpr", "fpr")
performance(ptest, "auc")@y.values
plot(perf, colorize=T)
#AUC: 0.5714285714
# Auc is the area under curve of true positive rate and false positive rate plot, which is 
#is their plot under different threshold for classification of probabilities vector


##Label if bigger than 0.5 then male, and otherwise female
pred_labels = ifelse(cmtp$net.result[, 1] > 0.5, 1, 0)

confusionMatrix(pred_labels, test_student$gender.m, positive = "1")
#Accuracy : 0.5454545

# Accuracy = (True positive+true negative)/all, It measures how well the model predicted classes

# Sensitivity : 0.7500000  
# Specificity : 0.4285714  

#Sensitifity is the ability of the test to correctly identify male gender
#Specificity is the ability of the test to correctly identify female gender

##Here the sensitivity is not bad, so we have good predictins for male
#and specificity is low, the model is not able to make good predictions
#of female gender