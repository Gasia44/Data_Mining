# The data frame voice contains information about
# the voices of females and males. It summarizes
# several measurements for voice and the label,
# indicating the gender of the 'owner' of the
# voice.  
# Do not forget to set the seed to 2016 for
# all the functions that need random activity.  

# 1. Load voice data. Create testing and training
# sets. 80% goes to train, the rest to test. (7)
voice<-read.csv("voice.csv")
library(ggplot2)
library(lattice)
library(caret)
set.seed(2016)
TrainIndex<-createDataPartition(voice$label, p=0.8, list=FALSE)
train<-voice[TrainIndex,]
test<-voice[-TrainIndex,]
#Check the propotions:
prop.table(table(voice$label))
prop.table(table(train$label))
prop.table(table(test$label))

# 2. Create svm model, try different types of
# kernels.  Which one is giving the highest
# accuracy? Hint: Look at the help of the svm
# function to see what types of kernels are
# avilable.(7)

library(e1071)
M<-svm(label~., data=train, kernel="linear", probability=T)
pred1<-predict(M, newdata=test, probability=T)
confusionMatrix(pred1, test$label)
#Accuracy : 0.9715   

MM<-svm(label~., data=train, kernel="polynomial", probability=T)
pred2<-predict(MM, newdata=test, probability=T)
confusionMatrix(pred2, test$label)
#Accuracy : 0.962 

MMM<-svm(label~., data=train, kernel="radial", probability=T)
pred3<-predict(MMM, newdata=test, probability=T)
confusionMatrix(pred3, test$label)
#Accuracy : 0.981    

MMMM<-svm(label~., data=train, kernel="sigmoid", probability=T)
pred4<-predict(MMMM, newdata=test, probability=T)
confusionMatrix(pred4, test$label)
#Accuracy : 0.7911  

#Using kernel="radial" is giving the highest accuracy

# 3. Take the type of the kernel that gives the
# highest accuracy and proceed with it. Plot the
# ROC taking female as the class of interest. (7)
library(ROCR)

model<-svm(label~., data=train, kernel="radial", probability=T)
pred<-predict(model, newdata=test, probability=T)
confusionMatrix(pred, test$label)
#Accuracy : 0.981  
#Sensitivity : 0.9873          
#Specificity : 0.9747 

probs<-attr(pred,"probabilities")
pred_test<-prediction(probs[,2], test$label,label.ordering =c("male", "female"))

perf<-performance(pred_test,"tpr","fpr")
plot(perf)
plot(perf, colorize=T)


# 4. Using library caret, do cross validation and
# find the optimal value of cost. What is the value
# of accuracy while using that value of cost? Use the
# seed of 2016. (8)
library(lattice)
library(ggplot2)
library(caret)
set.seed(2016)
ctrl <- trainControl(method="repeatedcv",number=10, repeats=3) 
set.seed(2016)
try <- train(label~., data = train, method = "svmRadial",
             trControl = ctrl, tuneLength=10 )
try$bestTune
try$results
# Optimal value of Cost is 2.00, the value of Accuracy is 0.9813363

# 5. Take any other classification method that we
# covered during the class.  Compare it to the svm
# model in terms of accuracy.  Which one does
# better? (7)
library(class)
Model<-knn(train[,1:20],test[,1:20], train$label, k=3, prob=T)
confusionMatrix(Model, test$label)
#Accuracy : 0.7057  
#Sensitivity : 0.6867         
#Specificity : 0.7247 
#SVM does better with any type of kernel, SVM still giving better accuracy than KNN
##SVM using Radial kernel, the Specificity and Sensitivity and accuracy are better.

# Diabetes dataset contains information about
# different characteristics of the patients, as well as
# if they are diagnosed with diabetes or not.

# 6. Load diabetes.csv into R. Create testing and
# training datasets. As usual, 80% goes to train,
# the rest to test.  The proportions of variable
# Class should be maintained.  Build the lda model
# on the training set. Report the accuracy of the
# model.  Use 1 as positive class for diabetes. (7)
diabetes<-read.csv("diabetes.csv")
#diabetes$Class<-factor(diabetes$Class, levels=c(0,1), labels=c("No", "Yes"))
diabetes$Class<-factor(diabetes$Class)
set.seed(2016)
TrainIndex<-createDataPartition(diabetes$Class, p=0.8, list=FALSE)
train_d<-diabetes[TrainIndex,]
test_d<-diabetes[-TrainIndex,]
#Check the propotions:
prop.table(table(diabetes$Class))
prop.table(table(train_d$Class))
prop.table(table(test_d$Class))

library(MASS)
cor(train_d[,1:8])
#We don't have high correlation between the independent variables

shapiro.test(train_d$NTS)
shapiro.test(train_d$PGC)
shapiro.test(train_d$DBP)
shapiro.test(train_d$TSFT)
shapiro.test(train_d$INS)
shapiro.test(train_d$BMI)
shapiro.test(train_d$DPF)
shapiro.test(train_d$Age)
#For all p-value<alpha They are all normal (failed to reject the null)

##So I can comfortably use LDA :D
Model_d<-lda(Class~., data=train_d)
predict_d<-predict(Model_d, newdata=test_d)

confusionMatrix(predict_d$class,test_d$Class, positive = "1")
#Accuracy : 0.7516 

##Or other way: to see the accuracy: (It gives the same answer :) )
#ct<-table(test_d$Class, predict_d$class)
#ct
#diag(prop.table(ct, 1))
#sum(diag(prop.table(ct)))
#(86+30)/(86+30+25+12)

##Do some stuff for myself:
#library(klaR)
#artimat(Class~., data=train_d, method="lda")
##it is not plotting :(    (figure margins too large)

# 7. Using any of the classification methods that
# we covered so far, make predictions for the
# Class. Is that method doing better in comparison
# with LDA in terms of accuracy? (7)
library(rpart)
fit<-rpart(Class~., data=train_d, method="class")
library(rpart.plot)
library(rattle)
prp(fit, type=1, extra=4, faclen=0,tweak=1.5, main="Decision Tree for voting")

pred_class_Tree<-predict(fit, test_d,type="class")
table( Prediction=pred_class_Tree,Actual=test_d$Class)
confusionMatrix(pred_class_Tree,test_d$Class, positive="1")
#Accuracy : 0.7059 
#Classification using Decision Tree method is not doing better, as the accuracy is lower
#Than the accuracy using LDA method.

fit<-rpart(Class~., data=train_d, method="class", minsplit=5, minbucket=2, cp=0.02)
pred_class_Tree<-predict(fit, test_d,type="class")
table( Prediction=pred_class_Tree,Actual=test_d$Class)
confusionMatrix(pred_class_Tree,test_d$Class, positive="1")
#Accuracy : 0.7582    
##With doing some optimizations I got a better accuracy,
#This accuracy is a little bit better than LDA, and maybe with using a cross validation we will get
#a better one




# 8. Bonus 
# Find a dataset. Find something
# interesting related to that data.  Prepare
# predictive methods, do clustering, find
# interesting and meaningful patterns based on that
# data. Do not forget to upload that new data to moodle
# along with your submission. (10)

##OK OK since we are students i am interested in student's Dataset
#Here after a looong search i found this interesting student's related Dataset :D


student<-read.csv("fl_student_survey.csv")
student$subject<-NULL
cor(student$high_sch_GPA,student$college_GPA)
#So the correlation between the high school GPA and College GPA is only
#27% which means that the students that are getting high GPA in high school
#may not get high GPA in colleges

#Another interested thing is that how much watching TV or playing sports affects the GPA:
cor(student$college_GPA, student$TV)
cor(student$college_GPA, student$sports)
##Watching TV is not affecting the GPA :/ this is new
##The same is for sport

cor(student$high_sch_GPA, student$TV)
cor(student$high_sch_GPA, student$sports)
##However for High school GPA there is 26% negative correlation between watching TV and GPA
#Now make some sense I can say Watching TV is consuming of time, and affect negatively
#on the GPA (because the student is wasting his/her time watching TV instead of studying :D)
#For playing sport there is 12% negative correlation between GPA for high school
#okay in some senses it is wasting of time but because sport is good for health and brain
#so it is not affecting the GPA that much
library(ggplot2)
qplot(student$high_sch_GPA,student$TV)
mean(student$TV)

cor(student$newspapers, student$TV)
#It is normal not to have that much correlation between watching TV and reading Newspaper
#my assumption is that there are two kind of people, people read Newspaper, books or people
#watch TV :D 

#Let's factor our data:
student$political_ideology<-factor(student$political_ideology, levels=c(1,2,3,4,5,6,7), 
                                   labels=c("very liberal","liberal","slightly liberal",
                                            "moderate","slightly conservative",
                                            "conservative","very conservative"))

student$religiosity<-factor(student$religiosity, levels=c(0,1,2,3), labels=c("never", "occasionally",
                            "most weeks","every week"))




#Now let's do a Xsquared test and see if the religous affects the opinion of doing abortion:
tablet<-table(student$abortion_legalize, student$religiosity)

chisq.test(tablet)
#Since p_value is smaller than alpha(0.05) so we reject H0, and we say that the 2 variables
#are dependent

tablet
#OMG the students that never attend religious services, are with the opinion that abortion
#should be legal, and most of the students that attend religious services every week are
#against this opinion. We can put some assumptions that the religious services are putting
#the idea that abortion is against religion and the fetus is sould we should'nt kill it

#Now let's do some clustering:
#I am doing clustering based on the numeric variables:

data1<-scale(student[,2:10])
distance<-dist(data1, method="euclidean")
data2<-data.frame(Gender=student$gender, vegeterian=student$vegetarian, political_ideology=student$political_ideology,
                  abortion_legalize=student$abortion_legalize,religiosity= student$religiosity,
                  political_affiliation=student$political_affiliation,life_after_death=student$life_after_death,
                  affirmative_action_support=student$affirmative_action_support ,as.data.frame(data1))

clust<-hclust(distance)
plot(clust, hang=-1, labels=F)
rect.hclust(clust,3)
plot(clust, hang=-1, labels=F)
rect.hclust(clust,5)
plot(clust, hang=-1, labels=F)
rect.hclust(clust,7)
plot(clust, hang=-1, labels=F)
rect.hclust(clust,10)
#I Think 5 clusters is good, (although you may think it is too much but wth less clusters
#I am getting most of the data in one cluster and is against the factor variables)
#For exmample the religions that is every week and none is being in the same catagory
#and i don't want that

#However i will take 5 clusters
student$membership<-cutree(clust, k=5)
d=aggregate(student[,2:10], by= list(student$membership), FUN="mean")
#so
##Cluster4: are the students that have 4.0 GPA for high school and more than
#         average GPA for college the distance of the campus from their home town
#         is teh highest, and the distance of the classroom from their
#         current residence is average, they do a lot of sport, they don't
#         spend a lot of time on watching TV, they don't read a lot of newspaper,
#         

##Cluster2:are the students that have the minimal GPA for high school and 
#         for college, the distance of the campus from their home town
#         is teh lowest, and the distance of the classroom from their
#         current residence is the lowest, they do a lot of sport, they
#         spend average time on watching TV, they don't read a lot of newspaper,
#         

##From above i can conclude with putting some assumption that if the person comes from a long distance place
##to the classroom they may appreciate the study more and study more.


library(lattice)
library(caret)
Student1<-student
Student1$membership<-NULL
set.seed(2016)
TrainIndex<-createDataPartition(Student1$political_ideology, p=0.8, list=FALSE)
train_Student<-Student1[TrainIndex,]
test_Student<-Student1[-TrainIndex,]

#Now some predictions:
#I would like to predict the political ideology:
library(rpart)
fit<-rpart(political_ideology~., data=train_Student, method="class")
library(rpart.plot)
library(rattle)
fit<-rpart(political_ideology~., data=train_Student, method="class",cp=0.0005)

prp(fit, type=1, extra=4, faclen=0,tweak=1., main="Decision Tree for voting")

fit1<-rpart(political_ideology~., data=train_Student, method="class", minsplit=5, minbucket=20, cp=0.0000001)
pred_class1<-predict(fit1, test_Student, type="class")
confusionMatrix(pred_class1, test_Student$political_ideology)
#Ok i think this was the first time i am doing prediction for factor more than two levels
#The accuracy that i got is only 0.3333, but it is good as it is 7 level factors
##I am satisfied with what i got :D


fit1<-rpart(gender~., data=train_Student, method="class", minsplit=20, minbucket=5, cp=0.01)
pred_class1<-predict(fit1, test_Student, type="class")
confusionMatrix(pred_class1, test_Student$gender)
#I predicted The gender using Desicion Tree, and I got 77.78% Accuracy :)  
##DONE :) 
