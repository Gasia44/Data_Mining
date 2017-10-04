# The following dataset consist of data on football games for 11
# european countries.  It covers time from 2011-2016. The variable
# result has two arguments: HDW- Home team didnt win, HW- Homw
# team won. Most of the data describes the players strength
# averaged for the last 7 games (prior to the given game) averaged
# by the team (those are only for players who participated in the
# game).  More information can be found here-
# http://sofifa.com/player/192883.  You can find a huge amount of
# analytics done on football data
# here-https://www.kaggle.com/hugomathien/soccer variables
# 'home_team_points', 'away_team_points' show the amount of the
# points the teams have earned before the game (3 for win, 1 for
# draw, 0 for lose) Variable stage shows the Round number during
# the season. The poitive case for the model is HW (home team
# won).

# Dont forget to set the seed everytime you run randomForest.
# Divide data into training and testing set,80% goes to train.
load("Soccer.rda")
library(ggplot2)
library(lattice)
library(caret)
set.seed(2016)
TrainIndex<-createDataPartition(soccer$result1, p=0.8, list=FALSE)
train<-soccer[TrainIndex,]
test<-soccer[-TrainIndex,]


# Question 1. Do some descriptive analytics (charts, tests, etc)
# to find interesting patterns in the data (10 points)

###First analyse is that all the variables are numeric except the result1
###Second analyse is that each row represents a game and the skills of two teams: home team, and away team

library(corrplot)
par(mar=c(100,100,0,0))
###Let's do correlation analyses:
####I found how to make it works :D
par(mfrow=c(1,1))
set.seed(123)
Mat <-train[-72]
colnames(Mat)<- names(train[,-72])
Correlations <- cor(Mat)
corrplot(Correlations, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.6, tl.col = 'black',
         order = "hclust", diag = FALSE)

##Just by visual we can see that we have a variables that are highly correlated
#(Dark Blues means high correlations)

##PS when i say home variables, i mean the variables that start with
#Home word, the same for away

##With analyzing the plot, we can see that home variables are 
#highly correlated with each other, and away variables are highly
#correlated with each other
#However home and away variables are not very much correlated

#By looking to our variables we can see that we have home... and away...
#This intuitively makes us want to seperate the data
##So let's seperate the home from away:
home<-train[,c(1,3,4:37,72)]
away<-train[,c(2,3,38:72)]

##Now let's have correlation for each
par(mfrow=c(1,2))
Mat <- home[,-37]
colnames(Mat)<- names(home[,-37])
Correlations <- cor(Mat)
corrplot(Correlations, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)


Mat <- away[,-37]
colnames(Mat)<- names(away[,-37])
Correlations <- cor(Mat)
corrplot(Correlations, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)


##They are beautiful :D
#so home_potential and home_reaction are highly correlated
#We can assume that if someone has high potential, then his
#reaction would change based o his potential so they would be correlated

#Another interesting fact, is the symmety, if you looked to the two
#graphs beside each other, and didin't look to the variable names,
#you would think that they are the same 
##Both of them have the same sequence of variables regardless the beginning
##name of home or away, that's why i will introduce
#the correlation between finishing variable (regardless if it was
#home or away since it gives the same result)
#So finishing variable is highly correlated with: 
#positioning, dribbling, curve, passing, control, potential, 
#reaction, vision, crossing, long passing, kick accuracy, penanlties,
#volleys, shot power & long shots
##I can say that those are the strength skills, so if the team is
#good at shotting, this means thay are strong and they can control
#the ball so if the team is good those skills should be good too


##Everything seems good, and we notice we have only one negative
#correlation in each (orange color)
#This variables that have negative correlation are:
##stage and home_stage
##stage and away_stage

#f<-function(x,y){
#  a<-cor(x,y)
#  return (a)
#}

#Cor<-home
#for(x in 1:36){
#  f(home[,x], away[,x])
#}

##Above function calculates corresponding corelation, but i figured an easier way to do it
#so i commented above function :D

CorDataset<-cor(home[1:36], away[1:36])
##Now examining the diameter only from the correlation dataset CorDataset(without changing the order)
###Away team points and home team points have 0.7 correlation, 

####Away team potential and home team potential are only 50% correlated, this means 
#   the potention differs based on the team, and they don't increse together (not much correlated)
dev.off()
par(mfrow=c(1,2))
hist(home[,3],xlab="Potential in Home", main="Potenial")
hist(away[,3], xlab="Potential away",main="Potenial")
summary(home[,3])
summary(away[,3])
##Both of them are almost normally distributed
#   The mean, median, max, and min is almost the same in both

##With looking closely to the diagonal in the CorDataset
#We can see that the maximum correlation we get (without considering the
#stage variable) is when we have home_team point and away team point correlation=0.7
#This means that when the two teams are playing with each other, they should have 
#had almost the same result (Lik football finals: if the team one go to next stage
#and play with another won team)

##then we have the second highest correlation is between away reaction 
# and home reaction (cor =0.6264), which is logic since the reaction
#of the team differs based on the opponent reaction

#Let's have a histogram on the reaction:
dev.off()   ##to reset the par to default

par(mfrow=c(1,2))
hist(train$home_reactions, xlab="Reaction", main="Home")
hist(train$away_reactions,xlab="Reaction", main="Away")
##Both of them are normally distributed, they are almost a=identical

#Now x^2 test:
a<-chisq.test(table(train$result1, train$home_team_points))
a
## p-value < 2.2e-16  which is less than alpha so the two variables
#are not independent (they are dependent), which is normal,
#as the winning is related to the points 

##Now doing some predictions using Decision tree:

library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
fit<-rpart(result1~., data=train, method="class", minsplit=20, minbucket=5, cp=0.01)
pred_class<-predict(fit, test, type="class")
confusionMatrix(pred_class, test$result1, positive = "HW")
#I got Accuracy : 0.6092 



# Question 2. Build a
# random forest model with the package randomForest. Your goal is
# to predict the game result (variable 'result1') (15 points)
library(randomForest)
set.seed(2016)
model<-randomForest(result1~., data=train, ntree=500)

set.seed(2016)
pr<-predict(model, newdata =test)
confusionMatrix(pr, test$result1, positive="HW")
#Accuracy : 0.6386   
dev.off() 
plot(model)


# 2.1 Develop randomForest model by tunning several parameters.
# Look for package help for more info.  Explain the meaning of
# each parameter.
#??randomforest

set.seed(2016)
##I chose some parameters to do with, but i will explain the others meaning too :D
bestmtry <- tuneRF(train[,-72], train$result1, stepFactor=0.7, improve=0.01, ntree=500)

print(bestmtry)
#Best mtry=12

###mtryStart: By default is the squareroot of number of variables, we can
#             specify it which will be the starting value of mtry
###ntreeTry:how many trees should be used in tuning step
###stepFactor: It shows the inflation or deflation number that is being done from mtry
#             at each iteration
###improve: Specifies the number of improvement in OOB in order for the search to continue
###trace: If we want to print the progress of the search we put TRUE otherwise we put FALSE
###plot: To plot plot the OOB error as function of mtry  (TRUE/FALSE)
###doBest: If we want to run a forest with the optimal mtry that we got (TRUE/FALSE)




#4

# 2.2 Report on accuracy of your final chosen model (OOB
# estimate). Comment on it
set.seed(2016)
model2<-randomForest(result1~., mtry= 12, data=train, ntree=500)
set.seed(2016)
pr2<-predict(model2, newdata =test)
confusionMatrix(pr2, test$result1, positive="HW")
# Accuracy : 0.6366    
##The accuracy didn't imrove, instead it got deacreased, maybe the probelm is with 
#the number of tree, as 500 is not seems a good number for this problem, or other factors
#Tunerf should have got the best mtry which would have increased the accuracy...

# 2.3 Report AUC on testing set.
pr2<-predict(model2, newdata =test, type="prob")
library(ROCR)
ptest<-ROCR::prediction(pr2[,2], test$result1)
perf<-ROCR::performance(ptest,"tpr", "fpr")
performance(ptest, "auc")@y.values
plot(perf, colorize=T)
#AUC is 0.6859162

# 2.4 What are the most important variables?
a<-varImpPlot(model2)
#Most important variables are: (most important to less important)
#away_potential
#home_ball_control
#home_vision
#home_reactions
#away_ball_control
#home_short_passing
#away_reactions
#home_potential
#away_vision
#away_dribbling
#home_positioning
#home_long_passing
#away_short_passing
#home_dribbling



# Question 3. Use caret to train randomforest model. Think about
# the hyperparameters you can use for model tuning.  Do grid
# search.Hint: play with expand.grid parameter.  Report the best
# model. (15 points).

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(2016)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(10:13))
rf_gridsearch <- train(result1~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
##If you have patient and strong server you can run above...
#But in my case it took over 4 hours and didn't finish..
#So i decided to take 20% of the train data, and still working slow :(

TrainIndex2<-createDataPartition(soccer$result1, p=0.2, list=FALSE)
twenty_train<-soccer[TrainIndex2,]

set.seed(2016)
control <- trainControl(method="repeatedcv", number=10, repeats=2, search="grid")
set.seed(2016)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(10:13))
rf_gridsearch <- train(result1~., data=twenty_train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

##I tried different expand.grid parameter, and get best mtry is 12
##I also tried from 13 to 18 and got best mtry=18 but the accuracy of predictions
##is only 0.634. But in the case of 12 is much more.
#So best is mtry=12

set.seed(2016)
model2<-randomForest(result1~., mtry= 12, data=train, ntree=500)
set.seed(2016)
pr2<-predict(model2, newdata =test)
confusionMatrix(pr2, test$result1, positive="HW")
#Accuracy : 0.6366    

# Question 4. there is a package 'AUCRF' that uses randomForest
# but reports AUC on the OOB sets.  Use it to build randomforest
# model (10 points)
#install.packages("AUCRF")
?AUCRF
library(AUCRF)
#AUCRF is an algorithm for variable selection using Random Forest based on 
#optimizing the area-under-the ROC curve (AUC) of the Random Forest.
# Additional randomForest parameters can be included, otherwise default
# parameters of randomForest function will be used:
# fit <- AUCRF(Y~., data=exampleData, ntree=1000, nodesize=20)

train$result1<-factor(train$result1, levels=c("HW","HDW"), labels=c(1,0))
test$result1<-factor(test$result1, levels=c("HW","HDW"), labels=c(1,0))


set.seed(2016)
fit<- AUCRF(result1~., data=train, ntree=100, nodesize=20)

summary(fit)
fit$`OOB-AUCopt`
OptimalSet(fit)
fit$RFopt
#`OOB-AUCopt` 0.6950215
plot(fit, showOpt=TRUE, digits=4, maxvars=NULL)


#Performes a repeated cross validation analysis and computes the probability 
#of selection for each variable.
##Warning:::: It takes toooooo long :D 
fitCV<-AUCRFcv(fit, nCV = 5, M = 20)
data(fitCV)
summary(fitCV)
plot(fitCV)


# Bonus question (15 points). 
# The variables in the dataset are the
# same measures of home and away teams. For example
# 'away_short_passing' and 'home_short_passing' are showing how
# good are both teams in short passing. Now think what kind of
# transformations can you do with the data to decrease the number
# of variables and get better model. Report your way of thinking
# and the final model.

## My suggestion is to keep the maximum number in binary form
#For example we can keep only the home data, and if the home point
#is bigger than the away point then we put 1, otherwise 0
#In this way we will have half the number of variables
#I think this is good approach as we don't eliminate the variables by just
#throwing them, instead we are making tranformation to combine them

home<-train[,c(1,3,4:37,72)]
away<-train[,c(2,3,38:72)]

for(x in 1:36){
  for(y in 1:nrow(home))
    if(home[y,x]> away[y,x])
       {
      home[y,x]<-1
    } else{
      home[y,x]<-0
    }
}

set.seed(2016)
bestmtry <- tuneRF(home[,-37], home$result1, stepFactor=0.7, improve=0.01, ntree=500)

##now our training set is home set with half of the number of variables
set.seed(2016)
modelOne<-randomForest(result1~., mtry=1, data=home, ntree=500)
set.seed(2016)
prOne<-predict(modelOne, newdata =test)
confusionMatrix(prOne, test$result1, positive="1")
#That's bad :(, I got Accuracy : 0.454

home<-train[,c(1,3,4:37,72)]
away<-train[,c(2,3,38:72)]
#Let me try to keep only the difference between them: (with considering the logic why
#It is good :D)
for(x in 1:36){
  for(y in 1:nrow(home))
    {
      home[y,x]<-home[y,x]-away[y,x]
    }
}

set.seed(2016)
bestmtry <- tuneRF(home[,-37], home$result1, stepFactor=0.7, improve=0.01, ntree=500)

set.seed(2016)
modelOne<-randomForest(result1~., mtry=4 , data=home, ntree=500)
prOne<-predict(modelOne, newdata =test)
confusionMatrix(prOne, test$result1, positive="1")
##Accuracy : 0.546, one more time not very good accuracy :( 


home<-train[,c(1,3,4:37,72)]
away<-train[,c(2,3,38:72)]

#Let me try to keep only the absolute difference between them:
for(x in 1:36){
  for(y in 1:nrow(home))
  {
    home[y,x]<-abs(home[y,x]-away[y,x])
  }
}

set.seed(2016)
modelTwo<-randomForest(result1~., data=home, ntree=500)
prTwo<-predict(modelTwo, newdata =test)
confusionMatrix(prTwo, test$result1, positive="1")
#Accuracy : 0.546 still the same accuracy :(

home<-train[,c(1,3,4:37,72)]
away<-train[,c(2,3,38:72)]
#Let me try to keep the sum between them:
for(x in 1:36){
  for(y in 1:nrow(home))
  {
    home[y,x]<-home[y,x]+away[y,x]
  }
}


modelTwo<-randomForest(result1~., data=home, ntree=500)
prTwo<-predict(modelTwo, newdata =test)
confusionMatrix(prTwo, test$result1, positive="1")
#Accuracy : 0.546 still the same accuracy :(
##In summary: This is the Cost when we do transformation and reduce the number 
#of variables, as getting half the number of variables seems interesting and
#saving time in analyzing the data, however we need to take into account
#that the accuracy is decreasing 

###Okay i know i suggested too many stuff :D But here is my last suggestion :D
##Look by x^2 test which variable is independent from result1 and eliminate that
##I know This is not a good solution as we need to look one by one. But it should work
##It may not work if all the variables are not independent
a<-chisq.test(table(train$result1, train$home_team_points))
a
##Dependent
a<-chisq.test(table(train$result1, train$away_team_points))
a
##Dependent
......

###THE END :D
