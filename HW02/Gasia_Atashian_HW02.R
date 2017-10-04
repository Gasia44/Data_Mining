pisaTrain<-read.csv("pisa2009train.csv")
pisaTest<-read.csv("pisa2009test.csv")

#Question 1
#Number of students in the training set
nrow(pisaTrain)

#Question 2
#On pisaTrain, what is the average reading test score of males?
aggregate(pisaTrain$readingScore, by=list(pisaTrain$male), FUN="mean", na.rm=TRUE)
#the average reading test score of males: 483.5325

#Question 3
#Which variables don’t have missing data at all in the training set? Select all that apply.
summary(pisaTrain)
#grade, male, publicSchool, urban, readingscore

#Question 4
pisaTrain<-na.omit(pisaTrain)
pisaTest<-na.omit(pisaTest)
nrow(pisaTrain)

#Question 5
nrow(pisaTest)

#Question 6
summary(pisaTrain$male)
str(pisaTrain$male)
str(pisaTrain$raceeth)
str(pisaTrain$grade)
#raceeth is factorized and it is unordered since it doesn't have a priority in races
#male and grade are not factorized (not nominal)

#Question 7
#Male is unordered 
#raceeth is unordered
#grade is ordered (10 is better than 9, 9 is better than 8....)

#Question 8
str(pisaTrain)
?relevel 
levels(pisaTrain$raceeth)
pisaTrain$raceeth<-relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth<-relevel(pisaTest$raceeth,"White")
levels(pisaTrain$raceeth)
levels(pisaTest$raceeth)

modelReadTrain<-lm(readingScore~., data=pisaTrain)
summary(modelReadTrain)
#Adjusted R-squared:  0.3172 

#Question 9
predectionTraining<-predict(modelReadTrain,newdata=pisaTrain)
RMSE<-sqrt(mean((pisaTrain$readingScore-predectionTraining)^2))
RMSE
#RMSE=73.36555

#Question 10
predectionTesting<-predict(modelReadTrain,newdata=pisaTest)
RMSE<-sqrt(mean((pisaTest$readingScore-predectionTesting)^2))
RMSE
#RMSE=76.29079

#Question 11
summary(modelReadTrain)
#29.542707*2
#59.085414

#Question 13
summary(modelReadTrain)

#Question 14
baseline = mean(pisaTrain$readingScore)
baseline
#517.9629

#Question 15
SST<-sum((pisaTest$readingScore-baseline)^2)
SST
#7802354


#Question 16
summary(modelReadTrain)
#p-value: < 2.2e-16    , p-value< 0.05 that means significant

#Question 17
Flu<-read.csv("Flu.csv")
hist(Flu$ILI)

#Question 18
plot(log(Flu$ILI),Flu$Queries)

#Question 20
FluTrend1<-lm(log(ILI)~+Queries, data=Flu)
summary(FluTrend1)
#Multiple R-squared:  0.709

#Question 21
set.seed(20)
sub<-sample(nrow(Flu), ceiling(nrow(Flu)*0.75))

Train<-Flu[sub,]
Test<-Flu[-sub,]
FluTrend2<-lm(log(ILI)~+Queries, data=Train)
Prediction<-predict(FluTrend2, newdata=Test)
RMSE<-sqrt(mean((log(Test$ILI)-Prediction)^2))
RMSE
#1.685168

#Question 22
BaseLineMean<-mean(Train$ILI)
RMSE_BaseLine<- sqrt(mean((Test$ILI-BaseLineMean)^2))
RMSE_BaseLine
#1.223446


ClimateChange<-read.csv("climate_change.csv")

#Question 23
View(ClimateChange)
SubSet<-ClimateChange[ClimateChange$Year<=2006,]
View(SubSet)
Train<-SubSet
Test<-ClimateChange[ClimateChange$Year>2006,]
View(Test)
Model<-lm(Temp~+MEI +CO2 +CH4 +N2O +CFC.11 +CFC.12 +TSI +Aerosols, data=Train)
summary(Model)
#Adjusted R-squared:  0.7436

#Question 24
summary(Model)

#Question 25
cor(ClimateChange)
cor(Train)

#Question 26
print(cor(Train),digits=2)
Choose<-abs(cor(Train))>0.7
Choose

#Question 27
print(cor(Train),digits=2)
abs(cor(Train))>0.7

#Question 28
Model1<-lm(Temp~ +MEI +TSI +Aerosols +N2O, data=Train)
summary(Model1)
#2.532e-02

#Question 29
#Adjusted R-squared:  0.7222 

#Question 30
StepClimate<-step(Model)
?step
summary(StepClimate)
PredictionStep<-predict(StepClimate, newdata=Test)
SSR<-sum((PredictionStep-mean(Test$Temp))^2)
SSR
SST<-sum((Test$Temp-mean(Test$Temp))^2)
SST
R2<-SSR/SST
R2
#0.2257779

#Question 31
summary(StepClimate)

#Question 32
PredictionStep<-predict(StepClimate, newdata=Test)
RMSE<-sqrt(mean((Test$Temp-PredictionStep)^2))
RMSE
#0.09522876
