songs<-read.csv("songs.csv")
options(scipen=999)

#Question 1
data2010<-songs[songs$year==2010,]
nrow(data2010)
#373

#Question 2
dataMichael_Jackson<-songs[songs$artistname=="Michael Jackson",]
nrow(dataMichael_Jackson)
#18

#Question 3
data_Michael_top10<-dataMichael_Jackson[dataMichael_Jackson$Top10==1,]

#Question 4
table(songs$timesignature)
#0    1    3    4    5    7 
#10  143  503 6787  112   19 

#Question 5
table(songs$timesignature)
#4

#Question 6
songs[which.max(songs$tempo),2]
#Wanna Be Startin' Somethin'

#Question 7
SongsTrain<-songs[songs$year<=2009,]
SongsTest<-songs[songs$year==2010,]
nrow(SongsTrain)
#7201

#Question 8
SongsTrain$Top10<-as.factor(SongsTrain$Top10)
SongsTest$Top10<-as.factor(SongsTest$Top10)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

Model1<-glm(Top10~., data=SongsTrain, family ="binomial" )
summary(Model1)

#Question 12
cor(SongsTrain$loudness,SongsTrain$energy)
#0.7399067

#Qusetion 13
Model2<-glm(Top10~.-loudness, data=SongsTrain, family ="binomial" )
summary(Model2)

#Qusetion 14
Model3<-glm(Top10~.-energy, data=SongsTrain, family ="binomial" )
summary(Model3)

PredTest<-predict(Model3,newdata = SongsTest, type="response")
PredTest<-ifelse(PredTest>0.45, 1,0)
table(PredTest, SongsTest$Top10, dnn = c("Predicted","Actual"))

(309+19)/(309+40+5+19)
#0.8793566

#Qusetion 15
SongsTest[,"Baseline_pred_top10"] <- 0
table(SongsTest$Baseline_pred_top10, SongsTest$Top10)

314/(314+59)
#0.8418231

#Qusetion 16
table(PredTest, SongsTest$Top10, dnn = c("Predicted","Actual"))
#True positive=19

#Qusetion 17
table(PredTest, SongsTest$Top10, dnn = c("Predicted","Actual"))
#false positive=5

#Qusetion 18
309/(309+5)
#specificity 0.9840764

#Qusetion 19
19/(19+40)
#sensitivity 0.3220339

#Question 20
library(ggplot2)
library(ROCR)
P_Test <- prediction(PredTest, SongsTest$Top10) 
perf <- performance(P_Test,"tpr","fpr")
plot(perf)
##Find Area under the curve (AUC)
performance(P_Test, "auc")@y.values
#0.6530552

