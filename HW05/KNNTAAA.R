songs<-read.csv("songs.csv")
songs<-songs[,-c(1:5)]
install.packages("caret")
library(caret)
library(ggplot2)
set.seed(2016) 
sub = createDataPartition(songs$Top10, list =F, p =.75)
train1<-songs[sub,]
test1<-songs[-sub,]

tab_train<-table(train1$Top10)
tab_test<-table(test1$Top10)
prop.table(tab_train)
prop.table(tab_test)

train_scaled<- data.frame(scale(train1[,-34]), Top10 = train1$Top10)
library(class)
knn1<- knn(train = train_scaled, test = test1, cl= train_scaled$Top10, k=2)
test1$knn1<- knn1
confusionMatrix(test1$knn1, test1$Top10, positive="1")

library(ROCR)
pred<- prediction(test$knn1, test$Top10)


library(caret)
ctrl<-trainControl( method="repeated.csv", repeats =3, number = 10)
knnfit<- train(Top10~. , method ="knn", trControl =ctrl,
                preProcess =c("center", "scale"), tuneLength =20, data =train1)
