#####Homework 3#####
# ATTENTION!!!: whenever you are applying k means clustering, set seed equal to 5.
# You need to set seed every time before performing k means clustering.
# The Global Competitiveness Report 2015-2016 assesses the competitiveness
# landscape of 144 economies, providing insight into the drivers of their 
# productivity and prosperity. The report remains the most comprehensive assessment of 
# national competitiveness worldwide, providing a platform for 
# dialogue between government, business and civil society about the actions 
# required to improve economic prosperity. Competitiveness is defined as the 
# set of institutions, policies and factors that determine the level of 
# productivity of a country. The level of productivity, in turn, sets the 
# level of prosperity that can be earned by an economy.
# Visit http://reports.weforum.org/global-competitiveness-report-2015-2016 for more details

# The different aspects of competitiveness are captured in 12 pillars, 
# which compose the Global Competitiveness Index. The value for pillar for each country is a 
# number between 1-7 and is measured based on the set of sub-indicators (also measured by the scale 1-7)
# Your task is to create clusters of countries based on the pillars of competitivness.

# 1. Load the dataset GCI.csv into R.
GCI<-read.csv("GCI.csv")

# 2. Make the variable Country.code as rownames for the dataframe 
# (hint: use rownames() command) (1)
?rownames()
rownames(GCI)<-GCI$Country.Code

# 3. Remove the variable Country.Code from the dataframe as you will 
# not need it anymore. (1)
GCI$Country.Code<-NULL

# 4. Run hierarchical clustering on the dataset, using 12 pillars as clustering variables (2)
#since pillars have the same scale so we don't need to scale them
dd<-dist(GCI[,2:13], method="euclidean")
clust<-hclust(dd)


# 5.Plot the dendogram. What do you think is the optimal number of clusters?
# Try 4 different options. The code should be written in such a 
# way that R gives you a new clear plot each time you try different number 
# of clusters and the rectangles are drown on top of them for each of your options.
# Your goal is to get as nit plots as possible. (2)
plot(clust, hang=-1, labels=F)
rect.hclust(clust, 3)
plot(clust, hang=-1, labels=F)
rect.hclust(clust, 4)
plot(clust, hang=-1, labels=F)
rect.hclust(clust, 5)
plot(clust, hang=-1, labels=F)
rect.hclust(clust, 6)
#Based on my observation, I think that 4 is the optimal number of clusters
#because with 4 clusters we minimize Intra-cluster distances
#and maximize Inter-cluster distances. By drawing line we can see that
#the distance between the intersection points is quite high, by taking
#more clusters that distance is being minimized, and by taking less
#clusters that distance is being maximized that will be the overall case not cluster



# 5.1 Choose one of the numbers of the clusters that you created in problem 5.
# Describe what are the differnces between the clusters in terms of differences in  means. (1)
GCI$cluster_membership<-cutree(clust, k=4)
data<-aggregate(GCI[,2:13], by=list(GCI$cluster_membership), FUN="mean", na.rm= TRUE)
#in terms of mean, we have different numbers between the clusters
#as each cluster contains a specific countries, whcih have good economies 
#or bad or middle. (see more description in the next question's answer)
###
#Pillars: 1: Institutions
#2: Infrastructure
#3: Macroeconomic environment
#4: Health and primary education
#5: Higher education and training
#6: Goods Market efficiency
#7: Labor market efficiency
#8: Financial market development
#9: Technological Readiness
#10: Market size
#11: Business sofistication
#12: Innovation


# 5.2 How will you describe your clusters? Try to give names to each of the clusters.(1)
#First cluster is the one that care of Health and primary education and they concentrate on the
#Higher education and training, however they are average on the other pillars(they don't 
#pay a lot of attention to the other pillars). This cluster have the weakest Labor market efficiency 
#Second cluster is the most develped one on every measure, except in Market size
#where cluster 3 market size is more developed than cluster 2 market size
#Third cluster is the second most developed one, but they are average in 
#Labor market efficiency and very developed in market size
#Fourth cluster is weak in terms of economics development
#as it is the weakest from all of the other clusters, however 
#Infrastructure, it is average in Labor market efficiency

#Names for every cluster:
#cluster 1: average competitive Economies, lacking Financial market development
#cluster 2: Prosper and developed competitive Economies
#cluster 3: Developed competitive Economies and big marketing size
#cluster 4: Under-developed competitive Economies

# 5.3 Looking at the averages of the pillars for each cluster, pick a pillar
# that you think constitutes the largest difference between the clusters.
# Create a boxplot of that pillar against the clusters. Give your comments. (2)
#Pillar 9 which represents the Technological Readiness  (for cluster 4 it is 2.7486
#whereas for cluster 2 it is 5.81  and the difference is 5.81-2.7486=3.0614)
boxplot(GCI$Pillar.9~GCI$cluster_membership)
#The boxplot also agree with what i said before :D
#we can see the huge difference in the avergae of pillar9 in cluster 4
#and cluster 2, they don't even have an intersection in their quartiles 
#the average of pillar 9 in cluster 3, is almost in the middle of the other two clusters
#we have an outlier in cluster 2

# 6.Run K-means algorithm, with the same number of clusters that you used in the
# prevous problem. (1) 
set.seed(5)
k<-kmeans(GCI[,2:13],4)
  
# 7. Are the results the same? Comment.
# (Remember that you might get different numbers (labels) for the 
# clusters if you are using different methods. (2)
GCI$kmeans_membership<-k$cluster
table(GCI$cluster_membership,GCI$kmeans_membership)
#the results are not the same
#the two approaches gave different clusters, for example cluster 1 in Hierarchy 
#approach has 43 members, wherease cluster 1 in kmeans approach has 38 members
#also for cluster 2, in Hierarchy approach we have 31 members, wherease in knmeans
#approach, we have 38

# Now choose one of the methods and continue with that.

# The dataset WDI indicators has some social and economic data on the countries included in GCI study.
# Note the WDI dataset has the same order as GCI,
# so you can easly add cluster variable to the WDI dataset

# 8.1 Read the dataset into R. Look at the population - what are the min and max values 
# for each cluster? what does this info tell you about the clusters? (2)
WDI<-read.csv("WDI.csv")

WDI$cluster_membership<-GCI$kmeans_membership
data2min<-aggregate(WDI$Population,by=list(WDI$cluster_membership), FUN="min", na.rm=TRUE)
data2max<-aggregate(WDI$Population,by=list(WDI$cluster_membership), FUN="max", na.rm=TRUE)

#View(data2min)
#View(data2max)
#Cluster 3 has the maximum number of population, but its economy
#is not the most developed one, however it can be said it is developed
#cluster 2 populations number is much less than cluster 3 and cluster 4
# and it has the msot developed economy, we can make some assumptions
#that the  large number of population doesn't play a big role in determining
#the economic development of the country
#Looking to the minimum number, we can see that cluster 2 has tha maximum number
#of population in the minimum section
#cluster 1 has the least number of population and its economic development
#is average

# 8.2 Describe your clusters using indicators from the WDI dataset:
# Note that the meanings of the variables are given in a seperate file.
# You need to do a small research, if you want to understand better these variables.
# Try to give description of each cluster in 2-3 sentences. (3)
WDI_data<-aggregate(WDI[,2:9], by=list(WDI$cluster_membership), FUN="mean", na.rm=TRUE)
#View(WDI_data)
##Cluster 4: Have a lot of export and import, it is evry developed,
#  as they have a lot of people that use internet, the GDP is high which
#  means the economy of the country is healthy. Since their inflation is low,
#  so it is good, their unemployment number is low and their population number 
#  is not too much. There is some correspondence, since they don't have 
#  a lot of population, so most of the people can find a job and their
#  unemployment should be low (of course taking into account that the economy should be good)
##Cluster 2:Represents the less development countries, as the population
#  number is not that much, but the unemployment rate is a lot. A small portion
#  uses internet. The import and export is low as well as the GDP. Inflation
#  is is high of course, it is not good for the economy or individuals. 
##Cluster 1: Have an average rate in import and export, but GDP is low.
#  The number of population is not that much, but the employment rate is
#  too much, which indicates that the economy is not that good, also the 
#  internet users are not a lot. Inflation is too high and life expectance is low.
#Cluster 3: Have an average export but less import. Densely populated with average
#   unemployment. average internet access and life expectancy. 

# Global Peace Index is an attempt to measure the relative position of nations' and regions' peacefulness.
# The dataset GPI provides the Global Peace Index scores and rankings 
# for the countries included in the list.
# Note the GPI dataset has the same order as GCI,
# so you can easly add cluster variable to the WDI dataset.

# 9.1 Load the dataset into R.
GPI<-read.csv("GPI.csv")
GPI$cluster_membership<-GCI$kmeans_membership
# 9.2 Calcualte average score for each cluster. Comment on your findings. (2)
GPI_data<-aggregate(GPI$GPI.2016.Score, by=list(GPI$cluster_membership), FUN="mean", na.rm=TRUE)
#cluster 4 has the least GPI score, which means those countries in cluster 4 
#are not very peacful (less peacful comparing to countries in other clusters)
#cluster 1 and 3 have almost the same GPI score, they are more peaceful than cluster 4
#cluster 2 has the most peacfull countries


# 9.3 Estimate rankings for cluster based on the average scores you received in previous step.
# (The rankings for each country are available in the GPI dataset.)(2)
#Calculate the mean rankings for each of the clusters. Find the relationship
#between the average ranking and average score for each of the clusters. Explain it
GPI_ranking_data<-aggregate(GPI$GPI.2016.Rank, by =list(GPI$cluster_membership), FUN="mean", na.rm=T)
#There is a realtionship between average ranking and average score
#for example in average score we saw that cluster 4 had the least number
#the same is in average ranking
#we can assume that the average score and average ranking are correlated
#as one increases the other also increase
#tha two averages cluster correspond to each other.

# 10. Do your own research on the datasets, find something interesting.(5)
# Do not do trivial manipulations like creating plots without making any valuable
# inferences about those plots.
#I was looking on the internet on how to determine the best number of clusters
#and i found this package 
# it measures the quality of a clustering. That is, 
#it determines how well each object lies within its cluster.
#A high average silhouette width indicates a good clustering.
#Average silhouette method computes the average silhouette of 
#observations for different values of k. The optimal number of 
#clusters k is the one that maximize the average silhouette over a
#range of possible values for k Reference from where i found: (Kaufman and Rousseeuw [1990]).
##install.packages("factoextra")
library(ggplot2)
library(factoextra)
require(cluster)
fviz_nbclust(GCI[,2:13], hcut, method = "silhouette",hc_method = "complete")
#It suggests that the optimal number is 2, however two is not enough for 
#deviding countries as it will be too general
data_scaleWDI<-scale(WDI[,2:9])
data_scale_WDI<-data.frame(Series=WDI$Series, as.data.frame(data_scaleWDI))
fviz_nbclust(WDI[,2:9], hcut, method = "silhouette",hc_method = "complete")
#for WDI, and GCI we can find by this method that the optimal number 
#of clustering is 2. However It is too general and can't be accepted.

#Now let me do some analysis on the data GCI:
cor(GCI[,6],GCI[,13])
plot(GCI[,6],GCI[,13], xlab="Higher education and training",ylab="Innovation")
#as the higher education increases, the innovation increases :)



#Football data analysis
# The file Soccer.csv summarized various data for football players from 11 European 
# Leagues. The data is collecting based on the FIFA ratings. 
# http://sofifa.com/player/192883 By opening this link, you can find the data for
# Henrikh Mkhitaryan, as well as find the decriptions of the variables. Do some
# reasearch on this web site before starting your homework.

  
# The file Soccer.csv summarized various data for football players from 11 European 
# Leagues. The data is collecting based on the FIFA ratings. 
# http://sofifa.com/player/192883 By opening this link, you can find the data for
# Henrikh Mkhitaryan, as well as find the decriptions of the variables. Do some
# reasearch on this web site before starting your homework.


  
# 11. Load file Soccer.csv into R. Perform k means clustering. 
# Try with at least two different numbers of clusters. (2)
Soccer<-read.csv("Soccer.csv")
set.seed(5)
kmeans_Soccer1<-kmeans(Soccer[,5:42],2)
set.seed(5)
kmeans_Soccer2<-kmeans(Soccer[,5:42],3)
set.seed(5)
kmeans_Soccer3<-kmeans(Soccer[,5:42],4)
set.seed(5)
kmeans_Soccer4<-kmeans(Soccer[,5:42],5)
set.seed(5)
kmeans_Soccer5<-kmeans(Soccer[,5:42],6)

# 12. Plot the clusters for all the trials (numbers of clusters).
# Do you see any similarity in both graphs?
# Hint: you may find useful package fpc and function called plotcluster. (3)
  #install.packages("fpc")
  library(fpc)
  plotcluster( Soccer[,5:42], kmeans_Soccer1$cluster)
  plotcluster( Soccer[,5:42], kmeans_Soccer2$cluster)
  plotcluster( Soccer[,5:42], kmeans_Soccer3$cluster)
  plotcluster( Soccer[,5:42], kmeans_Soccer4$cluster)
  plotcluster( Soccer[,5:42], kmeans_Soccer5$cluster)
#in every plot we have two density data on the two sides and we don't
  #have data in between
#we have two groups, one group is being devided when we increase
#the number of clusters, and the other half stay the same
  
    
# 13. Aggregate the means for all the variables for one of 
# the results that you got in question 2. Give comments about the clusters. (2)
#let me take the one with 3 clusters.
  Soccer$kmean_Soccer<-kmeans_Soccer2$cluster
  soccer_data<-aggregate(Soccer[,5:42], by=list(Soccer$kmean_Soccer), FUN="mean", na.rm=TRUE)
#View(soccer_data)
  #Cluster 3 has football players who are the tallest and oldest and 
#they weight more than the other two clusters, since they are older
#than others, their crossing skill is the least and also their heading 
#and shooting accurancy is low , they also lack stamina and their 
#goal keeper skills are low, however their strength is on average
#in other words: Cluster 3 players are the oldest that have the lowest
#attributions(the skills that are given)
#cluster 2 and cluster 1 have some similarities, their players
#are younger than cluster 3 and they have more abilities
#However cluster 1 represents the best players, best shoting skills, dribbling,
#curves and many other skills
#wherease we can define cluster 2 as average players
  
  
# 14. Now perform hierarchical clustering. Again try with several number of clusters. (2)
  scale_soccer<-scale(Soccer[,5:42])
  data_scale_soccer<-data.frame(Soccer[,1:4],as.data.frame(scale_soccer))
  distance_scale_soccer<-dist(data_scale_soccer[5:42],method="euclidean")
  clust_soccer<-hclust(distance_scale_soccer)
  Soccer$hclust2<-cutree(clust_soccer,k=2)
  Soccer$hclust3<-cutree(clust_soccer,k=3)
  Soccer$hclust4<-cutree(clust_soccer,k=4)
  Soccer$hclust5<-cutree(clust_soccer,k=5)
  
# 15. For all the trials, again create plots and try to find patterns.
# Describe the patterns that you noticed. (2)
  plotcluster( Soccer[,5:42], Soccer$hclust2)
  plotcluster( Soccer[,5:42], Soccer$hclust3)
  plotcluster( Soccer[,5:42], Soccer$hclust4)
  plotcluster( Soccer[,5:42], Soccer$hclust5)
#Those are almost the same as we did before using kmeans approach
#the pattern is that we have two groups (starting by 2 clusers),
#one group is being devided when we increase the number of clusters,
#and the other half stay the same

#Here are also the hierarchy plots:
  plot(clust_soccer, hang=-1, labels=F)
  rect.hclust(clust_soccer, 2)
  plot(clust_soccer, hang=-1, labels=F)
  rect.hclust(clust_soccer, 3)
  plot(clust_soccer, hang=-1, labels=F)
  rect.hclust(clust_soccer, 4)
  plot(clust_soccer, hang=-1, labels=F)
  rect.hclust(clust_soccer, 6)
  plot(clust_soccer, hang=-1,labels=F)
  rect.hclust(clust_soccer, 8)
#The same pattern i can also say on these plots:
#The left most cluster is fixed, and the right is being devided
#into clusters when we increase the number of clusters 

    
# 16. Based on the patterns and similarities between the graphs that you
# noticed while performing clustering with kmeans and hierarchical 
# clustering, come up with an optimal number of clusters. Choose
# either of the clustering methods, and assign clusters to each of 
# the cases in the soccer dataset. (2)
#The optimal number is 3, because looking to the plot, we can see that when we
  #take 4 clusters, the distance of the data between two clusters is 
  #narrowing (from the plot using fpc) 
#Since i put before in my data the cluster memberships, i just need to 
  #remove what i don't want, and keep the optimal one
Soccer$hclust2<-NULL
Soccer$hclust4<-NULL
Soccer$hclust5<-NULL

# 17. Aggregate the average data for all the variables for the number
# of the clusters that you chose in question 7. What are the differences 
# between those clusters? Try to give a general description for 5-6 of them. (2)
data_soccer<-aggregate(Soccer[,5:42], by=list(Soccer$hclust3), FUN="mean", na.rm=T)
#View(data_soccer)
#Cluster 1: It represents the players that are young, have a big potential,
#           are not very strength as they are still young, however
#           they have a good agility, as they are young take risks and
#           during the game without thinking. They can perfectly control 
#           the ball, and they are good at dribbling, volleys and short passing.
#Cluster 2: It represents average age players, with average potential,
#           who are very strong(as they were younger, they practiced a lot
#           to become stronger, they have a average agility, as they started
#           to play more by thinking, they can control the ball on average
#           as well as dribble, volly and short pass.
#Cluster 3:It represents the oldest players, with average potential,
#           who are average in strength, with very low agility as they 
#           became very wise throughout the years and can't take risks,
#           they are bad at controlling the ball, also in dribble,
#           volly and short pass.

#In summary we have three clusters: Good, Average, Bad
  
# 18. Pick a player from each of those clusters. Describe those players in terms of
# their affiliation to the clusters. What are the similarities and differences between them? (1)
#Cluster: 2:Philippe Coutinho
View(Soccer[c(850,2051,4499),])
#I took from cluster1: Nikola Gjorgjev
#             cluster2: Philippe Coutinho
#             cluster3: Ferdinando Coppola

#The age is different between the three players: 
#Nikola is the youngest 19years old   (cluster 1)
#Coutinho is 24 years old             (cluster 2)
#Ferdinando is 38 years old           (cluster 3)
#Those players are very affilated to their clusters, as above i mentined that
#cluster 1 represents young and good players, looking to Nikola Gjorgjev
#we can say that he is young and good player, as some of his skills are very high
#Also Philippe Coutinho is affiliated to his cluster, his age is average,
#but looking to his abilities and skills, we can say that he is a good player
#Ferdinando Coppola is very affiliated to his cluster, he is the oldest 
#between the three, and as his cluster describes, he is not that good
#player
#Only marking is almost the same between the three players
#Everything else is different
#Jumping is the same in Coutinho and Ferdinando
#Potential is the same in Nikola Gjorgjev and Ferdinando Coppola 
#what skills related to GK is very different between the three players

# 19. Now aggregate the data in a way that you end up with dataframe
# where each row represents a club. Now run clustering on that dataframe
# (use whichever method you prefer.) Aggregate the average values for
# each of the clusters. Again, choose 5-6 variables, compare the clubs
# based on them, and try to give names to the clusters.
Club_data<-aggregate(Soccer[,5:42],by=list(Soccer$team_long_name),FUN="mean", na.rm=T)
club_scale<-as.data.frame(scale(Club_data[,-1]))
dist_club_scale<-dist(club_scale[,-1], method="euclidean")
clust_club<-hclust(dist_club_scale)
plot(clust_club,hang=-1, labels=F)
rect.hclust(clust_club,3)
plot(clust_club,hang=-1, labels=F)
rect.hclust(clust_club,4)
plot(clust_club,hang=-1, labels=F)
rect.hclust(clust_club,3)
Club_data$cluster_membership<-cutree(clust_club, k=3)
data_soccer_club<-aggregate(Club_data[,-1],by=list(Club_data$cluster_membership), FUN="mean", na.rm=T)
View(data_soccer_club)
#Club 1: Are the youngest players that their overall rating is small
#       their potential is low, Their shot power and reaction is 
#       not that high. They are not accurate in free kick.
#Club 2: Are the average old players that their overall rating is too high
#       they have a big potential, Their shot power and reaction is
#       very high. They are accurate in free kick.
#Club 3: Are the average old players that their overall rating is average
#       They have a potential but not like cluster's 3,Their shot power 
#       and reaction is average. They are average on free kick accuracy.
##NAMES TO THE CLUSTERS:
#Cluster 1: Young and bad football players
#Cluster 2:Professional football players
#Cluster 3:Average football players

# 20. Do your own research on the datasets, find something interesting.
# Do not do trivial manipulations like creating plots without making any valuable
# inferences about those plots. (5)

#Let me take Liverpool league:
Liverpool<-subset(Soccer,Soccer$team_long_name=="Liverpool")
View(Liverpool)
cor(Liverpool$Age,Liverpool$overall_rating)
#The correlation is 0.7717309
#which means that the age and the overall rating are highly correlated
#with each other
boxplot(Liverpool$overall_rating~Liverpool$Age)
#The average is not the same, nor near each other
#we can see that as the player get older, his overall rating
#is being better. 
#Let us check by means:
age<-aggregate(Liverpool$overall_rating,by=list(Liverpool$Age), FUN="mean", na.rm=T)
plot(age, xlab="age",ylab="overall rating")
#But the number of players it seems like decresing,
#so let's check that:
hist(Liverpool$Age)
#My assumption was write the number of players is decreasing after 25 years
#And most of the Liverpool players are 25 years old
#In summary as the age of teh player increases, the number of players 
#decreases and the overall rating increases

#Now who is the best player in Liverpool ? :D
Liverpool[which.max(Liverpool$overall_rating),1]
#Best player is Kolo Toure

Liverpool[which.min(Liverpool$overall_rating),1]
#and the worst is Connor Randall

#What about my assumption above about age and overall rating?
#let me check that for the best player
Liverpool[which.max(Liverpool$overall_rating),5]
#YEEEEEEY my analyze was correct :D :D :D
#The best player is 35 years old :D

#now some clustering over a small size of a group (as we always did big size of group ):
Liverpool_scale<-scale(Liverpool[,5:42])
Liverpool_distance<-dist(Liverpool_scale, method="euclidean")
Liverpool_clust<-hclust(Liverpool_distance)
plot(Liverpool_clust, hang=-1, labels=F)
rect.hclust(Liverpool_clust,2)
plot(Liverpool_clust, hang=-1, labels=F)
rect.hclust(Liverpool_clust,3)
#Let me cluster by 3
Liverpool$membership<-cutree(Liverpool_clust,3)
data_Liverpool_<-aggregate(Liverpool[,5:42], by=list(Liverpool$membership), FUN="mean", na.rm=T)
View(data_Liverpool_)
#Third cluster represents the bad players, that have weak skills
#second cluster represents good players with weak slanding and sliding tackle
#First cluster represents average players with good marking, slanding and sliding tackle 