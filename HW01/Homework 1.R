#----Homework 1----
# For your homework you will need two datasets: Movies and Employee Attrition.
# Please load them into R so that you can start working on them.
# Make sure you provide enough description for your findings whenever question asks
# for that. Feel free to express your thoughts, use your own words.
# Write your code right below the questions.
# Submit your R script on the moodle.
# The code should be written in a way that I will be able to open and run it
# on my computer without getting any errors.
# For the upcoming questions, use movies database.

load("Employee Attrition.rda")
load("Movies.rda")
str(movies)
install.packages("ggplot2")
library(ggplot2)


# 1.What are the earliest and latest years for which we have a movie in the dataset? (1 point)
min(movies$Year)    #earliest is 1920
max(movies$Year)    #lastest is 2016

# 2.Create distribution of movies by years. What does this tell you? (1 point)
hist(movies$Year, main="Movies by Years", xlab="Years")
qplot(movies$Year,geom="histogram", xlab="Years")
  #almost from 1998 until 2015 we have a remarkable incresaing in the number of movies
  #this is because everyone have television and people concentrated more 
  #on movie production than before


# 3.Look at the average imdb rating for movies by year. Do you see any trend? (1 point)
q<-aggregate(movies$imdbRating, by=list(year=movies$Year), 
                FUN="mean", na.rm=TRUE)
qplot(q$x,q$year, ylab="Year", xlab = "imdbRating")
       # we can say that the period from 1954 until 1970 was the highest rating 
        #of movies, which in one way people can understand that the best movies
      #produced on that time, but i think that people were seeing those kind of
  #movies for the first time, so they were amazed by them and giving high rating
#however by the years passed by, people get used to them, and they started to 
#expect more, so they started to feel that those movies are usual and give lower
#imdbRating



# 4.Create a boxplot for imdbRating and year, what do you see? (2 points)
boxplot(imdbRating~Year, data=movies)
#from 1992 until 2016, the mean is almost the same of ImdbRating
#starting from 1997 the outliers are so much


# 5.Create a new dataframe which is a subset of the old one.
#The dataframe must contain only the years during which at least 10 movies were
#created. Call the new dataframe movies2. (4 points)
t<-table(movies$Year)
t    #by this table i analyze the years that have more than 10 movies 
      #from 1980 until 2016
movies2<-movies[movies$Year>=1980,]    
View(movies2)

# For the upcoming questions use the movies2 dataframe.
# 6.Create a histogram of imdbRating variable. What does it tell you? (1 point)
qplot(movies2$imdbRating, geom="histogram", xlab="imdbRating")
#most of the movies rating are between 5.5 and 7.5 This means that most of our
# movies rating are average, as we don't have a lot of 8, 9 and 10
#it is a normally distributed data


# 7.Create histogram for a variable tomatoUserRating. What does it tell you?
# Compare with histogram of imdbRating - any insights? (2 points)
qplot(movies2$tomatoRating, geom="histogram", xlab="tomatoRating")
#most of movies rating range is much more here, it is from 4 to 8 
#after doing some researches why this happened, i found that Rotten Tomatoes 
#does not average the ratings at all! Wherease IMDb publishes weighted vote 
#averages rather than raw data averages, in order to eliminate and reduce 
#attempts at vote stuffing by people more interested in changing the current 
#rating of a movie than giving their true opinion of it.


# 8.Metascore is a score given to the movie by the critics, while imdbRating
# is a rating given by the users.
# What is the meaning of correlation coefficient between these two variables?
# (Hint: cor function will give an error if you dont handle missing values.
# Look at the help for the function to fix that issue). (2 points)
?cor
cor(movies2$Metascore, movies2$imdbRating, use="complete.obs")
#we have a strong correlation +0.75  signifies that both variables move in the 
#same direction. We have a strong linear relationship
#Users criteria of rating are almost the same as the criteria of critics


# 9.Find the movie for which critics and users disagree the most. (Hint:
# You may need to pay attention to the scales of the ratings) (2 points)

movies2[which.max(abs(movies2$Metascore-movies2$imdbRating*10)),"Title"]
#The movie is "To Save a Life"


# 10. Create correlation matrix between all rating variables.
# (Hint: Read the descriptions of all the variables to see which of them are ratings.) (1 point)
cor(movies2[ ,c("imdb_score", "Metascore", "imdbRating","tomatoRating","tomatoUserRating") ],use="complete" )


# 11.Do your own research and explain the meanings of the signs of 
# correlation  coefficients.(2 points)
#A positive correlation means that if one variable gets bigger, the other 
#variable tends to get bigger.
#A negative correlation means that if one variable gets bigger, the other
#variable tends to get smaller.
#The strongest correlations (r = 1.0 and r = -1.0 ) occur when data points 
#fall exactly on a straight line
# From Question 10, we see that the correlation between the same variable is 1
#Also we can see that the correlation of imdb_score and imdbRating
#is almost r=1, which means a strong linear relationship  
#Also the correlation between Metascore and TomatoRating is also very strong
#the critics score and the score in TomatoRating are correlated very much


# 12.Create a new dataframe which will show mean 
# gross income generated for each year. Create a plot summarizing that data.
# What trend do you see. Explain. (3 points)
newDataframe<-aggregate(movies2$gross, by=list(movies2$Year), FUN="mean", na.rm=TRUE ) 

View(newDataframe)
plot(newDataframe, xlab="Years", ylab="Gross", main="Mean gross income of each year" )
newDataframe[which.min(newDataframe$x),"Group.1" ]
newDataframe[which.max(newDataframe$x),"Group.1" ]
#from 1995 until 2005, the gross income was incresing and decreasing in a small 
#amounts, however that period shows that the gross income wasn't that much
#good. After 2005, the gross income started to increase remarkably
#until its obtained the maximum in 2016. This is normal as we are in a 
#rapid development time, where people use their computers or go to
#cinema to watch movies, which means income. Also the value of price has increased. 


# 13.What do you think, which variable is mostly correlated with the gross income.
# Test your assumption using cor function. Did you guess correct? Elaborate. (2 points)
cor(movies2$cast_total_facebook_likes,movies2$gross)
#I think that the cast of the movie play a big role in the grossing income
#For example if there is a celebrity that i like and he plays in a movie
#no matter what is the movie, i will go and watch it. If people think like me,
#that will increase the gross income 
cor(movies2[,c("budget","gross","Year", "imdbRating","cast_total_facebook_likes"
               ,"director_facebook_likes", "actor_2_facebook_likes", 
               "actor_3_facebook_likes","imdb_score", "actor_1_facebook_likes",
               "tomatoMeter","tomatoRating","tomatoUserRating"), ])
#My assumption was almost correct. The most correlated variable is 
#"actor_3_facebook_likes",which means people like the actor 3


# 14.Which movie director has the highest average imdbRating? Is his average gross
# also the highest. Give your thoughts on this topic.(Hint: you might find useful
# the phenomenon of time value of money). (3 points)
a<-aggregate(movies2$imdbRating,by=list(movies2$director_name), FUN="mean", na.rm=TRUE)
a[which.max(a$x),1]

g<-aggregate(movies2$gross,by=list(movies2$director_name), FUN="mean", na.rm=TRUE)
g[which.max(g$x),1]
g[which(g$Group.1=="Tony Kaye"),2]==max(g$x)
#Tony Kaye has the highest average imdbRating, but his average gross income is not 
#the highest
#Tony Kaye born in 1952, he directed the first movie American History X in 1998
#on that time the value of money was less than it is now, and people's income
#was less. 

# 15.Do your own analysis. Find something interesting (aggregate the data, 
# create plots, etc) (5 points)
my.data<-aggregate(movies$budget, by=list(year=movies$Year), FUN="mean")
boxplot(x~year, data=my.data)
#We saw in question 1, that the number of movies after 2000 was almost the same
#But the average budget of movies increases from year to year(after 2000), 
#as the price of everything is incresing starting from buying stuff,
#ending with paying for actors and renting places to shot the movie in,

my.data[which.max(my.data$x),"year"]
qplot(movies$Year,geom="histogram", xlab="Years")
#the highest average budget was in 2006, but with looking to the hsitogram, we 
#can say that in 2006 we had a lot of movies produced

my.data2<-aggregate(movies$duration, by=list(year=movies$Year), FUN="mean")
print(my.data2, digits=4)
my.data2[which.max(my.data2$x),"year"]
my.data2[which.min(my.data2$x),"year"]
boxplot(x~year, data=my.data2)
#from 1920 until 1970 the average duration of movies was not stable
#this can be refered as movies were something new on that time, and people 
#didn't know how many hours should the movie take, so they had a different 
#scales, also the most movies duration was in 1963 and the least in 1935
# after that people had experienced and they started to make the duration 
#of the movie almost 110 minutes



# For the upcoming questions, use Employee attriotion database.
# Factors are not set to this data, so R recognises them as integers.
# For some problems you might need setting factors in order for R to operate correctly.
# Make sure your variable types are set correctly before writing the code.

load("Employee Attrition.rda")

# 16.Which position of employee (JobRole) is yielding maximum salary (HourlyRate) 
# on average? Representative of which position (JobRole) has the highest
# hourly rate according to dataset? (2 points)
JobRole_HourlyRate<-aggregate(attrition$HourlyRate, by=list(attrition$JobRole), FUN="mean", na.rm=TRUE )
JobRole_HourlyRate[which.max(JobRole_HourlyRate$x),1]
attrition[which.max(attrition$HourlyRate),"JobRole"]
#the answer is the same "Healthcare Representative"

# 17.What is the correlation between the age of the employee and the hourly rate. Can you
# say that the older is the employee, the higher he is earning? Explain. (1 point)
cor(attrition$Age, attrition$HourlyRate)
#since we had a plus sign in the correlation, we should say that the older 
#is the employee, the higher he is earning. But this is not correct.
#correlation coefficient is almost 0, this means a weak linear relationship 
#between those two variable. So we can't say that the older is the employee,
#the higher he is earning


# 18.What is the age of the youngest and the oldest employees in the data? (1 point)
max(attrition$Age)   #oldest is 60
min(attrition$Age)  #youngest is 18

# 19.Create a histogram of the employees by monthly income. What does it tell you? (1 points)
hist(attrition$MonthlyIncome)
qplot(attrition$MonthlyIncome,geom="histogram", xlab="Monthly Income")
#Most of employees income is 2500$, we have a few employees that their income is
#above 10000$. This tells us that there is somehow equality in the work, as we didn't
#see an incresing then decresing then one more time a big incresing in the histogram
#it is not a normally distributed

# 20.Create a boxplot with Eduation and Percent Salary Hike. What do you see? (2 points)
# Explain your findings.
attrition$Education<-factor(attrition$Education, levels=c(1,2,3,4,5),
                            labels=c("Below College","College","Bachelor","Master","Doctor") )
boxplot(PercentSalaryHike~Education, data=attrition)
#As much the education level is higher as much the mean of increases the salary is highrer
#However, in The education level of Bachelor and Below College, there are more 
#most of our data are above the mean, which means, they get  more Percent Salary Hike
#and their Percent Salary Hike is more than others even more than Doctr education
#we can interpret this as those are the people that worked more and eventually
#they are getting an increasing in their salary every year, wherease Doctor level
#educated people, they start work after a minimum of 7 years of education, their sarly 
#starts at bigger amount and increases slowly


# ----Hypothesis testing----
# 21. Is there a difference between average Monthly Income of employees that churn
# (leave the company) and those who stay within the company? (2 points)
t.test(attrition$MonthlyIncome~attrition$Attrition)
#p-value is smaller than alpha which is 0.05, that means that we reject null Hypothesis H0
#which means there is a difference between average Monthly Income of employee that churn
# (leave the company) and those who stay within the company


# 22.Is the decision of attriting from the company independent from the gender of
# the employee? (2 points)
tablee<- table(attrition$Attrition, attrition$Gender)
chisq.test(tablee)
#p-value is bigger than alpha which is 0.05, that means that we don't reject null Hypothesis H0
#which means that the decision of attriting from the company is independent from the gender of
# the employee

# 23. Choose any two variables that you want and conduct a test either for independence
# or for differences in means. (2 points)
t.test(attrition$MonthlyIncome~attrition$Gender)
#p-value is bigger than alpha which is 0.05, that means that we don't reject null Hypothesis H0
#which means there is no difference between average Monthly Income of employee that are female
#and those who are male
#which means there is equality in the company 


# 24. Do your own analysis for attrition data. Find something
# interesting (aggregate the data, create plots, etc) (5 points) 
check<-aggregate(attrition$MonthlyIncome,by=list(workingyears=attrition$TotalWorkingYears),
                 na.rm=TRUE, FUN="mean")
check
qplot(check$workingyears,check$x, xlab="Total Working Years", ylab="Monthly Income")

#from The beginning of the work until the worker reaches a 20 working years, the Monthly Income 
#increases, after that there is a huge increas in the monthly income from 20 years to 21 years:
#The monthly income for the worker who worked for 20 years is:6431.400 on average
#Whereas for worker who worked for 21 years is: 16264.882 on average
#After the 21 years work, the monthly income start to increase and decrease

cor(attrition$TotalWorkingYears,attrition$MonthlyIncome)
#also those two variables have a strong correlation (0.77) 

attrition$JobSatisfaction<-factor(attrition$JobSatisfaction, levels=c(1,2,3,4),
                                  labels=c("Low",	"Medium", "High", "Very High"))

bb<-aggregate( attrition$TotalWorkingYears,by=list(attrition$JobSatisfaction),FUN="mean", na.rm=TRUE)
bb
plot(bb)
#Nothing interesting here, since they are almost the same 

cc<-aggregate(attrition$EmployeeNumber,by=list(attrition$EducationField), FUN="mean",na.rm=TRUE)
cc
plot(cc, xlab="EducationalField", ylab="EmployeeNumber")

#From this we can see that we have so many employers that their Education field is Human Resources
#maybe because they have more general education about life in general, and they can apply
#to work in many other fields,
#Altough the difference in employee number is not that much, but we need to consider that every
#job counts as a big opportunity to save a whole family  

