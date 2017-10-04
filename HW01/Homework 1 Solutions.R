#----Homework 1----
# For your homework you will need to datasets: Movies and Employee Attrition.
# Please load them into R so that you can start working on them.
# For the upcoming questions, use movies database. Write your code right below the questions.
# Submit your R script on the moodle.
# 1.What is the earliest and latest years for which we have a movie in the dataset? (1 point)
min(movies2$Year)
max(movies2$Year)

# 2.Create distribution of movies by years. What does this tell you? (1 point)
barplot(table(movies$title_year))
hist(movies$title_year)

# 3.Look at the average imdb rating for movies by year. Do you see any trend? (1 point)
plot(aggregate(movies$imdbRating, by=list(movies$title_year), FUN=meand))

# 4.Create a boxplot for imdbRating and year, what do you see? (2 points)
boxplot(imdbRating~Year, data=movies2)

# 5.Create a new dataframe which is a subset of the old one.
#The dataframe must contain only the years during which at least 10 movies were
#created. Call the new dataframe movies2. (4 points)
library(plyr)
year_freq<-count(df = movies2$Year) 
#after 1980 all the years had more than 10 movies
movies<-movies2[movies2$Year>1979,]

# For the upcoming questions use the movies dataframe.
# 6.Create a histogram of imdbRating variable. What does it tell you? (1 point)
qplot(movies$imdbRating, geom="histogram")
hist(movies$imdbRating)

# 7.Create histogram for a variable tomatoUserRating. What does it tell you?
# Compare with histogram of imdbRating - any insights? (2 points)
hist(movies$tomatoUserRating)

# 8.Metascore is a score given to the movie by the critics, while imdbRating
# is a rating given by the users.
# What is the meaning of correlation coefficient between these two variables?
# (Hint: cor function will give an error if you dont handle missing values.
# Look at the help for the function to fix that issue). (2 points)
cor(movies[,c("imdbRating","Metascore")],use = "na.or.complete")

# 9.Find the movie for which critics and users disagree the most. (Hint:
# You may need to pay attention to the scales of the ratings) (2 points)
plot(movies$imdbRating, movies$Metascore)

# 10. Create correlation matrix between all rating variables.
# (Hint: Read the descriptions of all the variables to see which of them are ratings.) (1 point)
rating<-movies[,c("imdbRating", "tomatoRating",
                  "tomatoUserRating")]
cor(rating, use="na.or.complete")

# 11.Do your own research and explain the meanings of the signs of 
# correlation  coefficients.(2 points)

# 12.Create a new dataframe which will show mean 
# gross income generated for each year. Create a plot summarizing that data.
# What trend do you see. Explain. (3 points)
meangross<-aggregate(movies2$gross,by=list(movies2$Year), FUN="mean", na.rm=TRUE)

# 13.What do you think, which variable is mostly correlated with the gross income.
# Test your assumption using cor function. Did you guess correct? Elaborate. (2 points)

# 14.Which movie director has the highest average imdbRating? Is his average gross
# also the highest. Give your thoughts on this topic.(Hint: you might find useful
# the phenomenon of time value of money). (3 points)
dir_rat<-aggregate(movies2$imdbRating, by=list(movies2$director_name), FUN="mean",na.rm=TRUE)
dir_gross<-aggregate(movies2$gross,by=list(movies2$director_name),FUN="mean",na.rm=TRUE)
which.max(dir_rat$x)
dir_rat[214,]
which.max(dir_gross$x)
dir_gross[945,]

# 15.Do your own analysis. Find something interesting (aggregate the data, 
# create plots, etc) (5 points)

# For the upcoming questions, use Employee attrition database.
# Factors are not set to this data, so R recognises them as integers.
# For some problems you might need setting factors in order for R to operate correctly.
# Make sure your variable types are set correctly before writing the code.
attrition<-read.csv("Employee attrition.csv")

# 16.Which position of employee (JobRole) is yielding maximum salary (HourlyRate) 
# on average? Representative of which position (JobRole) has the highest
# hourly rate according to dataset? (2 points)
attrition[which.max(attrition$HourlyRate),"JobRole"]
aggregate(attrition$HourlyRate,by=list(attrition$JobRole),FUN="mean", na.rm=TRUE)

# 17.What is the correlation between the age of the employee and the hourly rate. Can you
# say that the older is the employee, the higher he is earning? Explain. (1 point)
cor(attrition[,c("Age","HourlyRate")])

# 18.What is the age of the youngest and the oldest employees in the data? (1 point)
min(attrition$Age)
max(attrition$Age)

# 19.Create a histogram of the employees by monthly income. What does it tell you? (1 points)
hist(attrition$MonthlyIncome) 

# 20.Create a boxplot with Eduation and Percent Salary Hike. What do you see? (2 points)
# Explain your findings.
boxplot(MonthlyIncome~Education, data=attrition) 

# ----Hypothesis testing----
# 21. Is there a difference between average Monthly Income of employees that churn
# (leave the company) and those who stay within the company? (2 points)
t.test(attrition$MonthlyIncome~attrition$Attrition)

# 22.Is the decision of attriting from the company independent from the gender of
# the employee? (2 points)
Table<-table(attrition$Attrition,attrition$Gender)
chisq.test(Table)

# 23. Choose any two variables that you want and conduct a test either for independence
# or for differences in means. (2 points)

# 24. Do your own analysis for attrition data. Find something
# interesting (aggregate the data, create plots, etc) (5 points)
