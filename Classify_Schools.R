

#=====Project begin here========================================================
#
# Random Forest Project
#
################################################################################

# 
# For this project we will be exploring the use of tree methods to classify schools as Private or Public based off their features.
# 
# Let's start by getting the data which is included in the ISLR library, the College data frame.
# 
# A data frame with 777 observations on the following 18 variables.
# 
# Private A factor with levels No and Yes indicating private or public university
# Apps Number of applications received
# Accept Number of applications accepted
# Enroll Number of new students enrolled
# Top10perc Pct. new students from top 10% of H.S. class
# Top25perc Pct. new students from top 25% of H.S. class
# F.Undergrad Number of fulltime undergraduates
# P.Undergrad Number of parttime undergraduates
# Outstate Out-of-state tuition
# Room.Board Room and board costs
# Books Estimated book costs
# Personal Estimated personal spending
# PhD Pct. of faculty with Ph.D.’s
# Terminal Pct. of faculty with terminal degree
# S.F.Ratio Student/faculty ratio
# perc.alumni Pct. alumni who donate
# Expend Instructional expenditure per student
#Grad.Rate Graduation rate




#Project goal
#Tree Methods Project
#For this project we will be exploring the use of tree methods to
#classify schools as Private or Public based off their features.

if(!require(caTools)) install.packages("caTools") 
if(!require(randomForest)) install.packages("randomForest") 
if(!require(rpart)) install.packages("rpart") 
if(!require(ISLR)) install.packages("ISLR") 



library(caTools)
#install.packages("randomForest")
library(randomForest)
#install.packages("rpart")
library(rpart)




#Let's start by getting the data which is included in the ISLR library, the College data frame.

library(ISLR)
head(College)
str(College)
summary(College)

#Get the Data
#Call the ISLR library and check the head of College (a built-in data frame with ISLR, 
#use data() to check this.) Then reassign College to a dataframe called df

data("College")
df <- College
head(df)


#EDA

#Let's explore the data!

#Create a scatterplot of Grad.Rate versus Room.Board, colored by the
#Private column.

#Call of 
library(car)
library(carData)
library(ggplot2)
set.seed(101)

head(df)

pl <- ggplot(df, aes(Grad.Rate, Room.Board)) + geom_point(aes(color = Private)) + theme_bw()
print(pl)



#Create a histogram of full time undergrad students, color by
#Private.

pl2 <- ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill = Private), color = "dark green") + theme_bw()

print(pl2)




#Create a histogram of Grad.Rate colored by Private. We should see
#something odd here.

ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill = Private), color = "dark green") + theme_bw()



#What college had a Graduation Rate of above 100% ?

subset(df, Grad.Rate > 100)



#Change that college's grad rate to 100%
df["Cazenovia College", "Grad.Rate"] <- 100




#Train Test Split

#Now let's split the data into training and testing sets 70/30. 
#Use the caTools library to do this.

library(caTools)
set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.7)

#Training
train = subset(df, sample = T)

#Testing
test = subset(df, sample = F)




#Decision Tree

#Use the rpart library to build a decision tree to predict whether
#or not a school is Private. Remember to only build your tree off
#the training data.

library(dplyr)
library(explore)

library(rpart)
library(rpart.plot)

tree <- rpart(Private ~ . , method='class', data= train)
prp(tree)



#Use predict() to predict the Private label on the test data.

tree.prediction <- predict(tree, test)



#Check the Head of the predicted values. We should notice that we
#actually have two columns with the probabilities.

head(tree.prediction)


#Turn these two columns into one column to match the original 
#Yes/No Label for a Private column.

tree.prediction <- as.data.frame(tree.prediction)

unity <- function(x){
  if (x >= 0.5){
    return("Yes")
    
  }else{
    return("No")
  }
  
}

tree.prediction$Private <- sapply(tree.prediction$Yes, unity)
head(tree.prediction)



#Now let's use table() to create a confusion matrix of the tree model.

table(tree.prediction$Private, test$Private)


#Use the rpart.plot library and the prp() function to plot out
#the tree model.

prp(tree)


#Random Forest

#Now let's build out a random forest model!

#Call the randomForest package library

library(randomForest)



#Now use randomForest() to build out a model to predict Private 
#class. Add importance=TRUE as a parameter in the model. 
#(Use help(randomForest) to find out what this does.


model <- randomForest(Private ~ .,   data = train, importance = T)
print(model) # view results

importance(model) # importance of each predictor


#What was the model's confusion matrix on its own training set?
#Use model$confusion.

model$confusion

# 
# Confusion matrix:
#   No Yes class.error
# No  183  29  0.13679245
# Yes  18 547  0.03185841
# > model$confusion
# No Yes class.error
# No  183  29  0.13679245
# Yes  18 547  0.03185841
# > 



#Grab the feature importance with model$importance. 
model$importance


#Predictions
#Now use your random forest model to predict on the test set!

p <- predict(model, test)

table(p, test$Private)


#Conclusion
#This is the end of this project, classify schools as Private or | Public based off their features.. Performance
#wise, it should have been better if it wasn’t just a single tree, how much better depends on whether we are
#emasuring recall, precision, or accuracy as the most important measure of the model.
