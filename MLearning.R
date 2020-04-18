setwd("/Users/MachineLearning")
# <<<Intro to Machine Learning >>>
# << Ch1:What is M.L ? >>
# Acquaint yourself w/data
# INSTRUC:
# Use the two ways presented in the video to find out the number of observations and variables of the iris data set: str() and dim(). Can you interpret the results?
# Call head() and tail() on iris to reveal the first and last observations in the iris dataset.
# Finally, call the summary() function to generate a summary of the dataset. What does the printout tell you?
# CODE
# iris is available from the datasets package

# Reveal number of observations and variables in two different ways
dim(iris)
str(iris)

# Show first and last observations in the iris data set
head(iris)
tail(iris)


# Summarize the iris data set
summary(iris)
#-----------------------
# < Basic Prediction Model >
predict() # 1st arg = model, 2nd arg= unseen observ as a df
?data.frame()
# INSTRUC:
# Take a look at the code that builds lm_wage, which models the wage by the age variable.
# See how the data frame unseen is created with a single column, age, containing a single value, 60.
# Predict the average wage at age 60 using predict(): you have to pass the arguments lm_wage and unseen. Make sure the variable is displayed in the console (don't assign it to a variable). Can you interpret the result?
## CODE
# The Wage dataset is available

# Build Linear Model: lm_wage (coded already)
lm_wage <- lm(wage ~ age, data = Wage)

# Define data.frame: unseen (coded already)
unseen <- data.frame(age = 60)

# Predict the wage for a 60-year old worker. 
predict(lm_wage, unseen)
#------------------------
# Classification: goal predict category of new observ
# Regression: lm(response~predictor)
#-----------------------
# < Classification: Filtering Spam >
emails  <- read.csv("/Users/MachineLearning/spambase/spambase.data", header = F)
colnames(emails) <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference", "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$", "char_freq_#", "capital_run_length_average", "capital_run_length_longest", "capital_run_length_total", "spam")
x<- 1:4
rep(NA, length(x))
length(avg_capital_seq)[1]
attach(emails)
## INSTRUC
# Check the dimensions of this dataset, use dim().
# Inspect the definition of spam_classifier(). It's a simple set of statements that decide between spam and no spam based on a single input vector.
# Pass the avg_capital_seq column of emails to spam_classifier() to determine which emails are spam and which aren't. Assign the resulting outcomes to spam_pred.
# Compare your prediction, spam_pred, to the true spam labels in emails$spam with the == operator. Simply print out the result. How many of the emails were correctly classified?
## Code
# The emails dataset is already loaded into your workspace

# Show the dimensions of emails
dim(emails)

# Inspect definition of spam_classifier()
spam_classifier <- function(x){
  prediction <- rep(NA,length(x)) # initialize v 
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(prediction)
}

# Apply the classifier to the avg_capital_seq column: spam_pred
attach(emails)
spam_pred <-spam_classifier(capital_run_length_average) # read in each words value for this variable, create a seq of NA's same length as instance of varb, assign value of 0,1 to 'prediction' based on length of variable.

# Compare spam_pred to emails$spam. Use ==
spam_pred == emails$spam
prediction_rate <- sum(spam_pred == emails$spam)/dim(emails)[1]
#----------------------------
# << Regression: Linkedin views for next 3 days >>
# INSTRUC:
# Create a vector days with the numbers from 1 to 21, which represent the previous 21 days of your linkedin views. You can use the seq() function, or simply :.
# Try to fit a linear model that explains the linkedin views based on days. Use the lm() function with the appropriate formula. lm(y ~ x), for example, builds a linear model of y based on x. Assign the resulting linear model to linkedin_lm.
# Using this linear model, predict the number of views for the next three days (22, 23 and 24). Use predict() and the predefined future_days data frame. Assign the result to linkedin_pred.
# See how the remaining code plots both the historical data and the predictions. Try to interpret the result.

## CODE
# linkedin is already available in the workspace

# Create the days vector
days <- seq(1:length(linkedin))

# Fit a linear model called on the linkedin views per day: linkedin_lm
linkedin_lm <- lm(linkedin ~ days)

# Predict the number of views for the next three days: linkedin_pred
future_days <- data.frame(days = 22:24)
# Syntax predict(model to use, )
linkedin_pred <- predict(linkedin_lm, future_days)


# Plot historical data and predictions
plot(linkedin ~ days, xlim = c(1, 24))
points(22:24, linkedin_pred, col = "green")
#--------------
# < Clustering: Sperating the iris species >
# CODE
# Set random seed. Don't remove this line.
set.seed(1)

# Chop up iris in my_iris and species
my_iris <- iris[-5]
species <- iris$Species

# Perform k-means clustering on my_iris: kmeans_iris
kmeans_iris <- kmeans(my_iris, 3)

# Compare the actual Species to the clustering using table()
table(kmeans_iris, )

# Plot Petal.Width against Petal.Length, coloring by cluster
plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)

#---------------
# < Getting practical w/supervised learning  >
# Instruc:
# Take a look at the iris dataset, using str() and summary().
# The code that builds a supervised learning model with the rpart() function is already coded for you. This model trains a decision tree on the iris dataset. Decision trees will be explained in Chapter 3.
# Use the predict() function with the tree model as the first argument. The second argument should be a dataframe containing observations of which you want to predict the label. In this case, you can use the predefined unseen data frame. The third argument should be type = "class". Simply print out the result of this prediction step.
# CODE
# Set random seed. Don't remove this line.
set.seed(1)

# Take a look at the iris dataset
str(iris)
summary(iris)

# A decision tree model has been built for you
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
              data = iris, method = "class")

# A dataframe containing unseen observations
unseen <- data.frame(Sepal.Length = c(5.3, 7.2), 
                     Sepal.Width = c(2.9, 3.9), 
                     Petal.Length = c(1.7, 5.4), 
                     Petal.Width = c(0.8, 2.3))

# Predict the label of the unseen observations. Print out the result.
predict(tree, unseen, type='class')
#------------
# How to do unsupervised learning (1)
# Instruc:
# Explore the dataset using str() and summary().
# Use kmeans() with two arguments to group the cars into two clusters based on the cars' hp and wt. Assign the result to km_cars.
# Print out the cluster element of km_cars; it shows which cars belong to which clusters.
# CODE
# The cars data frame is pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Explore the cars dataset
str(cars)
summary(cars)


# Group the dataset into two clusters: km_cars
km_cars <- kmeans(cars, 2)

# Print out the contents of each cluster
print(km_cars)
#---------------
# How to do unsupervised learning (2)
# Instruc
# Finish the plot() command by coloring the cars based on their cluster. Do this by setting the col argument to the cluster partitioning vector: km_cars$cluster.
# Print out the clusters' centroids, which are kind of like the centers of each cluster. They can be found in the centers element of km_cars.
# Replace the ___ in points() with the clusters's centroids. This will add the centroids to your earlier plot. To learn about the other parameters that have been defined for you, have a look at the graphical parameters documentation.
# CODE
# The cars data frame is pre-loaded

# Set random seed. Don't remove this line
set.seed(1)

# Group the dataset into two clusters: km_cars
km_cars <- kmeans(cars, 2)

# Add code: color the points in the plot based on the clusters
plot(cars, col=km_cars$cluster)

# Print out the cluster centroids
print(km_cars)['centers']

# Replace the ___ part: add the centroids to the plot
points(km_cars[['centers']], pch = 22, bg = c(1, 2), cex = 2)
#---------------
# << Ch2: Performance measures  >>
# < The Confusion Matrix >
# Instruc:
# Have a look at the structure of titanic. Can you infer the number of observations and variables?
# Inspect the code that build the decision tree, tree. Don't worry if you do not fully understand it yet.
# Use tree to predict() who survived in the titanic dataset. Use tree as the first argument and titanic as the second argument. Make sure to set the type parameter to "class". Assign the result to pred.
# Build the confusion matrix with the table() function. This function builds a contingency table. The first argument corresponds to the rows in the matrix and should be the Survived column of titanic: the true labels. The second argument, corresponding to the columns, should be pred: the predicted labels.
# CODE
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

# A decision tree classification model is built on the data
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred
pred <- predict(tree, titanic, type="class")

# Use the table() method to make the confusion matrix
table(titanic$Survived, pred)
#--------------------------
# < Deriving ratios from the confusion matrix >
# Instruc:
# Assign the correct values of the confusion matrix to TP, FN, FP and TN.
# Calculate the accuracy as acc and print it out.
# Finally, also calculate the precision and the recall, as prec and rec. Print out both of them.
# CODE
# The confusion matrix is available in your workspace as conf

# Assign TP, FN, FP and TN using conf
TP <- 212
FN <- 78
FP <- 53
TN <- 371

# Calculate and print the accuracy: acc
acc <- ( TP +TN )/(TP+FN+FP+TN)
print(acc)

# Calculate and print out the precision: prec
prec <- TP/( TP + FP )
print(prec)

# Calculate and print out the recall: rec
rec <- TP/ ( TP + FN ) 
print(rec)

#---------------
# < The quality of a regression  >
# Instuc:
# Take a look at the structure of air. What does it tell you?
# Inspect your colleague's code that builds a multivariable liner regression model based on air. Not familiar with multiple linear regression? No problem! It will become clear in chapter 4. For now, you'll stick to assessing the model's performance.
# Use the predict() function to make predictions for the observations in the air dataset. Simply pass fit to predict(); R will know what to do. Assign the result to pred.
# Calculate the RMSE using the formula above. yi corresponds to the actual sound pressure of observation i, which is in air$dec. ŷ i corresponds to the predicted value of observation i, which is in pred. Assign the resulting RMSE to rmse.
# Print out rmse.
# CODE
# The air dataset is already loaded into your workspace

# Take a look at the structure of air
str(air)

# Inspect your colleague's code to build the model
fit <- lm(dec ~ freq + angle + ch_length, data = air)

# Use the model to predict for all values: pred is y_hat
pred <- predict(fit)

# Use air$dec and pred to calculate the rmse
rmse <- sqrt(1/dim(air)[1]*sum((air$dec - pred)^2))
             
# Print out rmse
print(rmse)
#--------------------
# < Adding complexity to increase quality >
# Instruc:
# Use the predict() function to make predictions using fit2, for all values in the air dataset. Assign the resultin vector to pred2.
# Calculate the RMSE using the formula above. Assign this value to rmse2.
# Print rmse2 and compare it with the earlier rmse. What do you conclude?

# CODE
# The air dataset is already loaded into your workspace

# Previous model
fit <- lm(dec ~ freq + angle + ch_length, data = air)
pred <- predict(fit)
rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))
print(rmse)

# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the model to predict for all values: pred2
pred2 <- predict(fit2)

# Calculate rmse2
rmse2 <- sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))

# Print out rmse2
print(rmse2)
rmse2 < rmse
#------------------------------
# < Let's do some clustering  >
# Instruc
# Take a look at the structure of the seeds dataset.
# Extend the plot() command by coloring the observations based on their cluster. Do this by setting the col argument equal to the cluster element of km_seeds.
# Print out the ratio of the within sum of squares to the between cluster sum of squares, so WSS/BSS. These measures can be found in the cluster object km_seeds as tot.withinss and betweenss. Is the within sum of squares substantionally lower than the between sum of squares?
# CODE
# The seeds dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Explore the structure of the dataset
str(seeds)

# Group the seeds in three clusters
km_seeds <- kmeans(seeds, 3)

# Color the points in the plot based on the clusters
plot(length ~ compactness, data = seeds, col= km_seeds$cluster)

# Print out the ratio of the WSS to the BSS
print(km_seeds$tot.withinss/km_seeds$betweenss)

#-----------------------
# < Split the sets >
# Instruc
# Shuffle the observations of the titanic dataset and store the result in shuffled.
# Split the dataset into a train set, and a test set. Use a 70/30 split.
# Print out the structure of both train and test with str(). Does your result make sense?
# R DOCUMEMTATION CODE - more intuitive way of shuffling data set.
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = T) # sample from (A,B,C) w/replacement
df <- data.frame(x=1, y = 1:10, fac=fac)
#---------
# What the indexed(less intuitive) does in 1D
z <- 1:5
print(z[sample(length(z))])
#------------------
# What the indexed(less intuitive) does in 2D
c_df <- data.frame(a=LETTERS[1:3], b=LETTERS[4:6])
shuffled_df <- c_df[sample(nrow(c_df)), ]
#-------------------
# EX CODE
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled_df <- titanic[sample(n), ]

# Split the data in train and test
train_indices <- 1:round(0.7 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.7*n)+1):n
test <- shuffled_df[test_indices, ]

# Print the structure of train and test
str(train)
str(test)
#------------------
# < First you train, then you test   >
# Instruc
# Correct the decision tree model that is learned. It should be learned on the training set.
# Use the predict() function with as first argument the tree model, as second argument the correct dataset and set type to "class". Call the predicted vector pred.
# Use the table() function to calculate the confusion matrix. Assign this table to conf. Remember that the actual labels of the test set should come in the rows, and the predicted labels in the columns.
# Finally, print out conf.
# CODE
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset; build train and test
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Change the model that has been learned. One of the arguments is incorrect.
tree <- rpart(Survived ~ ., train , method = "class")

# Predict the outcome on the test set with tree: pred
pred <- predict(tree, test, type="class")

# Calculate the confusion matrix
conf <- table(test[,1], pred)

# Print this confusion matrix
print(conf)
#-------------------------
# < Using Cross Validation  >
# Instruc
# Initialize a vector of length 6. The elements should all be 0.
# The part to split the dataset correctly 6 times and build a model on the training set is already written for you inside the for loop; try to understand the code.
# Use the model to predict the values of the test set. Use predict() and don't forget to set type to "class". Assign the result to pred.
# Make the confusion matrix using table() and assign it to conf.
# Assign the accuracy of the model to the correct index in accs. Use the confusion matrix, conf. Tip: diag() gives you the elements on the diagonal of a matrix.
# Finally, print the mean accuracy.
# CODE
# The shuffled dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type='class')
  
  # Assign the confusion matrix to conf
  conf <- table(test[,1], pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/nrow(test)
}

# Print out the mean of accs
print(mean(accs))
#-------------------
# < Overfitting the Spam >
# Instruc
# Apply spam_classifier() on the avg_capital_seq variable in emails_full and save the results in pred_full.
# Create a confusion matrix, using table(): conf_full. Put the true labels found in emails_full$spam in the rows.
# Use conf_full to calculate the accuracy: acc_full. The functions diag() and sum() will help. Print out the result.
# CODE
# ** Syntax table()  table(rows, cols) = table(true lables, pred labels)
# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels=c("1","0")))
}

# Apply spam_classifier to emails_full: pred_full
pred_full <- spam_classifier(emails_full$avg_capital_seq)

# Build confusion matrix for emails_full: conf_full
conf_full <- table(emails_full$spam, pred_full)

# Calculate the accuracy with conf_full: acc_full
acc_full <- sum(diag(conf_full))/nrow(emails_full)

# Print acc_full
print(acc_full)
# Note: see results when submit code for interpretability
#----------------------
# < Increasing the Bias >
# Instruc
# Simplify the rules of the given classifier. emails with a avg_capital_seq strictly longer than 4 are spam, all others are seen as no spam.
# Inspect the code that defined conf_small and acc_small.
# Set up the confusion matrix for the emails_full dataset. Put the true labels found in emails_full$spam in the rows. Assign to conf_full.
# Use conf_full to calculate the accuracy. Assign this value to acc_full and print it out. Before, acc_small and acc_full were 100% and 65%, respectively; what do you conclude?
# CODE
# The all-knowing classifier that has been learned for you
# You should change the code of the classifier, simplifying it
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x <= 4] <- 0
  #prediction[x >= 3 & x <= 4] <- 0
  #prediction[x >= 2.2 & x < 3] <- 1
  #prediction[x >= 1.4 & x < 2.2] <- 0
  #prediction[x > 1.25 & x < 1.4] <- 1
  #prediction[x <= 1.25] <- 0
  return(factor(prediction, levels=c("1","0")))
}

# conf_small and acc_small have been calculated for you
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
acc_small <- sum(diag(conf_small)) / sum(conf_small)
acc_small

# Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
conf_full <- table(emails_full$spam, spam_classifier(emails_full$avg_capital_seq))
                   
# Calculate acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)
                   
# Print acc_full
print(acc_full)
#---------------------------
# << Ch3: Classification >>
# < Learn a decision tree >
# Instruc:
# Load in all the packages that are mentioned above with library().
# Use rpart() with a formula to learn a tree model and assign the result to tree. The algorithm can use all available features, so you can use Survived ~ .. Make sure to set the method to  "class" and to train on the training set only!
# Create a plot of the learned model using fancyRpartPlot().
# CODE
# The train and test set are loaded into your workspace.

# Set random seed. Don't remove this line
set.seed(1)

# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build a tree model: tree
tree <- rpart(Survived ~., data = train , method='class')

# Draw the decision tree
fancyRpartPlot(tree)
#-------------------
# < Classifying w/the decision tree >
# Instruc:
# Use tree to predict the labels of the test set with the predict() function; store the resulting prediction in pred.
# Create a confusion matrix, conf, of your predictions on the test set. The true values, test$Survived, should be on the rows.
# Use the confusion matrix to print out the accuracy. This is the ratio of all correctly classified instances devided by the total number of classified instances, remember?
#CODE
library(MASS)
?tree
install.packages("tree")
library(tree)
?lm
?tree
# Syntax predict(model, dataset, type)
# The train and test set are loaded into your workspace.

# Code from previous exercise
set.seed(1)
library(rpart)
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the values of the test set
pred <- predict(tree, test, type="class")

# Construct the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print out the accuracy
print(sum(diag(conf))/nrow(test))
#--------------------------------------
# < Pruning the Tree >
# Instruc:
# A model, tree is already coded on the right. Use fancyRpartPlot() to plot it. What do you think?
# Use the prune() method to shrink tree to a more compact tree, pruned. Also specify the cp argument to be 0.01. This is a complexity parameter. It basically tells the algorithm to remove node splits that do not sufficiently decrease the impurity.
# Take a look at this pruned tree by drawing a fancy plot of the pruned tree. Compare the two plots.
# CODE
library(rpart)
?prune
# All packages are pre-loaded, as is the data

# Calculation of a complex tree
set.seed(1)
tree <- rpart(Survived ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
pruned <- prune(tree, cp = 0.01)

# Draw pruned
fancyRpartPlot(pruned)
#---------------------------------
# < Splitting Criterion  >
# Have a look at the code that computes tree_g, pred_g, conf_g and acc_g. Here, the tree was trained with the Gini impurity criterion, which rpart() uses by default.
# Change the arguments of the rpart() function in the next block of code so that it will split using the information gain criterion. It is coded as "information". The code that calculates pred_i, conf_i and acc_i is already there.
# Draw a fancy plot of tree_g and tree_i using fancyRpartPlot().
# Print out the accuracy of the first model and that of the second model.
# CODE
?rpart
# All packages, emails, train, and test have been pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Train and test tree with gini criterion
tree_g <- rpart(spam ~ ., train, method = "class")
pred_g <- predict(tree_g, test, type = "class")
conf_g <- table(test$spam, pred_g)
acc_g <- sum(diag(conf_g)) / sum(conf_g)

# Change the first line of code to use information gain as splitting criterion
tree_i <- rpart(spam ~ ., train, method = "class", parms = list(split = "information"))
pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$spam, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)

# Draw a fancy plot of both tree_g and tree_i
fancyRpartPlot(tree_g)
fancyRpartPlot(tree_i)

# Print out acc_g and acc_i
print(acc_g)
print(acc_i)

#------------------------------
# < Preprocess the Data  >
# Instruc
# Assign the class label, Survived, of both train and test to separate vectors: train_labels and test_labels.
# Copy the train and test set to knn_train and knn_test.
# Drop the Survived column from knn_train and knn_test.
# Pclass is an ordinal value between 1 and 3. Have a look at the code that normalizes this variable in both the training and the test set. To define the minimum and maximum, only the training set is used; we can't use information on the test set (like the minimums or maximums) to normalize the data.
# In a similar fashion, normalize the Age column of knn_train as well as knn_test. Again, you should only use features from the train set to decide on the normalization! Use the intermediate variables min_age and max_age.
# CODE
# train and test are pre-loaded

# Store the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copy train and test to knn_train and knn_test
knn_train <- train
knn_test <- test


# Drop Survived column for knn_train and knn_test
knn_train <- subset(knn_train, select= -Survived)
knn_test <- subset(knn_test, select= -Survived)

# Normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

# Normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_class) / (max_age - min_age)
#-----------------------------

#--------------------------------
# <  >










#---------------------------------
# <     >