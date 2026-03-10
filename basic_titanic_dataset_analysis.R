#  Basic Titanic Analysis
#  Jignesh Trivedi  
#  2026-03-05

#  Header Section (Installing [if required] and Loading a required libraries)  ----

#  Checking if the user has the required packages installed, if not; it installs the required packages
if(!require("tidyverse"))install.packages("tidyverse")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("dplyr"))install.packages("dplyr")
if(!require("caret"))install.packages("caret")

tidyverse::library(tidyverse)   # Loads a collection of packages used for data science and analysis.
ggplot2::library(ggplot2)       # Used to create  graphs
deplyr::library(dplyr)          # filter(), select(), mutate(), summarise()
caret::library(caret)           # helps with data splitting, model evaluation, confusion matrix

#  Loading the dataset (Titanic Dataset for this project)  ----

data("Titanic")

#  Converting the Titanic dataset to Data Frame  ----

titanic <- base::as.data.frame(Titanic) 

#  Analyzing the data from the set ----

utils::head(Titanic)   # To view the first six rows of the Titanic data set 

utils::str(Titanic)  # This shows us the types of variables, number of rows and columns in the dataset

base::summary(Titanic)  # This gives the counts and statistics for each column

#  Calculating the survivors
stats::aggregate(Freq ~ Survived, data = titanic, FUN = sum)


# Visualising the DataSet ----

ggplot2::ggplot(titanic, ggplot2::aes(x=Sex, y=Freq, fill=Survived)) + 
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::labs(title="Survival by Gender")

titanic_full <- titanic[base::rep(1:base::nrow(titanic), titanic$Freq), ]
      #Expanding the dataSet to repeat the rows so that each passenger becomes one row

titanic_full$Freq <- NULL  # Removing the frequency column

# Using caret package to Train/Test Split  ----

base::set.seed(123)
train_index <- caret::createDataPartition(titanic_full$Survived, p=0.7, list=FALSE)
      # set.send() <- ensures same random result
      # createDataPartition <- splits data
      # p=0.7 <- 70% training data

# Creating the training dataSet  ----

train <-  titanic_full[train_index, ]
test <- titanic_full[-train_index, ]   #  the "-" sign here signifies that all rows are not in the training set

#  Logistic Regression Model  ----

model <- stats::glm(Survived ~ Class + Sex +Age, data = train,family = "binomial")
      # glm() <- generalized linear model
      # Survived ~ Class + Sex + Age <- predictors
      # samily = "binomial" <-  logistic regression

base::summary(model)  # coefficients, p-values and significance

#  Making predctions now  ----

predictions <- stats::predict(model, test, type = "response")  # This returns the probability of survival

predicted_class <- base::ifelse(predictions > 0.5, "Yes", "No")
      #This converts the probability from percentage to Yes or No

# Creating a confusion matrix ----

caret::confusionMatrix(
  base::as.factor(predicted_class),
  base::as.factor(test$Survived)
)

