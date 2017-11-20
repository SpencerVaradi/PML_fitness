reqPacks <- c("caret","ggplot2","forecast","rpart","lubridate","corrplot","reshape2","plotly","nFactors","mlbench","progress", "randomForest", "e1071"
)

for (reqPack in reqPacks){
  if (!(reqPack %in% installed.packages()[,1])){
    install.packages(reqPack)
  }
}

library(caret)
library(ggplot2)
library(forecast)
library(rpart)
library(lubridate)
library(corrplot)
library(reshape2)
library(plotly)
library(nFactors)
library(mlbench)
library(progress)
library(randomForest)
library(e1071)

training_origin <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = TRUE) #, stringsAsFactors = FALSE)
training_origin <- training_origin[,-1]
testing_origin <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header = TRUE) #, stringsAsFactors = FALSE)
testing_origin <- testing_origin[,-1]

###### Create training and testing subsets
set.seed(500)
inTrain <- createDataPartition(training_origin$classe, p = 3/4)[[1]]

# training <- training_origin
# testing <- testing_origin


# I thought originally that this was to be tested without prior knowledge. Changed to forget that
training <- training_origin[inTrain,]
testing <- training_origin[-inTrain,]
trainingClasse <- training$classe

# Hold onto the classe before we chuck it
testingClasse <- testing$classe
testingAnswers <- data.frame(cbind(problem_id = 1:dim(testing)[1], classe = testingClasse))
# Get rid of columns that we do not have the luxery of in the testing data
testing <- testing[,names(testing) %in% names(testing_origin)]
# Since we will be pulling the testing set apart a little bit, IDs will make rejoining easier
testing$problem_id <- 1:dim(testing)[1]

###### Full set for PCA and clusterings
# fullDF_origin <- rbind(training_origin[,1:ncol(training_origin)-1],testing_origin)
# names(testing_origin) != names(training_origin)

###### Functions
source("_scripts/functions.R")

# range(testing$cvtd_timestamp) - range(training$cvtd_timestamp)

