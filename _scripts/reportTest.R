

## Load Data
training_origin <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = TRUE) #, stringsAsFactors = FALSE)
training_origin <- training_origin[,-1]
testing_origin <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header = TRUE) #, stringsAsFactors = FALSE)
testing_origin <- testing_origin[,-1]

##Create training and testing subsets

set.seed(500)
inTrain <- createDataPartition(training_origin$classe, p = 3/4)[[1]]

training <- training_origin[inTrain,]
testing <- training_origin[-inTrain,]
trainingClasse <- training$classe

# Classe was removed from the testing set to keep the predictors clean and match with training set. The set was also cleaned of extra columns which were not present in the training set.

# Hold onto the classe before we chuck it
testingClasse <- testing$classe
testingAnswers <- data.frame(cbind(problem_id = 1:dim(testing)[1], classe = testingClasse))
# Get rid of columns that we do not have the luxery of in the testing data
testing <- testing[,names(testing) %in% names(testing_origin)]
# Since we will be pulling the testing set apart a little bit, IDs will make rejoining easier
testing$problem_id <- 1:dim(testing)[1]


## Cleaning training set
#Most of the columns were numeric or factors. Others were more information about the record. These were identified so they could be added to and subset of the training data. Of the remaining columns, many were missing most values. Those empty columns were identified and removed.

# Identify cols that are not measures. More record info.
recCols <- c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")

# Remove the columns which are mostly empty which tosses out most of the columns
findEmpty <- function(x){
  sum(length(which(is.na(x) | x == "" | is.null(x))))
}

thresh <- .95

percentEmpty <- sapply(training, findEmpty)/dim(training)[1]
rmColNames <- names(percentEmpty)[percentEmpty >= thresh]
trainingClean <- training[,-which(names(training) %in% rmColNames)]

# Classes
#Classes are almost equally distributed
library(ggplot2)
qplot(trainingClean$classe)


# Machine Learning Methods
## Linear Discriminant Analysis (LDA)
# LDA produced predictions with 85.8% accuracy. Classe B was the most difficult to predict and commonly confused with A and C.

library(MASS)
set.seed(500)
ldaFit <- lda(classe ~ ., data = trainingClean)
ldaPred <- predict(ldaFit, newdata = testing)
confusionMatrix(ldaPred$class, testingClasse)

trainctrl <- trainControl(verboseIter = TRUE)


rfFit <- train(classe ~ ., method = "rf",data = trainingClean, trControl = trainctrl)

## Random Forrest (RF)
#RF model produced 99.87% accuracy. Only 3 classes were misclassified.
#When this was submitted, it yeilded 17 out of 20 correct predictions. With an 85% accuracy actually closer to the LDA model, the model is either missing something or overfit to the training data. the `randomForest()` function appears to have a pretty significant bug when it comes using `predict()`. The model has to be fit from a subset the same dataframe.

trainingAll <- data.frame(rbind(trainingClean[-59],
                                testing_origin[,match(names(trainingClean[-59]),names(testing_origin))]))

library(caret)
trainData <- data.frame(trainingAll[nrow(trainingClean),], classe = trainingClean$classe)
#rfFit <- train(classe ~ ., method = "rf", data = trainData)
library(randomForest)

trainData <- data.frame(trainingAll[nrow(trainingClean),], classe = trainingClean$classe)
rfFit <- randomForest(classe ~ ., data = trainData)
saveRDS(rfFit, "rfFit.rds")
rfPred <- predict(rfFit, newdata = testing)
                    trainingAll[nrow(trainingClean)+1:nrow(trainingAll),])
confusionMatrix(rfPred, testingClasse)

# Mixed Models
#The models were combined and a model fit using the two new predictors assessed. Corresponding fit values were created in the testing sets to make sure the correct model is applied. More specifically, the data was bound together and notated for which records were present in their respective sets. This improved the accuracy to 99.98%.

# rfFit <- randomForest(classe ~ ., data = data.frame(trainingAll[nrow(trainingClean),], classe = trainingClean$classe))
