# classe is dependent variable

# only 2% are complete cases
sum(complete.cases(training))/dim(training)[1]
# 41% is NA data
sum(is.na(training))/(dim(training)[1] * dim(training)[2])
percentEmpty <- sapply(training, findEmpty)/dim(training)[1]
# 20% is simply blank
sum(training == "", na.rm = TRUE)/(dim(training)[1] * dim(training)[2])

# Remove the columns which are mostly empty which tosses out most of the columns
thresh <- .95
rmColNames <- names(percentEmpty)[percentEmpty >= thresh]
trainingClean <- training[,-which(names(training) %in% rmColNames)]

# Retest for missing data and there is none, it seems
percentEmpty <- sapply(trainingClean, findEmpty)/dim(trainingClean)[1]
summary(percentEmpty)
# All complete cases
sum(complete.cases(trainingClean))/dim(trainingClean)[1]

ggplot(trainingClean, aes(classe ~ pitch_belt)) + geom_point()

qplot(classe, pitch_belt, data = trainingClean, geom = "boxplot")

numClasses <- c("integer","numeric")
trainingClasses <- sapply(trainingClean, class)
trainingNum <- trainingClean[,trainingClasses == "numeric"]
trainingCors <- cor(trainingNum)
meltTrainingCors <- melt(round(trainingCors,2))

g <- ggplot(data = meltTrainingCors, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "White") + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
  midpoint = 0, limit = c(-1,1), space = "Lab")
ggplotly(g)

trainingNumeric <- cbind(trainingNum, classe = trainingClean$classe)
trainingNumericScaled <- data.frame(apply(trainingNum, 2, scale))
trainingNumericScaled <- cbind(trainingNumericScaled, classe = trainingClean$classe)


# Factor analysis to simplify predictors
# Determine the number of factors to break into (8)
ev <- eigen(cor(trainingNum))
ap <- parallel(subject=nrow(trainingNum),var=ncol(trainingNum),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fit_fac <- factanal(trainingNum, 8, rotation = "varimax", lower = .05)
load <- fit_fac$loadings[,1:2]
plot(load, type = "n")
text(load,labels=names(trainingNum),cex=.7)

# Maybe PCA will discriminate better. Shows 2 groups. Third one doesn't hurt.
fit_PCA <- prcomp(trainingNum)
plot(fit_PCA)
PCA_scores <- as.data.frame(fit_PCA$x)
plot(PC1 ~ PC2, data = PCA_scores)
text(PC1 ~ PC2, data = PCA_scores)

PCA_groups <- 3
training_PCA <- cbind(PCA_scores[,1: PCA_groups], classe = trainingClean$classe)


# Add PCAs to testing set
testingClasses <- sapply(testing, class)
testingNum <- testing[,testingClasses == "numeric"]
fit_PCA_testing <- prcomp(testingNum)
testing_PCA <- as.data.frame(fit_PCA_testing$x)

testingNumCrit <- cbind(testing_PCA[,1:PCA_groups], classe = testing$classe)


# Do all of this with the testing set involved from the beginning so that the groups are better defined for the testing set from the get-go. Should keep this DF for use with other analysis for which the testing set would be enrighted by the training set.


### A good way to predict this may be by first seperating out the test subjects. That will control for differences in physiology which may be inducing systematic biases in the readings. From there, we can likely train the model on linear variables for each individual and use that to predict values for each subject in the testing set.

table(trainingClean$classe, trainingClean$user_name)

# For every name in the set, I should create a model for it and apply that to the testing set. Simply parse the training and testing data to retain only a specific username. Run a single model or multiple averaged out. Then rejoin with the testing set using the ID number.

testingClasses <- sapply(testing, class)
testingNum <- testing[,testingClasses == "numeric"]
testingNumericScaled <- data.frame(apply(testingNum, 2, scale))
# testingNumericScaled <- cbind(testingNumericScaled, classe = testing$classe)

trainingDF <- cbind(trainingNumericScaled, user_name = trainingClean$user_name)
testingDF <- cbind(testingNumericScaled, user_name = testing$user_name)
uniUsers <- unique(trainingDF$user_name)
testMethod <- "rf"
criterion <- "classe"
samples <- 300
predDF <- data.frame()
pb <- progress_bar$new(total = length(uniUsers))

for (user in uniUsers){
  probID <- testing$problem_id[testing$user_name == user]
  selectedCols <- c(names(trainingDF) %in% names(testingDF) | names(trainingDF) == criterion)
  uniTraining <- trainingDF[trainingDF$user_name == user, selectedCols]
  uniTraining <- uniTraining[,!names(uniTraining) == "user_name"]
  if (!is.null(samples) & samples < dim(uniTraining)[1]){
     sampled <- sample(1:dim(uniTraining)[1], samples, replace = FALSE)
     uniTraining <- uniTraining[sampled,]
   }
  userFit <- train(classe ~ ., data = uniTraining, method = testMethod)
  uniTesting <- testingDF[testingDF$user_name == user,][,!names(testingDF) == "user_name"]
  predDF_temp <- predict(userFit, newdata = uniTesting)
  predDF_temp <- cbind(predDF_temp, problem_id = probID)
  predDF <- rbind(predDF, predDF_temp)

  for (i in 1:length(uniUsers)) {
    pb$tick()
    Sys.sleep(1 / 100)
  }
}

answers <- predDF[order(predDF$problem_id),]
confusionMatrix(as.factor(answers$predDF_temp), as.factor(as.numeric(testingClasse)))
# Forecasts would be a good way to take the changes over time into consideration. However, if the behavior is consistent for each, a time related practice effect is probably not apparent.

tests <- c("gbm", "rf", "treebag","elm","svm")

myTest <- testBattery(trainingDF, testingDF, uniUsers, finalTestingDF = testing_origin, testMethods = tests, finalMethod = "svm", samples = 500, finalPredictionSet = NA, criterion = "classe")
saveRDS(myTest, "myTest_svm.RDS")

confusionMatrix(as.factor(myTest$training$rf),as.factor(as.numeric(myTest$training$classe)))
confusionMatrix(as.factor(myTest$training$gbm),as.factor(as.numeric(myTest$training$classe)))
confusionMatrix(as.factor(myTest$training$all_fit),as.factor(as.numeric(myTest$training$classe)))


testing_pred <- as.factor(myTest$testing$elm)[order(myTest$testing$problem_id)]
confusionMatrix(testing_pred, testingAnswers$classe)

testing_pred <- as.factor(myTest$testing$all_fit)[order(myTest$testing$problem_id)]
confused <- confusionMatrix(testing_pred, as.factor(as.numeric(testingAnswers$classe)))


answers <- myTest$finalTesting

# For each prediction, run a confusionmatrix and pull the accuracy from those and pair up
accuracy <- getAccuracies()
thresh <- .85
modelsKept <- c(as.character(accuracy$Model[accuracy$Accuracy >= thresh]), "classe")
postTrainDF <- myTest$training[,modelsKept]
post_fit <- train(classe ~ ., data = postTrainDF, method = "rf")

post_fit_pred <- predict(post_fit, myTest$testing)
confusionMatrix(post_fit_pred,testingAnswers$classe)

cbind(myTest$finalTesting$problem_id,predict(post_fit, myTest$finalTesting))


str(confused)
confused$overall[1]
Accuracy

predict(fit_rf, myTest$finalTesting)
