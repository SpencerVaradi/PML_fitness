# Remove the columns which are mostly empty which tosses out most of the columns
thresh <- .95
percentEmpty <- sapply(training, findEmpty)/dim(training)[1]
rmColNames <- names(percentEmpty)[percentEmpty >= thresh]
trainingClean <- training[,-which(names(training) %in% rmColNames)]

trainingClasses <- sapply(trainingClean, class)
trainingNum <- trainingClean[,trainingClasses == "numeric"]
trainingNumeric <- cbind(trainingNum, classe = trainingClean$classe)
trainingNumericScaled <- data.frame(apply(trainingNum, 2, scale))
trainingNumericScaled <- cbind(trainingNumericScaled, classe = trainingClean$classe)

testingClasses <- sapply(testing, class)
testingNum <- testing[,testingClasses == "numeric"]
testingNumericScaled <- data.frame(apply(testingNum, 2, scale))
# testingNumericScaled <- cbind(testingNumericScaled, classe = testing$classe)

trainingDF <- cbind(trainingNumericScaled, user_name = trainingClean$user_name)
testingDF <- cbind(testingNumericScaled, user_name = testing$user_name)
uniUsers <- unique(trainingDF$user_name)
