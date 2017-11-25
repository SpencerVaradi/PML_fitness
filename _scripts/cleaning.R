# Identify cols that are not measures. More record info.
recCols <- c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")

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


# Commonly missing data colomns appear to be aggregates
trainingAgg <- cbind(training[recCols],as.data.frame(training[rmColNames]))

# New windows are where to find that info
table(trainingAgg$new_window,is.na(trainingAgg$max_roll_belt))

# Also the only complete cases. So let's get those.
sum(complete.cases(trainingAgg))
trainingAgg <- trainingAgg[complete.cases(trainingAgg),]


trainingAggClasses <- sapply(trainingClean, class)

fakeDB <- trainingAgg
for (col in names(trainingAgg[rmColNames])) {
  if (class(trainingAgg[col]) == "factor") {
    fakeDB[col] <- as.numeric(trainingAgg[col])

  }
}

str(trainingAgg)
