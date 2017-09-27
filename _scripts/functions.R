findEmpty <- function(x){
  sum(length(which(is.na(x) | x == "" | is.null(x))))
}


testBattery <- function(trainingDF, testingDF, uniUsers = uniUsers, finalTestingDF, testMethods = c("rf","gbm"), finalMethod = "rf", samples = NULL, finalPredictionSet = NA, criterion = "classe"){
  # Provide initial method to avoid redoing analysis and instead annotate the existing result
  # Need trainingID to traceback to source when spit by users
  trainingIDs <- 1:dim(trainingDF)[1]
  total <- (length(uniUsers) * length(testMethods))
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i <- 0

  if(!is.na(finalPredictionSet)){
    testingResults <- finalPredictionSet$testing
    trainingResults <- finalPredictionSet$training
    finalTestingResults <- finalPredictionSet$finalTesting
    testMethods <- !(testMethods %in% names(testingResults))
  }
  if(is.na(finalPredictionSet)){
    testingResults <- data.frame(problem_id = testingAnswers$problem_id)
    trainingResults <- data.frame(trainingID = trainingIDs, classe = trainingDF$classe)
    finalTestingResults <- data.frame(problem_id = finalTestingDF$problem_id)
  }
  for (testMethod in testMethods) {
    trainingPredDF <- data.frame()
    testingPredDF <- data.frame()
    finalTestingPredDF <- data.frame()
    for (user in as.character(uniUsers)){
      # result of this is are DFs containing predictions for testing and training subsets
      probID <- testing$problem_id[testing$user_name == user]
      selectedCols <- c(names(trainingDF) %in% names(testingDF) | names(trainingDF) == criterion)
      uniTraining <- trainingDF[trainingDF$user_name == user, selectedCols]
      uniTraining <- uniTraining[,!names(uniTraining) == "user_name"]
      if (!is.na(samples) & samples < dim(uniTraining)[1]){
        set.seed(500)
        sampled <- sample(1:dim(uniTraining)[1], samples, replace = FALSE)
        uniTraining <- uniTraining[sampled,]
      }
      userFit <- train(classe ~ ., data = uniTraining, method = testMethod)
      uniTesting <- testingDF[testingDF$user_name == user,][,!names(testingDF) == "user_name"]
      testingPredDF_temp <- predict(userFit, newdata = uniTesting)
      testingPredDF_temp <- cbind(testingPredDF_temp, problem_id = probID)

      # Do the same for the actual testing set that we are trying to figure out
      uniFinalTesting <- finalTestingDF[finalTestingDF$user_name == user,][,!names(finalTestingDF) == "user_name"]
      finalTestingPredDF_temp <- predict(userFit, newdata = uniFinalTesting)
      finalProbID <- finalTestingDF$problem_id[finalTestingDF$user_name == user]
      finalTestingPredDF_temp <- cbind(finalTestingPredDF_temp, problem_id = finalProbID)
      finalTestingPredDF <- rbind(finalTestingPredDF, finalTestingPredDF_temp)


      # PredDF will retain all predictions for testing set grouped by user and analysis
      finalProbID <- finalTestingDF$problem_id[finalTestingDF$user_name == user]
      testingPredDF <- rbind(testingPredDF, testingPredDF_temp)

      # Tracking predictions batched by users in training for ML on predictions.
      selectedTrainingIDs <- trainingIDs[trainingDF$user_name == user][sampled]
      trainingPredDF_temp <- predict(userFit, newdata = uniTraining)
      trainingPredDF_temp <- cbind(trainingPredDF_temp, trainingID = selectedTrainingIDs)
      trainingPredDF <- rbind(trainingPredDF, trainingPredDF_temp)
      # trainingPredDF contains the predictions for classe if batched by user and analysis type first
      # The missing permutation in this is classe predictions batched by just user but aggregated analyses

      # Updare progress bar
      i <- i + 1
      setTxtProgressBar(pb, i)
    }
    names(testingPredDF) <- c(testMethod, "problem_id")
    names(trainingPredDF) <- c(testMethod, "trainingID")
    names(finalTestingPredDF) <- c(testMethod, "problem_id")

    # Results for each user are modeled and then aggregated
    trainingResults <- merge(trainingResults, trainingPredDF, by = "trainingID")

    # Test battery results for each type of analysis paired with problem_id
    testingResults <- merge(testingResults, testingPredDF, by = "problem_id")
    finalTestingResults <- merge(finalTestingResults, finalTestingPredDF, by = "problem_id")
  }

  #trainingPredictions <- trainingResults
  #testingPredictions <- testingResults
  #finalTestingResults <- finalTestingResults

  # Add in the predictions from when not segmenting
  trainingIDs <- trainingResults$trainingID
#
#    if (!is.na(samples)){
#     set.seed(500)
#     sampled <- sample(1:dim(trainingDF)[1], allSamples, replace = FALSE)
#     sampledTraining <- trainingDF[sampled,]
#     sampledTrainingIDs <- trainingIDs[sampled]
#   } else {sampledTraining <- trainingDF; sampledTrainingIDs <- trainingIDs}

    sampledTraining <- trainingDF[trainingIDs,!names(trainingDF) == "user_name"]

    pb <- txtProgressBar(min = 0, max = total, style = 3)
    i <- 0
  for (testMethod in testMethods){
    fit <- train(classe ~ ., data = sampledTraining, method = testMethod)

    # add to the training table
    pred <- predict(fit, newdata = sampledTraining)
    pred <- data.frame(cbind(pred, trainingID = trainingIDs))
    trainingResults <- merge(trainingResults, pred, by = "trainingID")
    names(trainingResults)[length(names(trainingResults))] <- paste0("all_",testMethod)

    # Add to the sampled testing table
    pred <- predict(fit, newdata = testingDF)
    testingResults <- data.frame(cbind(testingResults, pred))
    # testingResults <- merge(testingResults, pred, by = "problem_id")
    names(testingResults)[length(names(testingResults))] <- paste0("all_",testMethod)

    # add to the final testing table
    pred <- predict(fit, newdata = finalTestingDF)
    finalTestingResults <- data.frame(cbind(finalTestingResults, pred))
    names(finalTestingResults)[length(names(finalTestingResults))] <- paste0("all_",testMethod)

    i <- i + 1
    setTxtProgressBar(pb, i)
  }
    # Now with these predicted, we can run a final model on the aggregate
  trainingResults <- trainingResults[,!(names(trainingResults) %in% c("trainingID","all_fit"))]
  trainingResults <- data.frame(lapply(trainingResults, FUN = function(x){as.factor(as.numeric(x))}))
  trainingResults <- trainingResults[,unlist(lapply(trainingResults, FUN = function(x){length(levels(x)) > 1}))]
  fit <- train(classe ~ ., data = trainingResults, method = finalMethod)
  trainingResults$all_fit <- predict(fit, newdata = trainingResults)

  testingResults <- data.frame(lapply(testingResults, FUN = function(x){as.factor(as.numeric(x))}))
  testingResults$all_fit <- predict(fit, newdata = testingResults)

  finalTestingResults <- data.frame(lapply(finalTestingResults, FUN = function(x){as.factor(as.numeric(x))}))
  finalTestingResults$all_fit <- predict(fit, newdata = finalTestingResults)



#
#
#     # Do the same for the actual testing set that we are trying to figure out
#     uniFinalTesting <- finalTestingDF[finalTestingDF$user_name == user,][,!names(finalTestingDF) == "user_name"]
#     finalTestingPredDF_temp <- predict(userFit, newdata = uniFinalTesting)
#     finalProbID <- finalTestingDF$problem_id[finalTestingDF$user_name == user]
#     finalTestingPredDF_temp <- cbind(finalTestingPredDF_temp, problem_id = finalProbID)
#     finalTestingPredDF <- rbind(finalTestingPredDF, finalTestingPredDF_temp)
#
#
#
#     pred <- predict(fit, newdata = sampledTraining)
#     pred <- data.frame(cbind(pred, trainingID = sampledTrainingIds))
#   }

  finalPredictionSet <- list(training = trainingResults, testing = testingResults, finalTesting = finalTestingResults)
  return(list(training = trainingResults, testing = testingResults, finalTesting = finalTestingResults))
}

getAccuracies <- function(predTable = myTest$testing, criterion = as.factor(as.numeric(testingClasse)), omit = c("problem_id")){
  predTable <- predTable[,!(names(predTable) %in% omit)]
  accuracies <- data.frame(Model = names(predTable), Accuracy = sapply(predTable, FUN = function(x){
    confusionMatrix(x,criterion)$overall[1]
  }))
  rownames(accuracies) <- NULL
  return(accuracies)
}
