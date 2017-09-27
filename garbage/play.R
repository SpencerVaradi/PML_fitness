# ballpark off the cuff guess

sampleSize <- 2000
sampled <- sample(1:dim(trainData)[1], sampleSize,replace = FALSE)
trainData <- trainingDF

fit_rf <- train(classe ~ ., data = trainData, method = "rf")
fit_svm <- train(classe ~ ., data = trainingNumericScaled, method = "svm")
fit_lasso <- train(classe ~ ., data = trainData, method = "lasso")
fit_gbm <- train(classe ~ ., data = trainingNumericScaled, method = "gbm")
fit_lda <- train(class ~ ., data = trainData, method = "lda")
fit_glm <- train(classe ~ ., data = trainData, method = "glm")
fit_glm2 <- glm(classe ~ ., data = trainData)

trainData <- training_PCA

# These are pretty fat models. Too long to run. Going to use only numeric predictors first to slim down
fit_rf <- train(classe ~ ., data = trainData, method = "rf")
fit_svm <- train(classe ~ ., data = trainData, method = "svm")
fit_lasso <- train(classe ~ ., data = trainData, method = "lasso")
fit_gbm <- train(classe ~ ., data = trainData, method = "gbm")
fit_lda <- train(class ~ ., data = trainData, method = "lda")
fit_glm <- train(classe ~ ., data = trainData, method = "glm")

pred_rf <- predict(fit_rf,newdata = testing_origin)
pred_svm <- predict(fit_svm,newdata = testing)
pred_lasso <- predict(fit_lasso,newdata = testing)
pred_gbm <- predict(fit_gbm,newdata = testing)
pred_lda <- predict(fit_lds,newdata = testing)

combo <- data.frame(pred_rf, pred_svm, pred_lasso, pred_gbm, pred_lda, classe = testing$classe)

confusionMatrix(pred_gbm, testing$classe)
