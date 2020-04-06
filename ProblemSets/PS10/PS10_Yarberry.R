#PS10 Yarberry 

# Question 4 
income <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
income.train <- income[train,]
income.test <- income[test, ]

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Question 5 
library(mlr)
library('rpart')

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.rpart")


# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

rpart(finalModel, data=income.train, control=rpart.control(minsplit=10))

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Logistic regression

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.glmnet")


# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Neural network
library('nnet')

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.nnet")

# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Naive Bayes

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.naiveBayes")

# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# kNN
library('kknn')

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.kknn")

# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$betanalModel)$beta

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# SVM
library('e1071')

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = income.train, target = "high.earner")
print(theTask)

# tell mlr what prediction algorithm we'll be using
predAlg <- makeLearner("classif.svm")

# Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 3 folds
print(sampleResults$aggr)

finalModel <- train(learner = predAlg, task = theTask)
prediction <- predict(finalModel, newdata = income.test, type = response)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
  return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$coefficients

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = income.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$betanalModel)$beta