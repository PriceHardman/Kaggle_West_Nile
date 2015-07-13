rm(list=ls())

library(dplyr)
library(h2o)
library(h2oEnsemble)
library(SuperLearner)
library(cvAUC)

data <- read.csv('../data/binarized_data.csv',header = TRUE)
data[,'WnvPresent'] <- as.factor(data[,'WnvPresent'])

# Indices of features and targets.
features <- 4:137
target <- 3

# Train test split
local_train <- data %>% filter(Dataset == "Train")
local_test  <- data %>% filter(Dataset == "Test")

# Instantiate the H2O server and upload train and test to it.
localH2O  = h2o.init(nthreads = -1,max_mem_size = '8G')
train <- as.h2o(
  localH2O ,local_train,header = TRUE,sep = ',',key = 'train.hex')
test <- as.h2o(
  localH2O ,local_test,header = TRUE,sep = ',', key = 'test.hex')

rf.1 <- function(...,ntree = 100, sample.rate = 0.5) {
  h2o.randomForest.wrapper(..., ntrees = ntrees, ssample.rate = sample.rate)
}
rf.2 <- function(...,ntree = 200,nbins = 100, sample.rate = 0.75) {
  h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, sample.rate = sample.rate)
}
# deeplearn.1 <- function(...,hidden = c(500,500), activation = "MaxoutWithDropout",balance_classes = TRUE){
#   h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes)
# }
# deeplearn.2 <- function(...,hidden = c(1024,1024,1024), 
#                         activation = "RectifierWithDropout",seed = 1,l1=1e-5,epochs = 100){
#   h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed,l1=l1,epochs=epochs)
# }

learner <- c("rf.1","rf.2")#,"deeplearn.1",'deeplearn.2')
metalearner <- c("SL.glm")

model <- h2o.ensemble(
  x = features, y = target, data = train, 
  family = "binomial", learner = learner, metalearner = metalearner,
  cvControl = list(V=5)
)

validation <- predict(model,train)
AUC(predictions=as.data.frame(
  validation$pred)[,1], labels=as.data.frame(train[,3])[,1])


pred <- predict(model, test)

output <- data.frame(Id = seq(pred$pred), WnvPresent = pred[,3])
#write.csv(x = output, file = "../submissions/27_ensemble.csv",row.names = FALSE)
