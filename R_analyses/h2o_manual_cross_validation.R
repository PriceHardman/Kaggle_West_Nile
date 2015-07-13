rm(list=ls())

library(dplyr)
library(h2o)
library(unbalanced)

data <- read.csv('../data/joined_data.csv',header = TRUE)
remove <- c('NumMosquitos','Address','Street','AddressAccuracy','Depart','Sunrise','Sunset','ResultSpeed','ResultDir','SprayIntensity')
categoricals <- c('Year','Week','Species','Block','AddressNumberAndStreet','WnvPresent')
numerics <- c('Latitude','Longitude')

data[numerics] <- lapply(data[numerics],as.numeric)
data[categoricals] <- lapply(data[categoricals],as.factor)
data <- data[,!(names(data) %in% remove)]

localTrain <- data %>% filter(Dataset == "Train")
localTest  <- data %>% filter(Dataset == "Test")

features <- 4:25
target <- 26

# smote <- ubSMOTE(localTrain[,-target],localTrain[,target],perc.over = 1800)
# localTrain <- data.frame(smote$X,smote$Y)

# train covers years {2007,2009,2011,2013}.
# We'll try two different model validation methods:
#   - First, we'll perform one-year-out cross validation to
#     try to get an accurate AUC, then train on all data, and predict on test.

#   - Next, we'll actually train models separately for each year of training data,
#     and predict on test for each of them. Our prediction will be the average.



# Instantiate the H2O server
localH2O = h2o.init(nthreads = -1, max_mem_size = "8G")

train <- as.h2o(localH2O,localTrain,header = TRUE,sep = ',',key = 'train.hex')
test <- as.h2o(localH2O,localTest,header = TRUE,sep = ',', key = 'test.hex')



# First, we'll do some hyperparameter tuning with a random search
# tuning_models <- c()
# n_iter <- 50
# for (i in seq(n_iter)) {
#   print(paste0("Training model ",i," of ",n_iter))
#   
#   rand_ntree <- c(50,100,250,500,1000)[sample(1:5,1)]
#   rand_depth <- c(10,20,40,80,100)[sample(1:5,1)]
#   rand_mtries <- c(-1,10,20)[sample(1:3,1)]
#   rand_sample.rate <- c(0.1,0.25,0.5,0.75,0.9)[sample(1:5,1)]
#   rand_nbins <- c(5,10,15,20,25,30,35,40)[sample(1:8,1)]
#   model <- h2o.randomForest(
#     x = features, y = target, data = train, 
#     ntree = rand_ntree, depth = rand_depth, mtries = rand_mtries,
#     sample.rate = rand_sample.rate, nbins = rand_nbins
#   )
#   tuning_models <- c(tuning_models, model)
# }

# best_model <- tuning_models[[1]]
# best_auc <- best_model@model$auc
# for (model in tuning_models) {
#   if (model@model$auc > best_auc) {
#     best_auc <- model@model$auc
#     best_model <- model
#   }
# }
# best_model
# best_params <- best_model@model$params
# best_params


years <- c('2007','2009','2011','2013')
crossval_statistics <- c()

for (year in years) {
  this_year_data   <- train[, (train$Year == year)]
  other_years_data <- train[,!(train$Year == year)]
  model <- h2o.randomForest(
    x           = features, 
    y           = target,
    data        = other_years_data,
    ntree       = 1000,#best_params[['ntree']], # 1000
    depth       = 20,#best_params[['depth']], # 20
    mtries      = 20,#best_params[['mtries']],# 20
    sample.rate = 0.1,#best_params[['sample.rate']], # 0.1
    nbins       = 1024,#best_params[['nbins']] # 1024
    seed        = 8.566502e+18
  )
  crossval_statistics <- c(
    crossval_statistics,
    h2o.performance(
      data = h2o.predict(object = model, newdata = this_year_data)[,3L], 
      reference = this_year_data[,target]))
}
aucs <- sapply(crossval_statistics,function(x){x@model$auc})
print(paste0("Cross Validation AUC: ", aucs %>% mean()))

best_model <- h2o.randomForest(
  x           = features, 
  y           = target,
  data        = train,
  ntree       = 1000,#best_params[['ntree']], # 1000
  depth       = 20,#best_params[['depth']], # 20
  mtries      = 20,#best_params[['mtries']],# 20
  sample.rate = 0.1,#best_params[['sample.rate']], # 0.1
  nbins       = 1024#best_params[['nbins']] # 1024
  #seed        = 8.566502e+18
)
best_model

prediction <- as.data.frame(h2o.predict(object = best_model, newdata = test))

single_model_out <- data.frame(Id = seq(nrow(prediction)), WnvPresent = prediction$X1)
write.csv(single_model_out,"../submissions/33_year_out_grid_search_random_forest.csv",row.names = FALSE)
#h2o.shutdown(localH2O,FALSE)
