rm(list=ls())

library(dplyr)
library(h2o)

# August 2007 and August 2009 have outbreaks.

data <- read.csv('../data/joined_data.csv',header = TRUE)
remove <- c('NumMosquitos','Address','Street','AddressAccuracy','Sunrise','Sunset','ResultSpeed','ResultDir')
categoricals <- c('Year','Week','Species','Block','AddressNumberAndStreet','WnvPresent')
numerics <- c('Latitude','Longitude')

data[numerics] <- lapply(data[numerics],as.numeric)
data[categoricals] <- lapply(data[categoricals],as.factor)
data <- data[,!(names(data) %in% remove)]

localTrain <- data %>% filter(Dataset == "Train")
localTest  <- data %>% filter(Dataset == "Test")


# Simple model: Year, Month, Tmax, Depart, PrecipTotal

#features <- match(c('Year','Month','Tmax','Depart','Heat','Cool','PrecipTotal','SprayIntensity'),names(data))
features <- 4:27
target <- match(c('WnvPresent'),names(data))

localH2O = h2o.init(nthreads = -1, max_mem_size = "8G")

train <- as.h2o(localH2O,localTrain,header = TRUE,sep = ',',key = 'train.hex')
test <- as.h2o(localH2O,localTest,header = TRUE,sep = ',', key = 'test.hex')

#First, we'll do some hyperparameter tuning with a random search
tuning_models <- c()
n_iter <- 15
for (i in seq(n_iter)) {
  print(paste0("Training model ",i," of ",n_iter))
  
  rand_alpha <- c(0,0.25,0.5,0.75,1)[sample(1:5,1)]
  rand_standardize <- c(TRUE,FALSE)[sample(1:2,1)]
  model <- h2o.glm(
    x = features,
    y = target,
    data = train,
    family = 'binomial',
    alpha = rand_alpha,
    standardize = rand_standardize
  )
  tuning_models <- c(tuning_models, model)
}

best_model <- tuning_models[[1]]
best_auc <- best_model@model$auc
for (model in tuning_models) {
  if (model@model$auc > best_auc) {
    best_auc <- model@model$auc
    best_model <- model
  }
}
best_model
best_params <- best_model@model$params
best_params

years <- c('2007','2009','2011','2013')
crossval_statistics <- c()

for (year in years) {
  this_year_data   <- train[, (train$Year == year)]
  other_years_data <- train[,!(train$Year == year)]
  model <- h2o.glm(
    x           = features, 
    y           = target,
    data        = other_years_data,
    family      = 'binomial',
    alpha       = best_params['alpha'][[1]]
  )
  crossval_statistics <- c(
    crossval_statistics,
    h2o.performance(
      data = h2o.predict(object = model, newdata = this_year_data)[,3L], 
      reference = this_year_data[,target]))
}
aucs <- sapply(crossval_statistics,function(x){x@model$auc})
print(paste0("Cross Validation AUC: ", aucs %>% mean()))

model <- h2o.glm(
  x           = features, 
  y           = target,
  data        = train,
  family      = 'binomial',
  alpha       = best_params['alpha'][[1]]
)
model

localTest$PWnv <- as.data.frame(h2o.predict(object = model, newdata = test))$X1
plot(localTest$YearMonth,localTest$PWnv)
localTest$NormWnv <- localTest$PWnv / max(localTest$PWnv)

write.csv((localTest %>% transmute(Id = seq(nrow(localTest)),WnvPresent = NormWnv)),file = "../submissions/35_glm.csv",row.names = FALSE)
