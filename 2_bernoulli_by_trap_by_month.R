rm(list=ls())


library(dplyr)
library(ggplot2)
library(ggmap)

train <- read.csv("./train.csv/train.csv", header = TRUE)
test <- read.csv("./test.csv/test.csv", header = TRUE)
train$WnvPresent <- factor(train$WnvPresent)


# Lets make an extremely naive model:
#   - For each trap in the training data, determine
#     the wnv rate (# of positive tests/total observations)
#   - For the test data, let the outcome be that of a bernoulli
#     trail with P equal to that of the trap in the training data,
#     and 0 otherwise.

trap_probs <- train %>% group_by(Trap) %>% summarise(wnv_prob = sum(WnvPresent == 1)/n())
results1 <- test %>% left_join(.,trap_probs) %>% mutate(wnv_prob = ifelse(is.na(wnv_prob),0,wnv_prob)) %>% 
  do({
    wnv_present <- rbinom(.$Id,1,.$wnv_prob)
    data.frame(Id = .$Id,WnvPresent = wnv_present)
  })

write.csv(results1,'./submissions/1_naive_bernoulli.csv',row.names = FALSE)

# A bit more complex:
# Calculate the incidence rate for each trap for each month

probs <- train %>% mutate(month = months(as.Date(Date))) %>%
  group_by(Trap,month) %>%
  summarise(wnv_prob = sum(WnvPresent == 1)/n())

results2 <- test %>% mutate(month = months(as.Date(Date))) %>%
  left_join(.,probs,by = c("Trap" = "Trap","month" = "month")) %>% 
  mutate(wnv_prob = ifelse(is.na(wnv_prob),0,wnv_prob)) %>%
  do({
    wnv_present <- rbinom(.$Id,1,.$wnv_prob)
    data.frame(Id = .$Id,WnvPresent = wnv_present)
  })

write.csv(results2,'./submissions/2_naive_bernoulli_by_month.csv',row.names = FALSE)