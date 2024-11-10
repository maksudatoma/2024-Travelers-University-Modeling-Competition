setwd("C:\\Users\\aarif\\OneDrive\\Desktop\\PhD Statistics\\STAT 823\\Project")
install.packages("caret")
library(caret)
library(dplyr)

trav <- read.csv("train_data.csv")
summary(trav)
#Exclude first column (ID column)
trav <- trav[,-1]

trav <- trav %>%
  mutate(across(where(is.character), as.factor))

View(trav)

#creating Train and Test Sets

trainIndex <- createDataPartition(trav$call_counts, p = 0.8, list = FALSE)
train_data <- trav[trainIndex, ]
test_data <- trav[-trainIndex, ]


#Checking whether stratification was successful
mean(train_data$call_counts)
mean(test_data$call_counts)




#ZIP Model
install.packages("pscl")
library(pscl)
train_data$household_group

zip_model <- zeroinfl(call_counts ~ X12m_call_history + acq_method + newest_veh_age+ann_prm_amt+household_group | household_policy_counts + digital_contact_ind + tenure_at_snapshot+trm_len_mo,
                      data = train_data, dist = "negbin")


zip_model$coefficients

zip_pred <- predict(zip_model, newdata = test_data, type = "response")
rmse6 <- sqrt(mean((zip_pred - test_data$call_counts)^2))
rmse6
pred6 <- data.frame(test_data$call_counts, data.frame(zip_pred))
print(pred6)






#Random Forest Model

library(randomForest)

rf.poisson <- randomForest(call_counts ~ ., data = train_data, importance = T, ntree = 50, mtry = 4, nodesize=20, maxnodes=NULL)

rf.poissonpred <- predict(rf.poisson, newdata = test_data, type = "response")

# Evaluate model performance using RMSE
rmse7 <- sqrt(mean((rf.poissonpred - test_data$call_counts)^2))
rmse7

pred7 <- data.frame(test_data$call_counts, data.frame(rf.poissonpred))
print(pred7)


#Random Forest with train function

set.seed(161)
rf.cv1 <-train(call_counts~., data = train_data, method="rf",
               trControl=trainControl(method = "repeatedcv", number = 5, repeats = 3),
               metric="RMSE",tuneGrid=expand.grid(mtry=c(1:4)), ntree=100)



#GBM Model
library(gbm)
gbm.poisson <- gbm(call_counts ~ ., data = train_data, distribution="poisson", n.tree = 500, interaction.depth=7, shrinkage=0.01,n.minobsinnode=20,bag.fraction=1)

gbm.poissonpred <- predict(gbm.poisson, newdata = test_data, type = "response")
# Evaluate model performance using RMSE
rmse8 <- sqrt(mean((gbm.poissonpred - test_data$call_counts)^2))
rmse8

pred8 <- data.frame(test_data$call_counts, data.frame(gbm.poissonpred))
print(pred8)


#Predicting on the Test Set
travtst <- read.csv("test_data.csv")

summary(travtst)

#Exclude first column (ID column)
travtst2 <- travtst[,-1]

travtst2 <- travtst2 %>%
  mutate(across(where(is.character), as.factor))


gbm.poissonpred <- predict(gbm.poisson, newdata = travtst2, type = "response")

subfile <- data.frame(travtst$id,gbm.poissonpred)

write.csv(subfile, file = "C:\\Users\\aarif\\OneDrive\\Desktop\\2024-Travelers-University-Modeling-Competition\\DataTest3.csv", row.names=TRUE)





#GBM Model with train function
gbm.cv1 <- train(call_counts ~ ., data = train_data, method="gbm",
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3), metric="RMSE",
                 tuneGrid=expand.grid(n.trees=seq(100,500,100), interaction.depth=5,shrinkage=seq(0.01,0.10,0.01),
                                      n.minobsinnode=100), bag.fraction=1, verbose=F, distribution="poisson")


gbm.cv1pred <- predict(gbm.cv1, newdata = test_data, type = "raw")
# Evaluate model performance using RMSE
rmse9 <- sqrt(mean((gbm.cv1pred - test_data$call_counts)^2))
rmse9





#Try Imputation
install.packages("mice")
library(mice)

trav3 <- read.csv("train_data.csv")
summary(trav3)
#Exclude first column (ID column)
trav3 <- trav3[,-1]

trav3 <- trav3 %>%
  mutate(across(where(is.character), as.factor))

trav3[trav3 == -2 |trav3 == -20 | trav3 == "missing"] <- NA


trav3 = complete(mice(trav3, method = "pmm"))

summary(trav3)

trav3$acq_method <- factor(trav3$acq_method, levels = setdiff(levels(trav3$acq_method), "missing"))



#creating Train and Test Sets

trainIndex <- createDataPartition(trav3$call_counts, p = 0.7, list = FALSE)
train_data2 <- trav3[trainIndex, ]
test_data2 <- trav3[-trainIndex, ]


#Checking whether stratification was successful
mean(train_data2$call_counts)
mean(test_data2$call_counts)


#Random Forest Model

library(randomForest)

rf.poisson <- randomForest(call_counts ~ ., data = train_data2, importance = T, ntree = 50, mtry = 4, nodesize=20, maxnodes=NULL)
rf.poissonpred <- predict(rf.poisson, newdata = test_data2, type = "response")

# Evaluate model performance using RMSE
rmse9 <- sqrt(mean((rf.poissonpred - test_data2$call_counts)^2))
rmse9


#Zero inflated Model
model_zinb <- zeroinfl(
  call_counts ~
    # Count model (negative binomial part)
    ann_prm_amt + tenure_at_snapshot + digital_contact_ind +
    household_policy_counts + geo_group + channel +
    has_prior_carrier + trm_len_mo |
    # Zero-inflation model (logistic part)
    digital_contact_ind + household_policy_counts + tenure_at_snapshot,
  dist = "negbin",
  data = train_data2
)


model_zinb <- zeroinfl(
  call_counts ~.-ann_prm_amt-digital_contact_ind-home_lot_sq_footage-newest_veh_age-tenure_at_snapshot-trm_len_mo|
    # Zero-inflation model (logistic part)
    digital_contact_ind + household_policy_counts +
    tenure_at_snapshot + telematics_ind + acq_method,
  dist = "negbin",
  data = train_data2
)


zip_pred <- predict(model_zinb, newdata = test_data2, type = "response")
rmse11 <- sqrt(mean((zip_pred - test_data2$call_counts)^2))
rmse11



#Hurdle Model
model_hurdle <- hurdle(
  call_counts ~
    ann_prm_amt + tenure_at_snapshot + digital_contact_ind +
    household_policy_counts + geo_group + channel +
    has_prior_carrier + trm_len_mo + newest_veh_age |
    digital_contact_ind + household_policy_counts +
    tenure_at_snapshot + telematics_ind + acq_method,
  dist = "negbin",
  data = train_data2
)

hrdl_pred <- predict(model_hurdle, newdata = test_data2, type = "response")
rmse12 <- sqrt(mean((hrdl_pred - test_data2$call_counts)^2))
rmse12





#GBM Model

train_data2$trm_len_mo


library(gbm)

gbm.poisson <- gbm(call_counts ~ ., data = train_data2, distribution="poisson", n.tree = 500, interaction.depth=7, shrinkage=0.01,n.minobsinnode=20,bag.fraction=1)

gbm.poisson <- gbm(call_counts ~ .-ann_prm_amt-digital_contact_ind-home_lot_sq_footage-newest_veh_age-tenure_at_snapshot-trm_len_mo, data = train_data2, distribution="poisson", n.tree = 1500, interaction.depth=7, shrinkage=0.01,n.minobsinnode=20,bag.fraction=1)

gbm.poissonpred <- predict(gbm.poisson, newdata = test_data2, type = "response")
# Evaluate model performance using RMSE
rmse10 <- sqrt(mean((gbm.poissonpred - test_data2$call_counts)^2))
rmse10

gbm.poissonpred <- predict(gbm.poisson, newdata = train_data2, type = "response")
rmse110 <- sqrt(mean((gbm.poissonpred - train_data2$call_counts)^2))
rmse110





#Predicting on the Test Set
travtst <- read.csv("test_data.csv")

summary(travtst)

#Exclude first column (ID column)
travtst2 <- travtst[,-1]

travtst2 <- travtst2 %>%
  mutate(across(where(is.character), as.factor))


travtst2[travtst2 == -2 |travtst2 == -20 | travtst2 == "missing"] <- NA


travtst2 = complete(mice(travtst2, method = "pmm"))

summary(travtst2)

travtst2$acq_method <- factor(travtst2$acq_method, levels = setdiff(levels(travtst2$acq_method), "missing"))



gbm.poissonpred <- predict(gbm.poisson, newdata = travtst2, type = "response")

subfile <- data.frame(travtst$id,gbm.poissonpred)

write.csv(subfile, file = "C:\\Users\\aarif\\OneDrive\\Desktop\\2024-Travelers-University-Modeling-Competition\\DataTest4.csv", row.names=TRUE)














#Permutation test

trav_num <- data.frame(trav$X12m_call_history,trav$ann_prm_amt,trav$digital_contact_ind,trav$home_lot_sq_footage,trav$household_policy_counts,trav$newest_veh_age,trav$tenure_at_snapshot,trav$trm_len_mo)


call_counts <- trav$call_counts

# Define the permutation test function
permutation_test <- function(response, predictor, n_permutations = 1000) {
  # Observed correlation
  observed_cor <- cor(response, predictor, method = "pearson")

  # Generate permutation distribution
  permuted_cor <- replicate(n_permutations, {
    permuted_response <- sample(response)  # Shuffle response
    cor(permuted_response, predictor, method = "pearson")
  })

  # Calculate p-value
  p_value <- mean(abs(permuted_cor) >= abs(observed_cor))

  return(list(observed_cor = observed_cor, p_value = p_value))
}

# Apply permutation test to each numeric predictor
results <- lapply(trav_num, function(predictor) {
  permutation_test(response = call_counts, predictor = predictor)
})

# Name results by the variable names in trav_num
names(results) <- names(trav_num)

# View the results
results







