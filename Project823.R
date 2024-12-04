



#Models to consider - Random Forest - Poisson and glm (regularization), gbm, PCA and clustering?
library(corrplot)
library(caret)
library(randomForest)
library(dplyr)


trav <- read.csv("train_data.csv")
summary(trav)
#Exclude first column (ID column)
trav <- trav[,-1]

trav <- trav %>%
  mutate(across(where(is.character), as.factor))

View(trav)


#Missing Values Option 1 - Removing all - issue: channel other will have no values
trav[trav == -2 |trav == -20 | trav == "missing"] <- NA
trav1 <- na.omit(trav)

1-nrow(trav1)/nrow(trav) #78% of the data is missing!

summary(trav1$prdct_sbtyp_grp)

trav1$acq_method <- factor(trav1$acq_method, levels = setdiff(levels(trav1$acq_method), "missing"))
trav1$bi_limit_group <- factor(trav1$bi_limit_group, levels = setdiff(levels(trav1$bi_limit_group), "NonAuto"))




#Multicollinearity between these three categories
table(trav1$household_group,trav1$prdct_sbtyp_grp)
table(trav1$prdct_sbtyp_grp,trav1$product_sbtyp)
table(trav1$household_group, trav1$product_sbtyp)


crossproduct <- factor(paste(trav$household_group, trav$p, trav$prdct_sbtyp_grp, sep = "_"))

summary(crossproduct)




trav1 <- trav
#creating Train and Test Sets

trainIndex <- createDataPartition(trav1$call_counts, p = 0.8, list = FALSE)
train_data <- trav1[trainIndex, ]
test_data <- trav1[-trainIndex, ]


#Checking whether stratification was successful
mean(train_data$call_counts)
mean(test_data$call_counts)


#Method 1-glm
glm_poisson <- train(call_counts ~.-channel-trm_len_mo-household_group-product_sbtyp, data = train_data,
                     method = "glm",
                     family = "poisson",
                     trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                     metric = "RMSE")

summary(glm_poisson)

print(glm_poisson)

prds1 <- predict(glm_poisson, newdata = test_data)

# Evaluate model performance using RMSE
rmse <- sqrt(mean((prds1 - test_data$call_counts)^2))
rmse
pred1 <- data.frame(test_data$call_counts, data.frame(prds1))
print(pred1)



#Method 2 - ZIP (Need to return to this)
install.packages("pscl")
library(pscl)

# Zero-Inflated Poisson model
zip_model <- zeroinfl(call_counts~X12m_call_history+acq_method+bi_limit_group+digital_contact_ind+geo_group+has_prior_carrier,
                      data = train_data, dist = "poisson")

summary(zip_model)


train_data$home_lot_sq_footage

summary(zip_model)


# For comparison, you could try Negative Binomial
negbin_model <- glm.nb(call_counts ~ ., data = train_data)

# Predict using the Zero-Inflated Poisson model
zip_predictions <- predict(zip_model, newdata = test_data, type = "response")

# Predict using the Negative Binomial model
nb_predictions <- predict(negbin_model, newdata = test_data, type = "response")





#Method 3 - Regularized GLM
install.packages("glmnet")
library(glmnet)

# Regularized Poisson model using caret with glmnet
set.seed(123)  # For reproducibility
glmnet_poisson <- train(
  call_counts ~ . - channel - trm_len_mo - household_group - product_sbtyp,
  data = train_data,
  method = "glmnet",
  family = "poisson",
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
  tuneGrid = expand.grid(
    alpha = c(0, 0.5, 1),  # 0 = Ridge, 1 = Lasso, and values between 0 and 1 for Elastic Net
    lambda = seq(0.01, 0.1, length = 10)  # Grid of regularization parameters
  ),
  metric = "RMSE"
)

coef(glmnet_poisson$finalModel, s = glmnet_poisson$bestTune$lambda)


prds2 <- predict(glmnet_poisson, newdata = test_data)

# Evaluate model performance using RMSE
rmse2 <- sqrt(mean((prds2 - test_data$call_counts)^2))
rmse2
pred2 <- data.frame(test_data$call_counts, data.frame(prds2))
print(pred2)




#Method4 - Random forest
library(randomForest)

rf.poisson <- randomForest(call_counts ~ ., data = train_data, importance = T, ntree = 50, mtry = 4, nodesize=20, maxnodes=NULL)

prds4 <- predict(rf.poisson, newdata = test_data)

# Evaluate model performance using RMSE
rmse4 <- sqrt(mean((prds4 - test_data$call_counts)^2))
rmse4
pred4 <- data.frame(test_data$call_counts, data.frame(prds4))
print(pred4)







#Predicting on the Test Set

travtst <- read.csv("test_data.csv")

summary(travtst)

#Exclude first column (ID column)
travtst2 <- travtst[,-1]

travtst2 <- travtst2 %>%
  mutate(across(where(is.character), as.factor))

View(travtst2)


#Missing Values Option 1 - Removing all - issue: channel other will have no values
travtst[travtst == -2 |travtst == -20 | travtst == "missing"] <- NA
travtst <- na.omit(travtst)

trav

1-nrow(travtst)/nrow(travtst) #0% of the data is missing!

summary(trav1$prdct_sbtyp_grp)

travtst$acq_method <- factor(travtst$acq_method, levels = setdiff(levels(travtst$acq_method), "missing"))
travtst$bi_limit_group <- factor(travtst$bi_limit_group, levels = setdiff(levels(travtst$bi_limit_group), "NonAuto"))



prds5 <- predict(rf.poisson, newdata = travtst2)

prds5 <- data.frame(prds5)




subfile <- data.frame(travtst$id,prds5)





write.csv(subfile, file = "C:\\Users\\aarif\\OneDrive\\Desktop\\MSc. Act Sci\\Spring 2024\\STAT 885\\Assignments\\Final Project\\DataTest.csv", row.names=TRUE)














# Evaluate model performance using RMSE
rmse3 <- sqrt(mean((prds3 - test_data$call_counts)^2))
rmse3
pred3 <- data.frame(test_data$call_counts, data.frame(prds3))
print(pred3)








































trav_num <- data.frame(trav$X12m_call_history,trav$ann_prm_amt,trav$digital_contact_ind,trav$home_lot_sq_footage,trav$household_policy_counts,trav$newest_veh_age,trav$tenure_at_snapshot,trav$trm_len_mo)

trav_num




cor_matrix <- cor(trav_num, use = "complete.obs")
cor_matrix

corrplot(cor_matrix, method = "circle")
#high correlation between trav.trm_len_mo and trav.newest_veh_age


hist(trav$call_counts)



#Imputing missing values
preProc <- preProcess(trav, method = "medianImpute")
trav_imputed <- predict(preProc, trav)

summary(trav_imputed)




trainIndex <- createDataPartition(trav_imputed$call_counts, p = 0.8, list = FALSE)
train_data <- trav_imputed[trainIndex, ]
test_data <- trav_imputed[-trainIndex, ]












cat("RMSE on the test set:", rmse, "\n")






#Method 3 - Random Forest using Train Function


set.seed(161)

rf.cv1 <-train(call_counts~., data = trav_imputed, method="rf",
               trControl=trainControl(method = "repeatedcv", number = 5, repeats = 3),
               metric="RMSE",tuneGrid=expand.grid(mtry=c(1:4)), ntree=101, importance=T)



summary(trav$call_counts)

summary(pred$predictions)



#Issues - a lot of missing values - remove? impute?
#The response (call count) has a lot of zeroes - try a zero-inflated poisson, or a hurdle model
#Multicollinearity issues






