setwd("C:\\Users\\aarif\\OneDrive\\Desktop\\PhD Statistics\\STAT 823\\Project")



#Models to consider - Random Forest - Poisson and glm (regularization), gbm, PCA and clustering?
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
library(dplyr)


trav <- read.csv("train_data.csv")
summary(trav)


trav <- trav %>%
  mutate(across(where(is.character), as.factor))

View(trav)
#Exclude first column (ID column)
trav <- trav[,-1]

trav[trav == -2] <- NA




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


#Checking whether stratification was successful
mean(train_data$call_counts)
mean(test_data$call_counts)




#Method 1-glm

glm_poisson <- train(call_counts ~ household_group+prdct_sbtyp_grp+product_sbtyp, data = train_data,
                     method = "glm",
                     family = "poisson",
                     trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                     metric = "RMSE")


summary(glm_poisson)


print(glm_poisson)


predictions <- predict(glm_poisson, newdata = test_data)

# Evaluate model performance using RMSE
rmse <- sqrt(mean((predictions - test_data$call_counts)^2))

pred <- data_frame(test_data$call_counts, data.frame(predictions))

print(pred)


#Multicollinearity between these three categories
table(trav$household_group,trav$prdct_sbtyp_grp)
table(trav$prdct_sbtyp_grp,trav$product_sbtyp)
table(trav$household_group, trav$prdct_sbtyp_grp)


table(trav$household_group,trav$prdct_sbtyp, trav$prdct_sbtyp_grp)

crossproduct <- factor(paste(trav$household_group, trav$prdct_sbtyp, trav$prdct_sbtyp_grp, sep = "_"))

summary(crossproduct)





cat("RMSE on the test set:", rmse, "\n")




#Method 2 - ZIP

# Install and load the pscl package if you haven't already
install.packages("pscl")
library(pscl)

# Zero-Inflated Poisson model
zip_model <- zeroinfl(call_counts ~ X12m_call_history+prdct_sbtyp_grp, data = train_data, dist = "poisson")

summary(zip_model)


# For comparison, you could try Negative Binomial
negbin_model <- glm.nb(call_counts ~ ., data = train_data)

# Predict using the Zero-Inflated Poisson model
zip_predictions <- predict(zip_model, newdata = test_data, type = "response")

# Predict using the Negative Binomial model
nb_predictions <- predict(negbin_model, newdata = test_data, type = "response")




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






