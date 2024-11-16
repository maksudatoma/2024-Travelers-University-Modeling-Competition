####################################Assignment Week 11/11 - 11/15#####################################
install.packages("caret")
library(caret)





trav <- read.csv("train_data.csv")
summary(trav)
#Exclude first column (ID column)
trav <- trav[,-1]

trav <- trav %>%
  mutate(across(where(is.character), as.factor))

View(trav)


trav[trav == -2 |trav == -20 | trav == "missing"] <- NA

missing_counts <- colSums(is.na(trav))

# Display variables with missing values and their counts
missing_counts[missing_counts > 0]

#Visualising pattern of missingness
md.pattern(trav)


#Zero values for the response
per0resp <- sum(trav$call_counts == 0) / nrow(trav) * 100
per0resp



#Correlation for numeric predictors
num_vars <- sapply(trav, is.numeric)
correls <- sapply(trav[, num_vars], function(x) cor(x, trav$call_counts, use = "complete.obs"))

# Print correlations
print(correls)


#ANOVA for categorical predictors
cat_vars <- sapply(trav, is.factor)

anova_results <- lapply(names(trav)[cat_vars], function(var) {
  anova_model <- aov(trav$call_counts ~ trav[[var]])
  summary(anova_model)
})

# Print ANOVA summaries
names(anova_results) <- names(trav)[cat_vars]
anova_results


# Split data into training set (60%)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(trav$call_counts, p = 0.6, list = FALSE)
train_data <- trav[trainIndex, ]

# Remaining data (40%) for validation and test sets
rem_data <- trav[-trainIndex, ]

# Split remaining data into validation (20%) and test (20%)
validIndex <- createDataPartition(rem_data$call_counts, p = 0.5, list = FALSE)
valid_data <- rem_data[validIndex, ]
test_data <- rem_data[-validIndex, ]

# Checking whether stratification was successful
mean(train_data$call_counts)
mean(valid_data$call_counts)
mean(test_data$call_counts)



gbm.poisson <- gbm(call_counts ~ ., data = train_data, distribution="poisson", n.tree = 500, interaction.depth=7, shrinkage=0.01,n.minobsinnode=20,bag.fraction=1)

gbm.poissonpred <- predict(gbm.poisson, newdata = test_data, type = "response")
# Evaluate model performance using RMSE
rmse8 <- sqrt(mean((gbm.poissonpred - test_data$call_counts)^2))
rmse8






######################################################################################################
