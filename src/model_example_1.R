# Split data into train and test
index <- createDataPartition(db_lite_no_NA$Y, p = .70, list = FALSE)
train <- db_lite_no_NA[index, ]
test <- db_lite_no_NA[-index, ]

# Check there are no NAs: 
#dim(train) # do the same for test
#colSums(is.na(train)) 
sum(is.na(train))
sum(is.na(test ))

str(train) # check all vars have been transformed to factors
str(test )

# LASSO MODEL ----
library(glmnet) # loading the library

x_vars  <- model.matrix( Y~ . , data = train)[,-1]
y_var   <- train$Y

x_test  <- model.matrix( Y~ . , data = test )[,-1]
y_test  <- test$Y

lambda_seq <- 10^seq(2, -2, by = -.1)
mod.lasso  <-  cv.glmnet(x_vars, y_var, alpha = 1, lambda = lambda_seq)

# Identifying best lambda:
best_lam <- mod.lasso$lambda.min

# Rebuilding the model with best lambda value identified:
lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_test)

# Combine predicted and actual values and use R-Squared to check model performance:
final <- cbind(y_test, pred)
head(final)

# R squared
rss <- sum((y_test - pred) ^ 2)
tss <- sum((y_test - mean(y_test)) ^ 2)
rsq <- 1 - rss/tss
rsq

# Adjusted R squared
rdf  <- nrow(x_vars) - ncol(x_vars) - 1
rsq_adj <- 1 - (1 - rsq) * ((nrow(x_vars) - 1)/rdf)
rsq_adj

# Getting the list of important variables:
tmp_coeffs <- coef(lasso_best)
coef_lasso <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x) # store coefficients in dataframe

# Elastic net
mod.elastic  <-  cv.glmnet(x_vars, y_var, alpha = 0.5, lambda = lambda_seq)

# Identifying best lambda:
best_lam_el <- mod.elastic$lambda.min

# Rebuilding the model with best lambda value identified:
elastic_best <- glmnet(x_vars, y_var, alpha = 0.5, lambda = best_lam_el)
pred_elastic <- predict(elastic_best, s = best_lam_el, newx = x_test)

# Combine predicted and actual values and use R-Squared to check model performance:
elastic_final <- cbind(y_test, pred_elastic)
head(elastic_final)

# R squared
e_rss <- sum((y_test - pred_elastic) ^ 2)
e_tss <- sum((y_test - mean(y_test)) ^ 2)
e_rsq <- 1 - e_rss/e_tss
e_rsq

# Adjusted R squared
e_rdf  <- nrow(x_vars) - ncol(x_vars) - 1
e_rsq_adj <- 1 - (1 - e_rsq) * ((nrow(x_vars) - 1)/e_rdf)
e_rsq_adj

# Getting the list of important variables:
el_tmp_coeffs <- coef(elastic_best)
coef_elastic <- data.frame(name = el_tmp_coeffs@Dimnames[[1]][el_tmp_coeffs@i + 1], coefficient = el_tmp_coeffs@x) # store coefficients in dataframe
coef_elastic

