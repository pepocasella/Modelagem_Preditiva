# Aula 04 (árvores de regressão)

rm(list = ls())

# Exemplo feito na lousa

# EQM de Treinamento

y     <- c(8.1, 5.2, 10.9, 3.5, 2.5, 6.3, 4.2, 4.5, 2.5, 3.1)
y_hat <- c(8.1, 8.1,  8.1, 3.0, 3.0, 5.3, 5.3, 3.4, 3.4, 3.4)

mean((y - y_hat)^2)

# EQM de Teste

y     <- c(6.1, 7.9, 4.0)
y_hat <- c(5.3, 8.1, 3.4)

mean((y - y_hat)^2)

# Boston housing

library(MASS)

# ?Boston

# str(Boston)

# View(Boston)

set.seed(1234)

idx <- sample(1:nrow(Boston), round(0.7 * nrow(Boston)))

training <- Boston[idx,]
test <- Boston[-idx,]

# OLS

ols <- lm(medv ~ ., data = training)

summary(ols)

y_hat_ols <- predict(ols, newdata = test)

(RMSE_ols <- sqrt(mean((y_hat_ols - test$medv)^2)) * 1000) # EQM de teste

# Árvore de regressão

library(tree)

tree <- tree(medv ~ ., data = training)

summary(tree)

plot(tree, type = "uniform") # interpretar
text(tree, cex = 0.75)

y_hat_tree <- predict(tree, newdata = test)

(RMSE_tree <- sqrt(mean((y_hat_tree - test$medv)^2)) * 1000)

# Random Forest

library(randomForest)

rf <- randomForest(medv ~ ., data = training)

y_hat_rf <- predict(rf, newdata = test)

(RMSE_rf <- sqrt(mean((y_hat_rf - test$medv)^2)) * 1000)

# Carseats dataset

rm(list = ls())

library(ISLR)

# ?Carseats

# str(Carseats)

# View(Carseats)

set.seed(1234)

idx <- sample(1:nrow(Carseats), round(0.7 * nrow(Carseats)))

training <- Carseats[idx,]
test <- Carseats[-idx,]

# OLS

ols <- lm(Sales ~ ., data = training)

summary(ols)

y_hat_ols <- predict(ols, newdata = test)

(RMSE_ols <- sqrt(mean((y_hat_ols - test$Sales)^2)))

# Árvore de regressão

tree <- tree(Sales ~ ., data = training)

summary(tree)

plot(tree, type = "uniform")
text(tree, cex = 0.75)

y_hat_tree <- predict(tree, newdata = test)

(RMSE_tree <- sqrt(mean((y_hat_tree - test$Sales)^2)))

# podando a árvore

pruned <- prune.tree(tree, best = 6)

summary(pruned)

plot(pruned, type = "uniform")
text(pruned, cex = 0.75)

y_hat_pruned <- predict(pruned, newdata = test)

(RMSE_pruned <- sqrt(mean((y_hat_pruned - test$Sales)^2)))
