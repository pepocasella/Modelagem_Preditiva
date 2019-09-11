rm(list = ls())

spam <- read.csv("spam.csv")

str(spam)

table(spam$Class)

library(tree)

tree <- tree(Class ~ ., data = spam) # utilizando todos os dados

summary(tree)

plot(tree, type = "uniform")
text(tree, cex = 0.95)

set.seed(1234)

idx <- sample(1:nrow(spam), size = round(0.7 * nrow(spam)), replace = FALSE)

training <- spam[idx,]
test <- spam[-idx,]

tree <- tree(Class ~ . , data = training)

y_hat <- predict(tree, test, type = "class")

(conf_table <- table(y_hat, test$Class, dnn = c("Predicted", "Actual")))

sum(diag(conf_table)) / sum(conf_table)

(cv <- cv.tree(tree, FUN = prune.misclass))

plot(cv$size, cv$dev, type = "b", lwd = 2, col = "dark green",
	 xlab = "Número de folhas",
	 ylab = "Número de erros de classificação",
	 main = "Validação cruzada em 10 lotes")

pruned_tree <- prune.misclass(tree, best = 7)

plot(pruned_tree, type = "uniform")
text(pruned_tree, cex = 0.95)

y_hat_pruned <- predict(pruned_tree, test, type = "class")

(conf_table <- table(y_hat_pruned, test$Class, dnn = c("Predicted", "Actual")))

sum(diag(conf_table)) / sum(conf_table)
