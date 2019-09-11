# Aula 1

rm(list = ls())

df <- read.csv("clientes.csv")

str(df)

X <- df[, 1:2]
y <- df[, 3]

plot(X, col = ifelse(y == "S", "red4", "blue4"), pch = 19, cex = 0.9,
	 xlim = c(18, 75), ylim = c(2, 12),
     xlab = "Idade", ylab = "Renda", main = "Utilizou o cheque especial por mais de 21 dias no último ano?")

legend(20, 12, c("Sim", "Não"), col = c("red4", "blue4"), pch = 19, cex = 0.9)

###

library(class)

plot_classifier <- function(X, y, k) {
    grid_X <- expand.grid(seq(18, 75, length = 100), seq(2, 12, length = 100))
    grid_y <- knn(X, grid_X, y, k = k)
    plot(grid_X, col = ifelse(grid_y == "S", "coral", "cornflowerblue"), pch = 19, cex = 0.5,
    	 xlim = c(18, 75), ylim = c(2, 12),
         xlab = "Idade", ylab = "Renda", main = sprintf("k = %d", k))
    points(X, col = ifelse(y == "S", "red4", "blue4"), pch = 19, cex = 0.9)
}

par(mfrow = c(1, 2))
plot_classifier(X, y, k = 1)
plot_classifier(X, y, k = 15)

###

set.seed(1234)

idx <- sample(1:nrow(X), size = round(0.5 * nrow(X)), replace = FALSE)

X_trn <- X[idx,]
y_trn <- y[idx]

X_tst <- X[-idx,]
y_tst <- y[-idx]

k_max <- 100
test_error <- numeric(k_max)
for (k in 1:k_max) {
	y_hat <- knn(X_trn, X_tst, y_trn, k = k)
	test_error[k] <- mean(y_hat != y_tst)
	cat(sprintf("k = %i: test error = %.5f\n", k, test_error[k]))
}

plot(1:k_max, test_error, type = "b", lwd = 2, col = "dark green", las = 1,
     xlab = "k", ylab = "", main = "Test error")

(best_k <- which.min(test_error))
min(test_error)

par(mfrow = c(1, 1))
plot_classifier(X_trn, y_trn, k = best_k)
