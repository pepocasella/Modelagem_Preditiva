# Aula 05 (bootstrap)

# Bootstrap

rm(list = ls())

set.seed(1234)

alpha <- 2
beta <- 0.5

true_median <- qgamma(0.5, shape = alpha, rate = beta)

n <- 75

# Monte Carlo sampling from the true distribution

N <- 10^4
x_mc <- matrix(rgamma(N*n, shape = alpha, rate = beta), nrow = N, ncol = n)
eqm <- apply(x_mc, 1, function(row) (median(row) - true_median)^2)
approx_true_eqm <- mean(eqm)
th_n = apply(x_mc, 1, median)
mean_th_n <- mean(th_n)
approx_true_se <- sqrt(mean(th_n^2) - mean_th_n^2)

# A "real world" sample of size n

(x <- rgamma(n, shape = alpha, rate = beta))

# But in the "real world", we don't know f (and the true median);
# all we know is x = (x_1, ... , x_n)

# Empirical distribution function

plot(ecdf(x), main = "Função de distribuição empírica", 
     xlab = "", ylab = "", cex = 0.5, xlim = c(0, max(x)))

# The ecdf converges to the true distribution function F
# when n grows; it's our proxy to learn about the true F

Fd <- function(t) pgamma(t, shape = alpha, rate = beta)
curve(Fd, col = "red", lwd = 2, add = TRUE)

# bootstrap sampling from our observed sample x = (x_1, ... , x_n)

theta_hat_n <- median(x)
B <- 10^4
x_bs <- matrix(sample(x, size = B*n, replace = TRUE), ncol = n)
U <- apply(x_bs, 1, function(row) (median(row) - theta_hat_n)^2)
bootstrap_estimate_eqm <- mean(U)
th_n <- apply(x_bs, 1, median)
mean_th_n <- mean(th_n)
bootstrap_estimate_se <- sqrt(mean(th_n^2) - mean_th_n^2)

cat(sprintf("True EQM     = %.4f (approximately)", approx_true_eqm))
cat(sprintf("Boostrap EQM = %.4f", bootstrap_estimate_eqm))

cat(sprintf("True SE     = %.4f (approximately)", approx_true_se))
cat(sprintf("Boostrap SE = %.4f", bootstrap_estimate_se))
