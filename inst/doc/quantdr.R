## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("quantdr")

## ----setup--------------------------------------------------------------------
library(quantdr)

## ---- eval=FALSE--------------------------------------------------------------
#  help(package = "quantdr")

## ---- eval=FALSE--------------------------------------------------------------
#  help(cqs)
#  ?cqs

## -----------------------------------------------------------------------------
set.seed(1234)
n <- 100
p <- 10
tau <- 0.5
x <- matrix(rnorm(n * p), n, p)
error <- rnorm(n)
y <- 3 * x[, 1] + x[, 2] + error

## -----------------------------------------------------------------------------
out1 <- cqs(x, y, tau = tau, dtau = 1)
out1

## -----------------------------------------------------------------------------
out2 <- cqs(x, y, tau = tau)
out2

## -----------------------------------------------------------------------------
out2$qvectors[, 1:out2$dtau]

## -----------------------------------------------------------------------------
library(pracma)
beta_true <- c(3, 1, rep(0, p - 2))
beta_hat1 <- out1$qvectors
beta_hat2 <- out2$qvectors[, 1:out2$dtau]
subspace(beta_true, beta_hat1) / (pi / 2)
subspace(beta_true, beta_hat2) / (pi / 2)

## -----------------------------------------------------------------------------
newx <- x %*% beta_hat1

## -----------------------------------------------------------------------------
qhat1 <- llqr(newx, y, tau)
qhat1

## -----------------------------------------------------------------------------
qhat2 <- llqr(newx, y, tau, method = "CV")
qhat2

## -----------------------------------------------------------------------------
qhat3 <- llqr(newx, y, tau, h = 1)
qhat3

## ----fig1, fig.height = 4.5, fig.width = 4.5, fig.align = "center"------------
library(ggplot2)
true_dir <- x %*% beta_true
data1 <- data.frame(true_dir, y, qhat1$ll_est)

ggplot(data1, aes(x = true_dir, y=y)) + geom_point(size = 1) + 
  geom_point(aes(x = true_dir, qhat1$ll_est), colour = 'red', size = 1) +
  xlab('sufficient direction')

## -----------------------------------------------------------------------------
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

out3 <- matrix(0, p, length(taus))
for (i in 1:length(taus)) {
  out3[, i] <- cqs(x, y, tau = taus[i], dtau = 1)$qvectors
}
out3

## ----fig2, fig.height = 5.5, fig.width = 7, fig.align = "center"--------------
newx <- x %*% out3
qhat_tau <- as.null()
for (i in 1:length(taus)) {
  qhat_tau <- c(qhat_tau, llqr(newx[, i], y, tau = taus[i])$ll_est)
}

data2 <- data.frame(rep(true_dir, n), rep(y, n), qhat_tau, rep(taus, each = n))
names(data2) <- c("true_dir", "y", "qhat_tau", "quantiles")
ggplot(data2, aes(x = true_dir, y = y)) + geom_point(size = 1) + 
  geom_point(aes(x = true_dir, qhat_tau), colour = 'red', size = 1) +
  facet_wrap(~quantiles, ncol = 3) + xlab('sufficient direction')

