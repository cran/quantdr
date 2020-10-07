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
true_dir <- x %*% beta_true
plot(true_dir, y, xlab = "sufficient direction", ylab = "y", pch = 16)
points(true_dir, qhat1$ll_est, pch = 16, col = 'red')

## -----------------------------------------------------------------------------
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

out3 <- matrix(0, p, length(taus))
for (i in 1:length(taus)) {
  out3[, i] <- cqs(x, y, tau = taus[i], dtau = 1)$qvectors
}
out3

## ----fig2, fig.height = 5.5, fig.width = 7, fig.align = "center"--------------
newx <- x %*% out3
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,3))
for (i in 1:length(taus)) {
  plot(true_dir, y, xlab = "sufficient direction", ylab = "y", main = taus[i], pch = 16)
  qhat_tau <- llqr(newx[, i], y, tau = taus[i])$ll_est
  points(true_dir, qhat_tau, pch = 16, col = "red")
}
par(oldpar)

