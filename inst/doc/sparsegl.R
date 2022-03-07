## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("dajmcdon/sparsegl", ref = "main")

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("dajmcdon/sparsegl", ref = "main",
#                            build_vignettes = TRUE, dependencies = TRUE)

## -----------------------------------------------------------------------------
library(sparsegl)
set.seed(1010)
n <- 100
p <- 200
X <- matrix(data = rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p)
beta_star <- c(rep(5, 5), c(5, -5, 2, 0, 0), rep(-5, 5), c(2, -3, 8, 0, 0), rep(0, (p - 20)))
groups <- rep(1:(p / 5), each = 5)

# Linear regression model
eps <- rnorm(n, mean = 0, sd = 1)
y <- X %*% beta_star + eps

# Logistic regression model
pr <- 1 / (1 + exp(-X %*% beta_star))
y_binary <- rbinom(n, 1, pr)

## -----------------------------------------------------------------------------
fit1 <- sparsegl(X, y, group = groups)

## ---- message = FALSE, warning = FALSE, fig.width = 8, fig.height = 4---------
plot(fit1, y_axis = "group", x_axis = "lambda")
plot(fit1, y_axis = "coef", x_axis = "penalty", add_legend = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  coef <- coef(fit1, s = c(0.02, 0.03))
#  pred <- predict(fit1, newx = X[100,], s = fit1$lambda[2:3])
#  print(fit1)

## -----------------------------------------------------------------------------
fit_l1 <- cv.sparsegl(X, y, group = groups, pred.loss = "L1")

## ---- message = FALSE, warning = FALSE, fig.width = 8, fig.height = 4---------
plot(fit_l1)

## -----------------------------------------------------------------------------
coef <- coef(fit_l1, s = "lambda.1se")
pred <- predict(fit_l1, newx = X[50:80, ], s = "lambda.min")

## -----------------------------------------------------------------------------
risk <- estimate_risk(fit1, X, y, type = "AIC", approx_df = FALSE)

