## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("sparsegl")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("dajmcdon/sparsegl")

## ---- eval = FALSE------------------------------------------------------------
#  remotes::install_github(
#    "dajmcdon/sparsegl",
#    build_vignettes = TRUE, dependencies = TRUE
#  )

## -----------------------------------------------------------------------------
library(sparsegl)
set.seed(1010)
n <- 100
p <- 200
X <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
beta_star <- c(rep(5, 5), c(5, -5, 2, 0, 0), rep(-5, 5), 
               c(2, -3, 8, 0, 0), rep(0, (p - 20)))
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

## ---- fig.width = 8, fig.height = 4-------------------------------------------
fit_l1 <- cv.sparsegl(X, y, group = groups, pred.loss = "mae")
plot(fit_l1)

## -----------------------------------------------------------------------------
coef <- coef(fit1, s = c(0.02, 0.03))
predict(fit1, newx = X[100,], s = fit1$lambda[2:3])
predict(fit_l1, newx = X[100,], s = "lambda.1se")
print(fit1)

## -----------------------------------------------------------------------------
risk <- estimate_risk(fit1, X, approx_df = FALSE)

## ----echo = FALSE, message=FALSE, warning=FALSE, fig.align='center', fig.width = 8, fig.height = 4----
library(dplyr)
library(tidyr)
library(ggplot2)
er <- risk %>% dplyr::select(-df) %>% pivot_longer(-lambda, values_to = "risk")
err <- er %>% group_by(name) %>% summarise(lambda = lambda[which.min(risk)])
ggplot(er, aes(lambda, risk, color = name)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(data = err, aes(xintercept = lambda, color = name),
             linetype = "dashed", show.legend = FALSE) +
  theme_bw() +
  ylab("Estimated risk") +
  xlab("Lambda") +
  scale_x_log10() + scale_y_log10() +
  theme(legend.title = element_blank())  

