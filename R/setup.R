rm(list = ls())

if(Sys.info()['user']=='dag') {
  pathScript <- '~/gitreps/mleMidterm/R'
}

library('MASS')
set.seed(6886)

## --- setup random data ----------------------------------------
N <- 100
mu <- c(0, 0, 0)
sigma <- matrix(
  c(
     1, .6, .4,
    .6,  1,  0,
    .4,  0,  1
  ),
  byrow = TRUE, ncol = 3
)
dta <- data.frame(mvrnorm(N, mu, sigma))
colnames(dta) <- c('x', 'y', 'z')
#summary(dta)
reference <- lm(y ~ x + z, data = dta)
## --- load ols function ----------------------------------------
source(file.path(pathScript, 'olsFunc.R'))
## --- test run -------------------------------------------------
form <- formula(y ~ x + z)
model <- ols(formula = form, data = dta)
model$coefficients                                 ## output okay
model$varcov                                       ## output okay
model$Rsq                                          ## output okay
model$Fstat                                        ## output okay
summary(reference); confint(reference)         ## comparison okay
## END