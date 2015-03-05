if(Sys.info()['user']=='dag') {
  pathScript <- '~/gitreps/mleMidterm/R'
}

library('MASS')
set.seed(6886)

## --- setup random data ----------------------------------------
N <- 10
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
summary(dta)
reference <- lm(dta[, 1] ~ dta[, 2] + dta[, 3], data = dta)
## --------------------------------------------------------------
## END