## --- setup random data ----------------------------------------
N <- 1000
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
colnames(dta) <- c('y', 'x', 'z')

missMatrix <- cbind(
  rbinom(N, 1, .9), rbinom(N, 1, .85), rbinom(N, 1, .83)
)
missMatrix[missMatrix==0] <- NA
dta <- dta * missMatrix
summary(dta)                  ## 40 Missing data points generated
form <- formula(y ~ x + z)

ols(form, data = dta)
modelListDel <- ols(form, data = dta)
set.seed(6886); modelImpDta <- ols(form, data = dta, impute = TRUE)

referenceListDel <- lm(form, data = dta)
set.seed(6886); referenceImpData <- lm(
  form, data = Amelia::amelia(dta, m = 1)[['imputations']][[1]]
)
modelListDel; summary(referenceListDel)      ## results identical
modelImpDta; summary(referenceImpData)  ## results subst. similar

## Do components output correctly?
modelListDel$coefficients                          ## output okay
modelListDel$varcov                                ## output okay
modelListDel$Rsq                                   ## output okay
modelListDel$Fstat                                 ## output okay
rm(mu, sigma, dta, form, missMatrix, modelListDel, modelImpDta,
  referenceListDel, referenceImpData, N
)