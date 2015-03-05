## --- setup workspace ------------------------------------------
rm(list = ls())
if(Sys.info()['user']=='dag') {
  pathScript <- '~/gitreps/mleMidterm/R';
  pathData <- '~/Dropbox/Buero/Dissertation/2015/duke/classes/mle/midterm/'
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
colnames(dta) <- c('y', 'x', 'z')

missMatrix <- cbind(
  rbinom(N, 1, .9), rbinom(N, 1, .85), rbinom(N, 1, .83)
)
missMatrix[missMatrix==0] <- NA
dta <- dta * missMatrix
summary(dta)                  ## 40 Missing data points generated

## --- load ols function ----------------------------------------
source(file.path(pathScript, 'olsFunc.R'))

## --- test run -------------------------------------------------
form <- formula(y ~ x + z)

modelListDel <- ols(form, data = dta)
modelImpDta <- ols(form, data = dta, impute = TRUE)

referenceListDel <- lm(form, data = dta)
referenceImpData <- lm(
  form, data = Amelia::amelia(dta, m = 1)[['imputations']][[1]]
)
modelListDel; referenceListDel               ## results identical
modelImpDta; referenceImpData    ## results substantively similar

## Do components output correctly?
modelListDel$coefficients                          ## output okay
modelListDel$varcov                                ## output okay
modelListDel$Rsq                                   ## output okay
modelListDel$Fstat                                 ## output okay
rm(mu, sigma, dta, form, missMatrix, modelListDel, modelImpDta,
  referenceListDel, referenceImpData, N
)

## --- Load midterm data ----------------------------------------
load(file.path(pathData, 'data', 'midTermData.rda'))
summary(data); summary(dataMiss)

form <- formula(gini_net_std ~ ELF_ethnic + polity2)
model <- ols(formula=form, data=data)
modelListDel <- ols(formula = form, data=dataMiss)
modelAmelia <- ols(formula = form, data=dataMiss, impute=TRUE)

summary(lm(form, data = data))
summary(lm(form, data = dataMiss)); modelListDel
## END