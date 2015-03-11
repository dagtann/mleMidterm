## --- setup workspace ------------------------------------------
rm(list = ls())
if(Sys.info()['user']=='dag') {
  pathScript <- '~/gitreps/mleMidterm/R';
  pathData <- '~/Dropbox/Buero/Dissertation/2015/duke/classes/mle/midterm/data';
  pathOut <- '~/Dropbox/Buero/Dissertation/2015/duke/classes/mle/midterm/out'
}
library('MASS'); library('ggplot2'); library('ggthemes')
set.seed(6886)

## --- load ols function ----------------------------------------
source(file.path(pathScript, 'ols.R'))

## --- Load midterm data ----------------------------------------
load(file.path(pathData, 'midTermData.rda'))
summary(data); summary(dataMiss)

form <- formula(gini_net_std ~ ELF_ethnic + polity2)
model <- ols(formula=form, data=data)
modelReference <- lm(formula = form, data = data)
modelListDel <- ols(formula = form, data=dataMiss)
set.seed(6886); 
modelAmelia <- ols(formula = form, data=dataMiss, impute=TRUE)
set.seed(6886); modelAmeliaReference <- lm(
  form, 
  data = Amelia::amelia(dataMiss, m = 1)[['imputations']][[1]]
)
set.seed(6886)
coef(lm(
  form, 
  data = Amelia::amelia(dataMiss, m = 1)[['imputations']][[1]]
))
coef(lm(
  form, 
  data = Amelia::amelia(dataMiss, m = 1)[['imputations']][[1]]
))

## --- Close up coefficients ------------------------------------
modelAmelia[['coefficients']][, 1]; coef(modelAmeliaReference)
modelListDel[['coefficients']][, 1]; coef(lm(form, data = dataMiss))
model[['coefficients']][, 1]; coef(lm(form, data = data))

## --- produce plot output --------------------------------------
source(file.path(pathScript, 'slopePlotELF.R'))
source(file.path(pathScript, 'slopePlotPolity2.R'))
source(file.path(pathScript, 'spaghettiPolity2.R'))
source(file.path(pathScript, 'spaghettiELF.R'))
## END