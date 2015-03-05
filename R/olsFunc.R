#ols <- function(depVar, predVar){
  output <- list(
    coefficients = matrix(NA, nrow = ncol(X), ncol = 6),
    varCov = matrix(NA, nrow = ncol(X), ncol = ncol(X)),
    rSquared = vector(length = 1)
  )
  
  X <- as.matrix(cbind(1, dta[, 2:3]))
  Y <- as.vector(dta[, 1])
  n <- length( apply(X, 1, function(x){ all(!is.na(x)) }) )
  degFree <- n-ncol(X)
  
  colnames(output[['coefficients']]) <- c(
    'Estimate', 'Std. Error', 't Value', 'Pr(>|t|)', 'Lower 95% CI', 
    'Upper 95% CI'
  )
  output[['coefficients']][, 1] <- solve(t(X)%*%X)%*%t(X)%*%Y
  fitted <- X%*%output[['coefficients']][, 1]
  residuals <- Y-fitted
  sigma2 <- sum(residuals^2)/degFree
  
  output[['varCov']] <- sigma2*solve(t(X)%*%X)
  output[['coefficients']][, 2] <- sqrt(diag(output[['varCov']]))
  output[['coefficients']][, 3] <-  output[['coefficients']][, 1]/output[['coefficients']][, 2]
  output[['coefficients']][, 4] <- sapply(
    output[['coefficients']][, 3], function(x) { 
      2*(1-pt(abs(x), df = degFree)) 
    }
  )
  ## Confidence intervals
  output
  summary(reference)
  # return(output)
#}