library(mvtnorm)

data <- read.table("eBayNumberOfBidderData.txt", header = TRUE)


########## A ##########
fit <- glm(nBids ~ 0 + ., data, family = poisson)
coeff <- fit$coefficients
plot(abs(coeff), type='h', 
     lwd=2, 
     xlab = "coefficient index", 
     main='Significance of covariates',
     ylab='absolute value of coefficient')

X <- as.matrix(data[,2:10])
## The most significant covariate is minBidShare. 

########## B ##########
sigmaPrior <- 100 * solve(t(X)%*%X)

logPois <- function(beta, y, x, ...) {
  # log likelihood of poisson model

  n <- length(x)
  
  logLik <- 0
  for (i in 1:length(n)) {
    logLik <- logLik + y[i] * t(beta)%*%x[i,] - exp( t(beta)%*%x[i,] - log(factorial(y[i])))
  }
  
  # log of prior
  logPrior <- dmvnorm(beta, mean = rep(0, 9), sigma = sigmaPrior, log=TRUE)
  
  # add 
  return(logLik + logPrior)
}

OptimResults<-optim(coeff,logPois,gr=NULL, y = data$nBids,x = X,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)


postCov <- -solve(OptimResults$hessian)
st_div <- sqrt(diag(postCov))
betaMode <- OptimResults$par

########## C ##########

gaussianSample <- function(theta, sigma, c) {
  val <- rmvnorm(1, theta, c*sigma)
  return (val)
}


RWMSampler <- function(c, it, fn, ...) {
  accRate <- 0
  sample <- c()
  prev <- gaussianSample(betaMode, postCov, c)
  for (i in 1:it) {
    candidate <- gaussianSample(prev, postCov, c)
    alpha <- min(1,exp(fn(prev, ...) - fn(candidate, ...)))
    u <- runif(1, 0, 1)
    if (alpha <= u) {
      # accept candidate
      prev <- candidate
      accRate <- accRate + 1
      # as matrix
      sample <- rbind(sample, prev)
    }
  }
  return (sample)
}

sample = RWMSampler(1,10000, logPois, data$nBids, X)
plot(sample[,3], type='l')


