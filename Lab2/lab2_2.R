######## D ########

womenData = read.table("WomenWork.txt", header=TRUE)

# Added a zero in the model formula so that R doesn’t add an extra intercept
# A . in the model formula means to add all other variables in the dataset as features
glmModel <- glm(Work ~ 0 + ., data = womenData, family = binomial)

# Param setup
tau <- 10
nParams <- 8
mu <- as.vector(rep(0, nParams))
X <- as.matrix(womenData[,2:9])
y <- as.matrix(womenData[,1])
initVal <- as.vector(rep(0,nParams))
sigma <-  tau^2 * diag(nParams)
initBetas <- rep(0,nParams)


# Calculate the Log Posterior Logistic
logPost <- function(beta,y,X,mu,Sigma) {
  
  # Calculate predictions by multiplying data with betas
  pred <- X%*%beta
  
  # Log likelihood function, derived by taking the log of the likelihood function prod(e^pred*y/(1+e^pred))
  logLik <- sum(y * pred - log(1 + exp(pred)))
  
  # 
  logPrior <- dmvnorm(beta, matrix(0,nParams,1), Sigma, log=TRUE);
  
  return (logPrior + logLik)
}

OptimResults<-optim(initBetas,logPost,gr=NULL,y,X,mu,sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

postHessian <- sqrt(diag(-solve(OptimResults$hessian)))[7]
betaMode <- OptimResults$par[7]
nSmallChild <- postDraws[,7]
postDraws <- dnorm(x=nSmallChild, postHessian)
plot(postDraws)

