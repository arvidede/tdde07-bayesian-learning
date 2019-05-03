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

postCov <- -solve(OptimResults$hessian)
st_div <- sqrt(diag(postCov))
betaMode <- OptimResults$par
nSmallChild <- postDraws[,7]
postDist <- dnorm(x = seq(0,10,0.01), mean = betaMode[7], sd = st_div[7])

# plot posterior distribution NSmallChild parameter
cred_int <- quantile(postDraws, c(0.025, 0.975))
plot(postDist, 
     xlim = c(0,100),
     ylim = c(0,0.002),
     type = 'l')
abline(v = cred_int[1], col='red')
abline(v = cred_int[2], col='blue')

########## c ###########
y <- c(constant = 1, 
       husbandInc = 10,
       educYears = 8, 
       expYears = 10,
       expYears2 = 1,
       age = 40, 
       nSmallChild = 1,
       nBigChild = 1)

n_draws <- 1000

predDist <- function(n, beta, sigma, y) {
  y_draws = c()
  for (i in 1:n) {
    #beta draw
    betaDraw = as.vector(rmvnorm(1, mean = beta, sigma = sigma))
    #probability
    p <- exp(y%*%betaDraw)/(1+exp(y%*%betaDraw))
    #prediction of y
    y_draw = rbinom(1, 1, p)
    y_draws = c(y_draws, y_draw)
  }
  return (y_draws)
}

draws = predDist(n_draws, betaMode, postCov, y)
workProb <- sum(draws == 1) 

