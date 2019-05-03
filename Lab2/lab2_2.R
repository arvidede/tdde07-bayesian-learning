######## D ########

womenData = read.table("WomenWork.txt", header=TRUE)

# Added a zero in the model formula so that R doesn’t add an extra intercept
# A . in the model formula means to add all other variables in the dataset as features
glmModel <- glm(Work ~ 0 + ., data = womenData, family = binomial)

# Param setup
tau <- 10
beta_prior = as.vector(rmvnorm(1, rep(0, 8), tau^2 * diag(8)))
X <- as.matrix(womenData[,2:9])
y = as.matrix(womenData[,1])
pred <- X%*%beta_prior

logPost <- function(betaVect,y,X,mu,Sigma) {
  
}

OptimResults<-optim(beta_prior,logPost,gr=NULL,y,,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)