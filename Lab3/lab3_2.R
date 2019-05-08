
data <- read.table("eBayNumberOfBidderData.txt", header = TRUE)


########## A ##########
fit <- glm(nBids ~ 0 + ., data, family = poisson)
coeff <- fit$coefficients
plot(abs(coeff), type='h', lwd=2, xlab = "coefficient index")

## The most significant covariate is minBidShare. 

########## B ##########

logPois <- function() {
  # log likelihood of poisson model
  
  # log of prior
  
  # add 
  
}


OptimResults<-optim(coeff,logPois,gr=NULL,data$nBids,X,mu,sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)