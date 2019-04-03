library(invgamma)

income <- c(14,25,45,25,30,33,19,50,34,67)
m <- 3.5
n <- length(income)

t <- 0

for (v in income) {
  t <- t + (log(v) - m) ^ 2
}

tSquared <- t / n

theoretical <- function(theta, v, s) {
  return ( ((v/2) ^ (v/2)) / gamma(v/2)  * (s ^ v) * (theta ^ (-(v/2+1))) * exp((-v * s ^ 2) / (2 * theta) ))
}

range <- seq(0,10,by=0.01)
mx <- 0 * range

for (i in 1:length(range)) {
  mx[i] <- theoretical(range[i], length(income), sqrt(tSquared))
}

# Given chisquared
NormalNonInfoPrior<-function(NDraws,Data){
  Datamean<-mean(Data)
  s2<-var(Data)
  n<-length(Data)
  PostDraws=matrix(0,NDraws,2)
  PostDraws[,2]<-((n-1)*s2)/rchisq(NDraws,n-1)
  PostDraws[,1]<-Datamean+rnorm(NDraws,0,1)*sqrt(PostDraws[,2]/n)
  
  return(PostDraws)
}

Data <- c(14,25,45,25,30,33,19,50,34,67) 			# Sampling 100 observations from the N(5,10) density##
PostDraws <- NormalNonInfoPrior(10000, Data) # Generating 1000 draws from the joint posterior density of mu and sigma^2
hist(PostDraws[,1], freq=FALSE) 			# Plotting the histogram of mu-draws
lines(mx, lwd=3, type='l', col='red')
