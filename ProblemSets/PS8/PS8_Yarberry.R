# PS8 Yarberry 

# Problem 4 
set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5

X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1 # first column of X should be all ones
eps <- rnorm(N,mean=0,sd=0.5)
betaTrue <- as.vector(runif(K))
Y <- X%*%betaTrue + eps

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Problem 5  
OLS <- as.vector(-2*t(X)%*%(y-X%*%beta))
OLS

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Problem 6 ~ Stepwise 
# set up a stepsize
alpha <- 0.0000003
## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}
# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*X%*%(y-t(X)%*%beta)) )
}
## read in the data
y <- 100000
X <- 10
## initial values
beta <- runif(dim(X)[1]) #start at uniform random numbers equal to number of coefficients
# randomly initialize a value to beta
set.seed(100)
# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),iter)
# stochastic gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-12) {
  # Randomly re-order the data
  random <- sample(nrow(X))
  X <- X[random,]
  y <- y[random]
  # Update parameters for each row of data
  for(i in 1:dim(X)[1]){
    beta0 <- beta
    beta <- beta0 - alpha*gradient(beta0,y[i],as.matrix(X[i,]))
    beta.All[,i] <- beta
  }
  alpha <- alpha/1.0005
  if (iter%%1000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Problem 7 
install.packages("nloptr")
library(nloptr)
## Our objective function
eval_f <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

## Gradient of our objective function
eval_grad_f <- function(beta,y,X) {
  return ( as.vector(-2*X%*%(y-t(X)%*%beta)) )
}

## initial values
x0 <- 1

## Algorithm parameters
opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8)

## Find the optimum!
res <- nloptr( x0=x0,eval_f=eval_f,eval_grad_f=eval_grad_f,opts=opts)
print(res)

# Nelder-Mead Code 
## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

## initial values
xstart <- 1

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

## Find the optimum!
res <- nloptr( x0=xstart,eval_f=objfun,opts=options)
print(res)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Problem 8 
gradient <- function(theta ,Y,X) { 
  grad <- as.vector(rep(0,length(theta))) 
  beta <- theta[1:(length(theta)-1)] 
  sig <- theta[length(theta)] 
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2) 
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig ^3) 
  return ( grad ) }

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Problem 9 
estimates <- lm(Y~X -1)
print(summary(estimates))

library(stargazer)
stargazer(estimates)
