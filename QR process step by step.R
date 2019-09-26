
library(iris)
QR <- function(X,y){
  # Create response vector y
  y <- iris$Sepal.Length
  
  # Matrix of feature variables from Iris
  X <- as.matrix(iris[-ncol(iris)])
  
  # vector of ones with same length as rows in Iris
  I <- rep(1, length(y))
  
  # Add intercept column to X
  X <- cbind(I, X)
  n_col <- as.integer(ncol(X))
  
  n_row <- as.integer(nrow(X))
  # Implement closed-form solution
  betas <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Round for easier viewing
  betas <- round(betas, 2)
  betas
  
  e <- as.vector(y - X %*% betas)
  e
  R2 <- (X[1:n_col, 1:n_col])^2
  R2
  
  Yhat <- X %*% betas
  # degrees of freedom 
  n <- length(y)
  p <- (length(all.vars(formula))-1)
  df <- n - p
  
  
  
  
  return(list(betas = betas, e= e, R2 = R2, fittedV = Yhat, DegreesFreedom = df ))
}

QR(X,y)

#instead of QR we multiply by the identity matrix 

