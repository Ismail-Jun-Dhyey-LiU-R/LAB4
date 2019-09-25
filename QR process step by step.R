my.QR <- function(y, X) {
  X <- as.matrix(X)
  y <- as.vector(y)
  n_col <- as.integer(ncol(X))
  
  n_row <- as.integer(nrow(X))
  
  qr.X <- qr(X)
  beta <- solve(t(X) %*% X, t(X) %*% y)
  e <- as.vector(y - X %*% beta) 
  R2 <- (X[1:n_col, 1:n_col])^2
  return(list(beta = beta, e= e, R2 = R2 ))
}

X <- as.matrix(iris)
X<- diag(X)

y <- as.matrix(iris$Sepal.Length)
y <- diag(y)
y <- all.vars(y)
my.QR(y, X)

