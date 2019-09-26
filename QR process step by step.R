my.QR <- function(y, X) {
  X <- as.matrix(X)
  y <- as.vector(y)
  n_col <- as.integer(ncol(X))
  
  n_row <- as.integer(nrow(X))
  
  X <- qr(X)
  beta <- solve(t(X) %*% X, t(X) %*% y)
  e <- as.vector(y - X %*% beta) 
  R2 <- (X[1:n_col, 1:n_col])^2
  return(list(beta = beta, e= e, R2 = R2 ))
}

X <- iris


y <- iris$Sepal.Length

my.QR(y, X)




f2 <- function (X, y) {
  ## QR factorization `X = QR`
  QR <- qr.default(X)
  ## After rotation of `X` and `y`, solve upper triangular system `Rb = Q'y` 
  b <- backsolve(QR$qr, qr.qty(QR, y))
  ## residuals
  e <- as.numeric(y - X %*% b)
  ## R-squared
  RSS <- crossprod(e)[1]
  TSS <- crossprod(y - mean(y))[1]
  R2 <- 1 - RSS / TSS
  ## multiple return
  list(coefficients = b, residuals = e, R2 = R2)
}


f2(X,y)
