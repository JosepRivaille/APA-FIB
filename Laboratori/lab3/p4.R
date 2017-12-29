data <- matrix(c(1, 2, 3, 4, 5, 7.97, 10.2, 14.2, 16.0, 21.2), nrow = 5, byrow = FALSE)

phi <- matrix(c(rep(1, 5), data[,1]), nrow = 5)

# Pseudo-inverse
library('MASS')
MP <- ginv(phi, tol = sqrt(.Machine$double.eps))
w1 <- MP %*% data[,2]

# SVD
sv <- svd(phi)
V <- sv$v
U <- sv$u
D <- diag(pmax(1 / sv$d, 0))
w2 <- V %*% D %*% t(U) %*% data[,2] 