# Multivariate Statistical Methods: A Primer with R. 4th Edition
# Examples. Appendix in Chapter 2. Matrix Algebra
# 
# Matrix creation
# To create the following matrix 2 x 3 (denoted A) columnwise
#       [ 2 3 1]
#       [-1 0 4]
A <- matrix(c(2, -1, 3, 0, 1, 4), nrow=2)
A
# To create matrix A row-wise. Useful in revealing the rectangular form
# of the matrix in the source vector
A <- matrix(c( 2, 3, 1,
              -1, 0, 4), nrow=2, byrow=TRUE)
A
# Matrix dimension
# Function dim() is a 2-dimensional vector carrying the number of
# rows and columns
dim(A)
# The individual number of rows and columns can be invoked with
# functions nrow() and ncol()
nrow(A)
ncol(A)
# If the length of source vector is not compatible to the dimension of
# the matrix, a warning message is displayed. In the following
# example, the length of the source vector is not a multiple of nrow=2
A1 <- matrix(c( 2, 3, 1,
               -1, 4), nrow=2, byrow=TRUE)
A1
# Vectors and matrices in R are different objects
# A vector has a length attribute, but not a dim(ension)
r <- c(3,4,6,8)
length(r)
dim(r)
# Converting vector r into a matrix: function as.matrix()
# R converts vectors into a column vector (a 1-column matrix) 
c.vec <- matrix(c(3,4,6,8))
c.vec
# Transpose: function t()
A.t <- t(A)        # A.t is a 3x2 matrix
A.t
r.vec <- t(c.vec)  # r.vec is a 1x4 matrix (row vector)
r.vec
# Creating a 3x2 zero matrix (here 3x2=6 is a multiple of the escalar 0
# dimension = 1)
matrix(0, 3, 2)
# An example of diagonal matrix
diag(c(1, 4, 0, 5)) 
# Equality of two matrices. Remember: sin(pi/2) = 1, cos(0) = 1
# where sin and cos are the trigonometric functions sine and cosine
# Here: pi is the constant number equal to the ratio of the circumference
# length to its diameter (pi=3.1415926...)
diag(c(sin(pi/2),cos(0))) == diag(2)
# Trace of a matrix
C <- matrix(c( 2,-1,0,
              -1, 4,6,
               0, 6,1), nrow=3, byrow=TRUE)
C
trace.C <- sum(diag(C))
trace.C
# Computing the trace of a matrix with the function tr() in psych package
# Before running library(psych) make sure you have installed the package psych
library(psych)
B <- matrix(c(1, 2, 3, 4), 2, byrow=TRUE)
B
tr(B)     # This IS NOT the transpose of B, but the trace of B
# Addition and subtraction of matrices; multiplication of a matrix by a scalar
A <- matrix(c( 3, 1, 4,
              -1, 0, 2), 2, byrow=TRUE)
A
B <- matrix(c( 1, 1,-2,
               0, 3,-2), 2, byrow=TRUE)
B
A+B
A-B
2*A
# Multiplication of two matrices: %*%
A <- matrix(c( 3, 1, 4,
              -1, 0, 2), 2, byrow=TRUE)
B <- matrix(c( 1,-1,
               2, 0,
               3, 1), 3, byrow=TRUE)
A%*%B
B%*%A
# If matrices are not compatible, an error message is produced
C <- matrix(c(1,-1,
              2, 1), 2, byrow=TRUE)
A%*%C
# Matrix inversion in R: the solve function.
# In Section 2.4 it was shown that the inverse of
#    [2  1]
#    [1  2]
# is
#    [ 2/3  -1/3]
#    [-1/3   2/3]
M <- matrix(c( 2, 1,
               1, 2), 2, byrow=TRUE)
solve(M)
# The determinant of M is different to zero (3)
det(M)
# An example of a singular matrix
N <- matrix(c(2, 1,
              4, 2), 2, byrow=TRUE)
det(N)
solve(N)
# An example of an orthogonal matrix
P <- matrix(c( 0,-1, 0,
               1, 0, 0,
               0, 0,-1), 3, byrow=TRUE)
P
P.t <- t(P)
P.t
P.tinv <- solve(P)
P.tinv
# Quadratic forms. Here x is avector, but it is coerced into a matrix
x <- c(1,2)
A <- matrix(c( 1, 3,
               3,-1), 2, byrow=TRUE)
Q <- t(x)%*%A%*%x
Q
#
# Eigenvalues and eigenvectors: examples using the corresponding R functions 
# will be postponed until Chapter 6
#
# Vectors of means, covariance and correlation matrices
# Calculations are illustrated with the Bumpus' sparrows data
sparr <-  read.table("Bumpus_sparrows.txt", header=TRUE)
# Selecting the numeric variables in data frame sparr.
# A negative number when indexing columns in a data frame indicates that
# the new data frame WILL NOT CONTAIN those columns. Below: the first column
# containing the factor "Survivorship" will be omitted
sparr.num <- sparr[,-1]
#
sparr.num
# Computing the means of each variable in data frame sparr.num
colMeans(sparr.num)
# Covariance matrix
cov(sparr.num)
# Correlation matrix
cor(sparr.num)