context("tensorOperations: interoperability with sparse linear algebra methods")

library(nCompiler)
library(Rcpp)

library(spam)

##
## multivariate normal test
##


set.seed(2021)

# grid dimension
nx <- 10
ny <- nx

# precision and covariance matrices for gridded data
Q <- precmat.GMRFreglat(n = nx, m = ny, par = .1, model = 'm1p1')
Sigma <- solve(Q)
Sigma.chol <- chol(Sigma)

# reformat Q for Rcpp types
Q <- as.dgCMatrix.spam(Q)

# random variable dimension
n <- nrow(Q)

# generate a random mean vector
mu <- rnorm(n = n)

# draw from the distribution
x <- nimble::rmnorm_chol(
  n = 1, cholesky = Sigma.chol, prec_param = FALSE, mean = mu
)

# evaluate log-likelihood of sample
ll.ref <- nimble::dmnorm_chol(
  x = x, mean = mu, cholesky = Sigma.chol, prec_param = FALSE, log = TRUE
)

dmvn <- function(x, mu, Q, log) {
  # Density for a multivariate normal random variable
  #
  # Parameters:
  #  x - random vector
  #  mu - mean vector
  #  Q - precision matrix
  #  log - TRUE to return log-density

  # random vector dimension
  n <- length(x)

  # cholesky decomposition Sigma = t(Q) %*% Q
  R <- chol(Q)

  # log-determinant for chol(Q)
  ldet_chol <- sum(log(diag(R)))

  # evaluate quadratic form
  z <- x - mu
  qform <- (t(z) %*% Q %*% z)[1,1]

  res <- -.5 * n * log(2*pi) + ldet_chol - .5 * qform

  if(log) {
    return(res)
  } else {
    return(exp(res))
  }
}


#
# nCompiler code
#

opt <- nOptions()
opt$compilerOptions$cppStacktrace <- FALSE
nOptions(opt)

nDmvn <- nFunction(
  fun = dmvn,
  argTypes = list(x = 'numericVector', mu = 'numericVector',
                  Q = 'nSparseMatrix', log = 'logical'),
  returnType = 'double'
)

cDmvn <- nCompile(nDmvn)

#
# demonstration
#

# validate the R implementation
expect_equal(dmvn(x = x, mu = mu, Q = Q, log = TRUE), ll.ref)

# validate the compiled nFunction
expect_equal(cDmvn(x = x, mu = mu, Q = Q, ARG_log_ = TRUE), ll.ref)
