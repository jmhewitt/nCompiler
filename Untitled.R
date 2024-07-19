library(nCompiler)

# context("tensorOperations: openMP support")

#
# nCompiler code
#

opt <- nOptions()
opt$compilerOptions$cppStacktrace <- TRUE
opt$compilerOptions$useOpenMP <- TRUE
nOptions(opt)

# ##
# ## triangular and LU solver tests
# ##
# 
# # solve wrapper
# lusolve <- function(A, b) {
#   x <- solve(A, b)
#   return(x)
# }
# 
# # nCompiler implementation of forward solve with unknown matrix
# nLUsolveMat <- nFunction(
#   fun = lusolve,
#   argTypes = list(A = 'numericMatrix', b = 'numericMatrix'),
#   returnType = 'numericMatrix'
# )
# 
# # compile nFunctions
# cLUsolveMat <- nCompile(nLUsolveMat)
# 
# # test data
# L <- structure(c(1, 2, 3, 0, 1, 1, 0, 0, 2), .Dim = c(3L, 3L))
# U <- t(L)
# A <- L %*% t(L)
# x <- c(-1, 3, 1)
# x2 <- cbind(x, x)
# b <- L %*% x
# b2 <- L %*% x2
# bU <- U %*% x
# b2U <- U %*% x2
# bA <- A %*% x
# bA2 <- A %*% x2
# 
# # validate results (lower triangular system)
# expect_equal(cLUsolveMat(A = A, b = bA2), unname(x2))

##
## matrix multiplication
##

mmult <- function(x, y) {
  ans <- x %*% y
  return(ans)
}

mmultAggregator <- function(x, y, n) {
  ans <- 0
  for(i in 1:n) {
    tmp <- x %*% y
    ans <- ans + tmp[1,1]
  }
  return(ans)
}

# nCompiler implementation of matrix multiplication with matrix inputs
nMultMM <- nFunction(
  fun = mmultAggregator,
  argTypes = list(x = 'numericMatrix', y = 'numericMatrix', n = 'double'),
  returnType = 'double'
)

# compiled functions
cMultMM <- nCompile(nMultMM)

# vector dimension
n = 3e3

# square matrix
m = matrix(data = runif(n = n^2), nrow = n)

#
# matrix multiplication tests
#

# general dense matrix - matrix products
# PartialPivLU (used in nSolve)
# row-major-sparse * dense vector/matrix products

# as promised, we see multiple  threads being used
microbenchmark::microbenchmark(
  cMultMM(x = m, y = m, n = 5), times = 1
)

n = 1e3

# single core
Unit: seconds
expr      min       lq     mean   median       uq      max neval
cMultMM(x = m, y = m) 4.157974 4.172081 4.334999 4.358698 4.366579 4.619663     5

# openmp
Unit: seconds
expr      min       lq     mean   median       uq      max neval
cMultMM(x = m, y = m) 43.94204 47.57446 47.67291 47.67222 49.29588 49.87997     5

# but, openmp doesn't seem to make things go faster.  tested using gemm.

# maybe it's a thread pool issue?  or it could be a benchmarking issue.  
# microbenchmark is likely creating and destroying threads a lot.
# 
# but, pushing the microbenchmark loop inside the function doesn't help...

  # > microbenchmark::microbenchmark(
  #   +   cMultMM(x = m, y = m, n = 5), times = 1
  #   + )

# single core
Unit: seconds
expr      min       lq     mean   median       uq      max neval
cMultMM(x = m, y = m, n = 5) 20.03265 20.03265 20.03265 20.03265 20.03265 20.03265     1

# openmp
Unit: seconds
expr      min       lq     mean   median       uq      max neval
cMultMM(x = m, y = m, n = 5) 244.3771 244.3771 244.3771 244.3771 244.3771 244.3771     1

# not sure these will be terribly useful accelerations if they are only useful
# for truly massive matrices.  in order to have a reasonable statistical model,
# we need the model to have relatively quick likelihood evaluations so we can 
# run optimization or MCMC algorithms on them