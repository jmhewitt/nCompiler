library(nCompiler)

#
# try to get nCompiler to emit C++ code, which will ultimately call 
# a to-be-implemented C++ function nEigenImpl, which will take templated input
# types for eigen decompositions, and populate the input argument with data.
# 
# template arguments are proposed as a strategy to avoid the "include loop" 
# in which tensorOperations.h is required by the predefined classes, but we 
# want to use the predefined classes as the ideal output type for operations 
# like "eigen(x)".
#

testDecomp <- function(x) {
  ans <- eigen(x)
  return(ans)
}

nTestDecomp <- nFunction(
  fun = testDecomp, 
  argTypes = list(x = 'numericMatrix'), 
  returnType = 'EigenDecomp'
)

# code fails to compile b/c the 'EigenDecomp' type is lost when trying to 
# generate C++ for nTestDecomp.  issue seems to be that nCompile is not
cTestDecomp <- nCompile(EigenDecomp, nEigen, nTestDecomp)

# system(paste('open',tempdir()))
# Rcpp::sourceCpp(file.path(tempdir(),'nCompiler_generatedCode','nCompiler_units_1.cpp'))


#
# double check that the EigenDecomp predefined compiles when used directly
#

e <- function(x) {
  ans <- EigenDecomp$new()
  ans$values <- x
  return(ans)
}

nE <- nFunction(
  fun = e,
  argTypes = list(x = 'numericVector'),
  returnType = 'EigenDecomp'
)

cE <- nCompile(EigenDecomp, nE)

res <- cE$nE(1:5)

# output is as expected
res$values
res$vectors
