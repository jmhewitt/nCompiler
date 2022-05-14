library(testthat)

Rcpp::sourceCpp('tmp_reference.cpp')

#
# testing base features of eigen
#

m = 11
n = 10

x = matrix(runif(n = m * n), nrow = m)
y = runif(n = n)

x = matrix(1:(m*n), nrow = m)
z = x
z[3,] = y

# Eign can assign into Eigen::Tensor::chip operations
expect_identical(z, chipAssign(x = x, y = y, cdim = 0, coffset = 2))

# copy constructor valid for Eigen::TensorMap objects
expect_identical(3 * x, mapOfMap(x = x, y = 2 * x))

#
# testing uses of strided tensor maps
#

Rcpp::sourceCpp('tmp_reference.cpp')

x = matrix(1:100, nrow = 10)


# a strided tensor map of a strided tensor map is not the same as subsetting a 
# subset
expect_equal(
  x[1:3,][,4:6],
  nCompilerMapOfMap(x = x, xmin = 1, xmax = 3, ymin = 4, ymax = 6)
)

Rcpp::sourceCpp('tmp_reference.cpp')

x[2:3, , drop = FALSE][, 1:1, drop = FALSE]

tensorView(x = x, cminx = 1, cmaxx = 2, cminy = 0, cmaxy = 0)


#
# testing multiple subsetting via Eigen chipping
#

Rcpp::sourceCpp('tmp_reference.cpp')
library(testthat)

ind1 = 3
ind2 = 2

expect_equal(
  as.numeric(x[,ind1][ind2]),
  as.numeric(chipOfChip(
    x = array(data = x, dim = c(dim(x), 1)), 
    cdim1 = 1, 
    coffset1 = ind1 -1, 
    cdim2 = 0, 
    coffset2 = ind2 - 1
  ))
)

expect_equal(
  as.numeric(x[ind1,][ind2]),
  as.numeric(chipOfChip(
    x = array(data = x, dim = c(dim(x), 1)), 
    cdim1 = 0, 
    coffset1 = ind1 - 1, 
    cdim2 = 0, 
    coffset2 = ind2 - 1
  ))
)


#
# testing multiple subsetting within nCompiler
#

library(nCompiler)


# strided tensor maps don't currently support this; we need to extend their 
# constructor s.t. it works with more than just lvalue objects (i.e., ref's).
#
# one way to try to approach this might be to use SFINAE with the strided 
# tensor map ::make function s.t. if the argument is a strided tensor map, then
# the ::make function will update the underlying strided tensor map and return
# it as an updated reference, rather than trying to create a new one
multiple_subset = function(x, inds1, inds2) {
  
  # this is not yet supported!
  # return(x[inds1,][,inds2])  
  
  # this will not compile either due to rval reference issue...
  # generated c++ is makeStridedTensorMap( makeStridedTensorMap(...)), but the
  # function requires an rval reference as input.
  # return(x[1:3,][,4:6])
  
  # this sequence of code compiles, but is not a sufficient approximation for
  # use of "nested" strided tensor maps, especially for argument passing
  s1 <- x[1:3,] # s1 is defined as an Eigen::Tensor
  s2 <- s1[,4:6] # s1 is defines as an Eigen::Tensor
  # s2 <- s2 + 1 # does not compile
  return(s2)
}

x = matrix(1:100, nrow = 10)

inds1 = c(1,3,5)
inds2 = c(2,4,6)


nMS = nFunction(
  fun = multiple_subset, 
  argTypes = list(
    x = 'integerMatrix',
    inds1 = 'numericVector',
    inds2 = 'numericVector'
  ), 
  returnType = 'integerMatrix'
)

debugonce(nCompiler:::genCppEnv$EigenCast)
cMS = nCompile(nMS)

expect_identical(
  multiple_subset(x, inds1, inds2),
  # cMS(x, inds1, inds2)
  nFun_7_NFID_7(x, inds1, inds2)
)

system(paste('open',tempdir()))
Rcpp::sourceCpp(file.path(tempdir(), 'nCompiler_generatedCode', 'nCompiler_units_6.cpp'))
