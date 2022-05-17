library(testthat)

Rcpp::sourceCpp('sketches/make_subsets.cpp')

x = matrix(1:100, nrow = 10)
y = array(1:1e3, rep(10,3))
z = matrix(1:100, nrow = 10) + runif(n = 1)

ind = 3
ind2 = 2

# basic dimension dropping
expect_equivalent(
  TestDropping(x = x, cdim = 0, coffset = ind - 1),
  x[ind,]
)

# test of passed/reference dimension dropping (i.e., the dimension being 
# subsetted can be specified at runtime OR at compile time)
expect_equivalent(
  TestNestedDroppingLval(
    x = y, 
    cdim1 = 1, coffset1 = ind - 1,
    cdim2 = 0, coffset2 = ind2 - 1
  ),
  y[,ind,][ind2,]
)

# test of chained dimension dropping
expect_equivalent(
  TestNestedDroppingRval(
    x = y, 
    cdim1 = 1, coffset1 = ind - 1,
    cdim2 = 0, coffset2 = ind2 - 1
  ),
  y[,ind,][ind2,]
)

# basic views of a matrix
expect_equivalent(
  x[,3:5], 
  TestSubview(x = x, cdim = 1, cstart = 2, cend = 4)
)

# passed/reference views of a matrix
expect_equivalent(
  x[,3:5][1:3,], 
  TestNestedSubviewLval(
    x = x, cdim1 = 1, cstart1 = 2, cend1 = 4, cdim2 = 0, cstart2 = 0, cend2 = 2
  )
)

# chained views of a matrix
expect_equivalent(
  x[,3:5][1:3,], 
  TestNestedSubviewRval(
    x = x, cdim1 = 1, cstart1 = 2, cend1 = 4, cdim2 = 0, cstart2 = 0, cend2 = 2
  )
)

# verify the Eigen implementation is valid
expect_equivalent(
  y[3,,][3:5,],
  TestMixedOp(x = y, cdim1 = 0, coffset1 = 2, cdim2 = 0, cstart2 = 2, cend2 = 4)
)

# verify we can write/modify the underlying tensors through our maps
ymod = y
ymod[3,,][3:5,] = x[3:5,]
expect_equivalent(
  ymod,
  TestMixedOpWriting(x = y, cdim1 = 0, coffset1 = 2, cdim2 = 0, cstart2 = 2, 
                     cend2 = 4, y = x[3:5,])
)

# Alternatives can support subsets of subsets
expect_equivalent(
  x[1:3,][,4:6],
  TestNestedSubviewRval(x = x, cdim1 = 0, cstart1 = 0, cend1 = 2, cdim2 = 1, 
                        cstart2 = 3, cend2 = 5)
)

# Alternatives can support subsets of subsets
expect_equivalent(
  y[,3:5,1][8,], 
  TestAltMixedOp(x = y, cstart1 = 2, cend1 = 4, coffset2 = 0, coffset3 = 7)
)

# Alternatives can support subsets of subsets
expect_equivalent(
  y[,2,][4,10], 
  TestImplicitEval(x = y, coffset1 = 1, ci = 3, cj = 9)
)

# Alternatives can be applied to Tensor operations
expect_equivalent(
  (x + z)[1:3,3:5],
  TestSubsettingOps(x = x, y = z, cxmin = 0, cxmax = 2, cymin = 2, cymax = 4)
)

expect_equivalent(
  x[1:3,3:6] + z[1:3,3:6],
  TestSubsettingThenOps(
    x = x, y = z[1:3,3:6], cxmin = 0, cxmax = 2, cymin = 2, cymax = 5
  )
)


#
# demonstrate needed functionality
#

library(nCompiler)

nf <- nFunction(
  fun = function(x) {
    return(x[1,][3])
  }, 
  argTypes = list(x = 'numericMatrix'), 
  returnType = 'double'
)

# StridedTensorMap doesn't support subsets of subsets
expect_error(nCompile(nf))


nf <- nFunction(
  fun = function(x, y) {
    res <- (x + y)[1:3,3:5]
    return(res)
  }, 
  argTypes = list(x = 'numericMatrix', y = 'numericMatrix'), 
  returnType = 'numericMatrix'
)

# StridedTensorMap not designed to be applied to Tensor operations
expect_error(nCompile(nf))
