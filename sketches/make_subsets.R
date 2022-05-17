library(testthat)

Rcpp::sourceCpp('sketches/make_subsets.cpp')

x = matrix(1:100, nrow = 10)
y = array(1:1e3, rep(10,3))

ind = 3
ind2 = 2

# basic dimension dropping
expect_equivalent(
  TestDropping(x = x, cdim = 0, coffset = ind - 1),
  x[ind,]
)

# test of passed/reference dimension dropping
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

# verify the decomposition of operations is valid
expect_identical(
  y[3,3:5,],
  y[3,,][3:5,]
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

# additional subset test
expect_equivalent(
  x[1:3,][,4:6],
  TestNestedSubviewRval(x = x, cdim1 = 0, cstart1 = 0, cend1 = 2, cdim2 = 1, 
                        cstart2 = 3, cend2 = 5)
)

expect_equivalent(
  y[,3:5,1][8,], 
  TestAltMixedOp(x = y, cstart1 = 2, cend1 = 4, coffset2 = 0, coffset3 = 7)
)

