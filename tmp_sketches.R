library(testthat)

Rcpp::sourceCpp('tmp_code.cpp')

# source data
m = matrix(runif(100),nrow=10)
a = array(runif(1000), dim = rep(10,3))

# specs for subset
inds1 = c(1,3,5)
inds2 = c(3,8)
inds3Range = c(3,7)

# element to extract from subsets
elem = c(2,2,1)

# low-level extraction, given individual element locations i0 and i1
expect_identical(
  Operator_i0_i1(
    x = m, 
    inds1 = inds1 - 1, # "-1" to offset for 0-based indexing at C++ level
    inds2 = inds2 - 1, 
    i0 = elem[1] - 1, 
    i1 = elem[2] - 1
  ),
  m[inds1,inds2][elem[1],elem[2]]
)

# standard extraction with vector subsets, given index vector for indsOut
expect_identical(
  Operator_indVec(
    x = m, 
    inds1 = inds1 - 1, # "-1" to offset for 0-based indexing at C++ level
    inds2 = inds2 - 1, 
    indsOut = elem - 1
  ),
  m[inds1,inds2][elem[1],elem[2]]
)

# TODO: test with logical vectors
# TODO: implement within Eigen as a class that dispatches member access via a
#  wrapped Eigen::TensorMap object b/c the TensorMap objects themselves don't 
#  do any clever handling of the mapping, so we should piggyback as much as 
#  possible on the existing implementation.  TensorMap objects access elements
#  by fully-realizing the index vector, then dispatching to a helper class.  so, 
#  we do no worse by simply replicating this behavior

# subsetting including a number range
expect_identical(
  Operator_Mixtures(
    data = a, 
    dim = dim(a),
    inds1 = inds1 - 1, # "-1" to offset for 0-based indexing at C++ level
    inds2 = inds2 - 1,
    inds3range = inds3Range - 1,
    indsOut = elem - 1
  ),
  a[inds1,inds2,inds3Range[1]:inds3Range[2]][elem[1],elem[2],elem[3]]
)

# subsetting including a logical vector
lvec2 = rep(FALSE, dim(a)[2])
lvec2[inds2] = TRUE
expect_identical(
  Operator_LogicalMixtures(
    data = a, 
    dim = dim(a),
    inds1 = inds1 - 1, # "-1" to offset for 0-based indexing at C++ level
    inds2Logical = lvec2,
    inds3range = inds3Range - 1,
    indsOut = elem - 1
  ),
  a[inds1,inds2,inds3Range[1]:inds3Range[2]][elem[1],elem[2],elem[3]]
)
