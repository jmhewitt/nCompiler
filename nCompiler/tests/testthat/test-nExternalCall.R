context("nExternalCall: support for calling external C++ fns. from nFunctions")

# cpp/h test file pair
sink('add1.h')
cat('
 #include <unsupported/Eigen/CXX11/Tensor>
 void my_internal_function(Eigen::Tensor<double, 1> p, Eigen::Tensor<double, 1> & ans);
')
sink()
sink('add1.cpp') 
cat('
 #include "add1.h"
 // [[Rcpp::depends(RcppEigen)]]
 void my_internal_function(Eigen::Tensor<double, 1> p, Eigen::Tensor<double, 1> & ans) {
     /* cat reduces the double slash to single slash */ 
     ans = p + 1.0;
 }
')
sink()

# cpp/h test file pair using Rcpp types
sink('add1-rcpp.h')
cat('
#include <Rcpp.h>
 void my_rcpp_function(Rcpp::NumericVector p, Rcpp::NumericVector & ans);
')
sink()
sink('add1-rcpp.cpp') 
cat('
 #include "add1-rcpp.h"
 void my_rcpp_function(Rcpp::NumericVector p, Rcpp::NumericVector & ans) {
     /* cat reduces the double slash to single slash */ 
     ans = p + 1.0;
 }
')
sink()

# header-only test file
sink('add1-header_only.h')
cat('
 #include <unsupported/Eigen/CXX11/Tensor>
 // [[Rcpp::depends(RcppEigen)]]
 void my_internal_function2(Eigen::Tensor<double, 1> p, Eigen::Tensor<double, 1> & ans) {
  ans = p + 1.0;
 }
')
sink()

# header-only test file with pointers
sink('add1-pointers.h')
cat('
 void my_internal_function(double * p, double * ans, std::size_t n) {
  double * end = ans + n;
  for(double * it = ans; it < end; ++it) {
    *it = *(p++) + 1.0;
  }
 }
')
sink()

# header-only test file with scalar returns
sink('my-sum.h')
cat('
  #include <unsupported/Eigen/CXX11/Tensor>
  // [[Rcpp::depends(RcppEigen)]]
  double my_sum(Eigen::Tensor<double, 1> & x) {
    Eigen::Tensor<double, 0> res = x.sum();
    return res(0);
  }
')
sink()

# nExternalCall variation: scalar return
expect_no_error({
  Rsum <- nExternalCall(
    prototype = function(x = numericVector()) { },
    Cfun =  'my_sum', 
    Cpointers = FALSE,
    refArgs = 'x',
    headerFile = 'my-sum.h',
    returnType = double(0)
  )
  csum = nCompile(Rsum)
  x = 1:5
  csum(x)
})

# basic nExternalCall: void return, tensor ref arg, single .h/.cpp file
expect_no_error({
  Radd1 <- nExternalCall(
    prototype = function(x = double(1), ans = double(1)) {},
    Cfun =  'my_internal_function',
    Cpointers = FALSE,
    refArgs = 'ans',
    headerFile = 'add1.h',
    returnType = void(),
    cppFile = 'add1.cpp'
  )
  cadd1 = nCompile(Radd1)
  x = 1:5
  y = 1:5
  cadd1(x = x, ans = y)
})


# basic nExternalCall variation: numericVector versus double(1) arguments
expect_no_error({
  Radd1 <- nExternalCall(
    prototype = function(x = numericVector(), ans = 'numericVector') {},
    Cfun =  'my_internal_function',
    Cpointers = FALSE,
    refArgs = 'ans',
    headerFile = 'add1.h',
    returnType = void(),
    cppFile = 'add1.cpp'
  )
  cadd1 = nCompile(Radd1)
  x = 1:5
  y = 1:5
  cadd1(x = x, ans = y)
})

# TODO: Test/implement support for Eigen::TensorMap objects
# TODO: Test/implement support for StridedTensorMap objects

# TODO: fix conversion errors from Eigen::Tensor<double, 1> to Rcpp::NumericVector
# # basic nExternalCall variation: Rcpp types in external c++
# expect_no_error({
#   Radd1_rcpp <- nExternalCall(
#     prototype = function(x = numericVector(), ans = 'numericVector') {},
#     Cfun =  'my_rcpp_function',
#     refArgs = 'ans',
#     headerFile = 'add1-rcpp.h',
#     returnType = void(),
#     cppFile = 'add1-rcpp.cpp'
#   )
#   cadd1_rcpp = nCompile(Radd1_rcpp)
#   x = 1:5
#   y = 1:5
#   cadd1_rcpp(x = x, ans = y)
# })

# TODO: test example of nExternalCall that requires several headerFile and 
#   cppFiles.  Intuition is that all files will be copied to tmpdir() and 
#   the #include directives will include all .h files, and the appropriate cpp
#   files will be compiled as needed.  But, we'll need to make sure that we 
#   have the include sequence correct... so, it might actually just be best to 
#   tell developers to add all additional cppFiles (including additional 
#   header files) to the cppFile argument... or we could force the matter and 
#   add "additionalHeader" and "additionalCpp" arguments that simply concatenate
#   on to the cppFile argument.  to implement this idea correctly, we may need 
#   to modify the file.copy call in Rcpp_nCompiler.R (under the write cpp fn)
#   so that it can handle copying multiple files, although it looks like 
#   base::file.copy is already vectorized.  if this works, then it's a cheap 
#   way to get more complex compilation needs without having to define a 
#   separate R package.  Then, presumably, additional more complex compiler 
#   needs could be set with standard CXX_FLAGS, etc.  It may be useful to avoid
#   defining a separate R package because the Rcpp::depends attribute simply 
#   mimics "LinkingTo", which only adds -I tgtpkg/inst/include to the R CMD SHLIB
#   command.  So, basically, the dependent package needs to be header-only c++
#   code if developers want to avoid more complex linkages (cf. https://cran.r-project.org/doc/manuals/R-exts.html#Linking-to-native-routines-in-other-packages)

# basic nExternalCall variation: full file paths
expect_no_error({
  Radd1 <- nExternalCall(
    prototype = function(x = numericVector(), ans = 'numericVector') {},
    Cfun =  'my_internal_function',
    Cpointers = FALSE,
    refArgs = 'ans',
    headerFile = file.path(getwd(), 'add1.h'),
    returnType = void(),
    cppFile = file.path(getwd(), 'add1.cpp')
  )
  cadd1 = nCompile(Radd1)
  x = 1:5
  y = 1:5
  cadd1(x = x, ans = y)
})

# basic nExternalCall variation: relative file paths
cwd = getwd()
setwd(dirname(cwd))
expect_no_error({
  Radd1 <- nExternalCall(
    prototype = function(x = numericVector(), ans = 'numericVector') {},
    Cfun =  'my_internal_function',
    Cpointers = FALSE,
    refArgs = 'ans',
    headerFile = file.path(basename(cwd), 'add1.h'),
    returnType = void(),
    cppFile = file.path(basename(cwd), 'add1.cpp')
  )
  cadd1 = nCompile(Radd1)
  x = 1:5
  y = 1:5
  cadd1(x = x, ans = y)
})
setwd(cwd)

# basic nExternalCall variation: basic header-only external c++ code
expect_no_error({
  Radd1_2 <- nExternalCall(
    prototype = function(x = numericVector(), ans = 'numericVector') {},
    Cfun =  'my_internal_function2',
    Cpointers = FALSE,
    refArgs = 'ans',
    headerFile = 'add1-header_only.h',
    returnType = void()
  )
  cadd1_2 = nCompile(Radd1_2)
  x = 1:5
  y = 1:5
  cadd1_2(x = x, ans = y)
})

# basic nExternalCall variation: pass by pointers to c++ code
expect_no_error({
  Radd1_pointers <- nExternalCall(
    prototype = function(
      x = numericVector(), ans = 'numericVector', n = integer(0)
    ) { },
    Cfun =  'my_internal_function',
    refArgs = 'ans',
    headerFile = 'add1-pointers.h',
    returnType = void()
  )
  cadd1_pointers = nCompile(Radd1_pointers)
  x = 1:5
  y = 1:5
  cadd1_pointers(x = x, ans = y, n = length(x))
})
