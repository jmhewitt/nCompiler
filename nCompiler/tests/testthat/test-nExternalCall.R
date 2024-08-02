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

# basic nExternalCall: void return, tensor ref arg, single .h/.cpp file
expect_no_error({
  Radd1 <- nExternalCall(
    prototype = function(x = double(1), ans = double(1)) {},
    Cfun =  'my_internal_function',
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

# basic nExternalCall variation: basic header-only external c++ code
expect_no_error({
  Radd1_2 <- nExternalCall(
    prototype = function(x = numericVector(), ans = 'numericVector') {},
    Cfun =  'my_internal_function2',
    refArgs = 'ans',
    headerFile = 'add1-header_only.h',
    returnType = void()
  )
  cadd1_2 = nCompile(Radd1_2)
  x = 1:5
  y = 1:5
  cadd1_2(x = x, ans = y)
})
