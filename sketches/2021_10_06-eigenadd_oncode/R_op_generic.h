#include "preamble.h"

using Eigen::Tensor;
using Eigen::DefaultDevice;

/**
 * Templated reshaping to use the Eigen library to lazily evaluate a `op` b 
 * when a and b are specializations of Eigen::TensorBase objects with
 * different dimensions
 * 
 * Eigen::Tensorbase specializations numerically represent tensors 
 * (i.e., via Eigen::Tensor) and the results of performing mathematical 
 * operations on tensors (i.e., such as a + b via Eigen::TensorCwiseBinaryOp).
 * Eigen uses templates to generate C++ code that exploits compile-time 
 * optimizations that reduce redundant operations and temporary objects.
 * 
 * The arguments a and b must have the same number of elements, otherwise an 
 * error will be thrown.  For example, the intended use is for when a is either 
 * a column or row matrix, and b is a vector of the same length.
 * 
 * The template function relies on automatic template deduction to allow the 
 * function to be called recursively without requiring users to explicitly 
 * declare object types.
 * 
 * @param a specialization of Eigen::TensorBase (i.e., Eigen::Tensor, or 
 *   Eigen::TensorCwiseBinaryOp)
 * @param b specialization of Eigen::TensorBase (i.e., Eigen::Tensor, or 
 *   Eigen::TensorCwiseBinaryOp)
 * @param o a binary operator suitable for use with Eigen::TensorBase objects
 * 
 * @return a `op` b, with dimensions matching those of the argument a
 */
template<typename OP_, typename A_, typename B_>
auto R_binaryOp_t1_t2(const A_ &a, const B_ &b, const OP_ &o) -> decltype(
  a.binaryExpr(
    b.reshape(
      Eigen::TensorRef<
        Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>
      >(a).dimensions()
    ), 
    o
  )
) {
  // dimensions of a
  Eigen::TensorRef<
    Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>
  > aEval(a);
  auto aDim = aEval.dimensions();
  // dimensions of b
  Eigen::TensorRef<
    Eigen::Tensor<typename B_::Scalar, B_::NumDimensions>
  > bEval(b);
  // throw runtime error if number of elements differ
  if(aEval.size() != bEval.size()) {
      throw std::range_error("R_binaryOp_t1_t2 :: Tensors have unequal size.\n");
  }
  // reshape b into a tensor with a's dimensions
  auto y = b.reshape(aDim);
  // perform a `op` b
  return a.binaryExpr(y, o);
}