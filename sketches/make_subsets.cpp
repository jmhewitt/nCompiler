#include <RcppEigen.h>
#include <nCompiler/nCompiler_Eigen.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

// GOAL: StridedTensorMap-like functionality via chipping and slicing

namespace nCompiler {

    // template meta-programming aid
    template<typename T>
    struct BaseType { typedef T type; };

    // partial specialization to get non-ref. type from template ref. params
    template<typename T>
    struct BaseType<T&> { typedef T type; };

    /**
     * Do not perform any subsetting
     */
     struct FullDim {
         template<typename T>
         auto op(T&& x) -> decltype(x) { return std::forward<T>(x); }
     };

    /**
     * Implement an Eigen chip operation
     */
    struct DropDim {

        Eigen::Index m_dim, m_offset;

        // constructor specifies parameters of the operation
        DropDim(Eigen::Index dim, Eigen::Index offset) :
            m_dim(dim), m_offset(offset) { }

        template<typename T>
        auto op(T&& x) -> decltype(
            // return type will be an Eigen operation
            x.chip(m_offset, m_dim)
        ) {
            return x.chip(m_offset, m_dim);
        }

    };

    /**
     * Implement a slice on a single dimension of a tensor, yielding a subview
     * of the input
     */
    struct SubView {

        Eigen::Index m_dim, m_start, m_end;

        // constructor specifies parameters of the operation
        SubView(Eigen::Index dim, Eigen::Index start, Eigen::Index end) :
            m_dim(dim), m_start(start), m_end(end) { }

        template<typename T>
        auto op(T&& x) -> decltype(
            // return type will be an Eigen operation
            x.slice(
                Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>(),
                Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>()
            )
        ) {
            // initialize slice offsets and extents
            Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> offsets;
            Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> extents;
            offsets.fill(0);
            extents.fill(0);
            // set the start of the subview in the specified dimension
            offsets[m_dim] = m_start;
            // get dimension information for the object being subsetted
            Eigen::TensorRef<
                Eigen::Tensor<typename BaseType<T>::type::Scalar,
                BaseType<T>::type::NumDimensions>
            > xref(x);
            auto dim = xref.dimensions();
            // subview fully spans all dimensions except the subsetted dim.
            for(Eigen::Index i = 0; i < BaseType<T>::type::NumDimensions; ++i) {
                extents[i] = i == m_dim ? m_end - m_start + 1 : dim[i];
            }
            // execute slice
            return x.slice(offsets, extents);
        }

    };

}

/*
 * examples of ideal c++ code for reproducing various contiguous-block
 * subsetting operations from R.  the strategy is to be able to generate
 * "chainable" code that requires little overhead for use.
 */

using namespace nCompiler;

// [[Rcpp::export]]
Eigen::Tensor<double, 1> TestDropping(
    Eigen::Tensor<double, 2> x,
    Eigen::Index cdim,
    Eigen::Index coffset
) {
    return DropDim(cdim, coffset).op(x);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> TestNestedDroppingRval(
        Eigen::Tensor<double, 3> x,
        Eigen::Index cdim1,
        Eigen::Index coffset1,
        Eigen::Index cdim2,
        Eigen::Index coffset2
        ) {
    // this does (nested) subsetting similar to makeStridedTensorMap, but
    // the operations are explicit, and there is no need for formal IndexBlock
    // classes, which implicitly define the subsetting operations to use.
    // the strategy below also does not require any template parameters to be
    // explicitly specified, such as the object's output dimension
    return DropDim(cdim2, coffset2).op(DropDim(cdim1, coffset1).op(x));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> TestNestedDroppingLval(
        Eigen::Tensor<double, 3> x,
        Eigen::Index cdim1,
        Eigen::Index coffset1,
        Eigen::Index cdim2,
        Eigen::Index coffset2
        ) {
    // the output of the tools are Eigen operations, which can be used elsewhere
    auto s1 = DropDim(cdim1, coffset1).op(x);
    // the tools can also work with Eigen expressions
    return DropDim(cdim2, coffset2).op(s1);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> TestSubview(
    Eigen::Tensor<double, 2> x,
    Eigen::Index cdim,
    Eigen::Index cstart,
    Eigen::Index cend
) {
    return SubView(cdim, cstart, cend).op(x);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> TestNestedSubviewRval(
        Eigen::Tensor<double, 2> x,
        Eigen::Index cdim1,
        Eigen::Index cstart1,
        Eigen::Index cend1,
        Eigen::Index cdim2,
        Eigen::Index cstart2,
        Eigen::Index cend2
) {
    return SubView(cdim2, cstart2, cend2).op(
        SubView(cdim1, cstart1, cend1).op(x)
    );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> TestNestedSubviewLval(
    Eigen::Tensor<double, 2> x,
    Eigen::Index cdim1,
    Eigen::Index cstart1,
    Eigen::Index cend1,
    Eigen::Index cdim2,
    Eigen::Index cstart2,
    Eigen::Index cend2
) {
    auto s1 = SubView(cdim1, cstart1, cend1).op(x);
    return SubView(cdim2, cstart2, cend2).op(s1);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> TestMixedOp(
    Eigen::Tensor<double, 3> x,
    Eigen::Index cdim1,
    Eigen::Index coffset1,
    Eigen::Index cdim2,
    Eigen::Index cstart2,
    Eigen::Index cend2
) {
    return SubView(cdim2, cstart2, cend2).op(DropDim(cdim1, coffset1).op(x));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> TestMixedOpWriting(
    Eigen::Tensor<double, 3> x,
    Eigen::Tensor<double, 2> y,
    Eigen::Index cdim1,
    Eigen::Index coffset1,
    Eigen::Index cdim2,
    Eigen::Index cstart2,
    Eigen::Index cend2
) {
    SubView(cdim2, cstart2, cend2).op(DropDim(cdim1, coffset1).op(x)) = y;
    return x;
}

/**
 * operations like x[,3:5,1][8,]...
 */
// [[Rcpp::export]]
Eigen::Tensor<double, 1> TestAltMixedOp(
    Eigen::Tensor<double, 3> x,
    Eigen::Index cstart1,
    Eigen::Index cend1,
    Eigen::Index coffset2,
    Eigen::Index coffset3
) {
    return DropDim(0,coffset3).op(FullDim().op(FullDim().op(SubView(1,cstart1,cend1).op(DropDim(2,coffset2).op(x)))));
}

/**
 * operations like x[,2,][i,j] without operator=.  Code like this might be
 * useful as a means to support sequences of subset operators (i.e., the demo
 * x[,2,][i,j]).  Otherwise, it is probably more efficient to use operator() to
 * directly access elements of a tensor (i.e., x[3,1,4]).  The two scenarios can
 * probably be detected by modifying the Bracket handler in nCompiler to
 * recognize the two scenarios by checking to see what the caller for Bracket
 * is.  If the caller for Bracket is another Bracket, (or if the type being
 * subsetted is a TensorRefBlock) then we'll use this strategy instead.
 */
 // [[Rcpp::export]]
 double TestImplicitEval(
     Eigen::Tensor<double, 3> x,
     Eigen::Index coffset1,
     Eigen::Index ci,
     Eigen::Index cj
 ) {
    return static_cast<Eigen::Tensor<double, 0> >(DropDim(0,ci).op(DropDim(1,cj).op(FullDim().op(DropDim(1,coffset1).op(FullDim().op(x))))))();
 }
