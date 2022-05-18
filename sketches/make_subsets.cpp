#include <RcppEigen.h>
#include <nCompiler/nCompiler_Eigen.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

//
// Proposed code to add to nCompiler
//

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

         template<typename T>
         auto operator()(T&& x) -> decltype(op(x)) { return op(x); }
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

        template<typename T>
        auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

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

        template<typename T>
        auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

    };

    /**
     * Implement a slice on N dimensions of a tensor, yielding a subview
     * of the input
     */
    template<Eigen::Index N>
    struct SubViewN {

        // array elements assumed to be {dimension, start, end}
        typedef std::array<Eigen::Index, 3> SliceDetails;

        std::array<SliceDetails, N> m_config;

        // couldn't get this to compile with std::array<SliceDetails> as arg
        SubViewN(std::initializer_list<SliceDetails> config) {
            // couldn't seem to transfer objects using operator= or memcpy
            auto cfg = config.begin();
            auto mcfg = m_config.begin();
            auto cfgend = config.end();
            while(cfg != cfgend) {
                *(mcfg++) = *(cfg++);
            }
        }

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
            // get dimension information for the object being subsetted
            Eigen::TensorRef<
                Eigen::Tensor<typename BaseType<T>::type::Scalar,
                BaseType<T>::type::NumDimensions>
            > xref(x);
            auto dim = xref.dimensions();
            // initialize subview to fully span all dimensions
            for(Eigen::Index i = 0; i < BaseType<T>::type::NumDimensions; ++i) {
                extents[i] = dim[i];
            }
            // transfer slice parameters
            auto cfgend = m_config.end();
            for(auto cfg = m_config.begin(); cfg != cfgend; ++cfg) {
                offsets[(*cfg)[0]] = (*cfg)[1];
                // must modify extents twice in fn. b/c cfg might not be sorted
                extents[(*cfg)[0]] = (*cfg)[2] - (*cfg)[1] + 1;
            }
            // execute slice
            return x.slice(offsets, extents);
        }

        template<typename T>
        auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

    };

}

//
// End: Proposed code to add to nCompiler
//

//
// Tests and demonstrations of proposed subsetting code (contiguous views)
//

/*
 * Improvements/enhancements over StridedTensorMap:
 *   1) Offers a code-generation strategy for multiple subsetting, (i.e., x[][])
 *   2) Can be applied to Tensor objects, or Tensor expressions (i.e., x + y)
 *
 * TODO:
 *   1) Will need to implement logical/integer subsetting by writing a custom
 *      Tensor op.  We can probably keep things simpler by a) only allowing the
 *      operation to work on a single dimension at a time, and b) using the
 *      TensorChipping operation as a template to build from.
 *   2) Can try to implement "TensorRefBlocks" as nCompiler objects in R by
 *      using external pointers to the Eigen Tensor operation class created by
 *      subsetting operations.  The idea is to borrow ideas from R packages that
 *      implement database connections using external pointers.
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
 * operations like x[,3:5,1][8,]..., but implemented with list initialization
 * and overloaded function call operators as a possible way to improve
 * readability of generated code.  here the syntax for generated code makes the
 * subsetting resemble a function call with a "dynamically generated" function,
 * which of course, is really just the class.  So, for nCompiler code
 * generation, the hope is that this implementation will be simpler to generate
 * c++ code for.
 *
 * Syntax: SUBSETTYPE{PARAMS}(OBJ. TO SUBSET)
 */
// [[Rcpp::export]]
Eigen::Tensor<double, 1> TestAltMixedOpListInit(
    Eigen::Tensor<double, 3> x,
    Eigen::Index cstart1,
    Eigen::Index cend1,
    Eigen::Index coffset2,
    Eigen::Index coffset3
) {
    return DropDim{0,coffset3}(FullDim{}(FullDim{}(SubView{1,cstart1,cend1}(DropDim{2,coffset2}(x)))));
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

 /**
  * operations like (x + y)[1:3,3:5]
  */
 // [[Rcpp::export]]
 Eigen::Tensor<double, 2> TestSubsettingOps(
     Eigen::Tensor<double, 2> x,
     Eigen::Tensor<double, 2> y,
     Eigen::Index cxmin,
     Eigen::Index cxmax,
     Eigen::Index cymin,
     Eigen::Index cymax
 ) {
     return SubView(0, cxmin, cxmax).op(SubView(1, cymin, cymax).op(x + y));
 }

 /**
  * operations like x[1:3,3:6] + y
  */
 // [[Rcpp::export]]
 Eigen::Tensor<double, 2> TestSubsettingThenOps(
         Eigen::Tensor<double, 2> x,
         Eigen::Tensor<double, 2> y,
         Eigen::Index cxmin,
         Eigen::Index cxmax,
         Eigen::Index cymin,
         Eigen::Index cymax
         ) {
    return SubView(0, cxmin, cxmax).op(SubView(1, cymin, cymax).op(x)) + y;
 }


 Eigen::Tensor<double, 2> maptensor(Eigen::TensorMap<Eigen::Tensor<double, 2> > x) {
     return x;
 }

 /**
 * operations like x[1:3,3:6] + y; alternate implementation
 */
 // [[Rcpp::export]]
 Eigen::Tensor<double, 2> TestAltSubsetting(
         Eigen::Tensor<double, 2> x,
         Eigen::Tensor<double, 2> y,
         Eigen::Index cxmin,
         Eigen::Index cxmax,
         Eigen::Index cymin,
         Eigen::Index cymax
         ) {
     return SubViewN<2>({{0, cxmin, cxmax}, {1, cymin, cymax}})(x) + y;
 }
