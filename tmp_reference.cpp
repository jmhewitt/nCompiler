#include <RcppEigen.h>
#include <nCompiler/nCompiler_Eigen.h>


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

/**
 * demonstrate using ::chip as the LHS, to write into that view
 * @param x tensor to subset and write
 * @param y data to write into x
 * @param cdim 0-indexed dimension of x into which to write
 * @param coffset 0-indexed row/col of x into which to write
 * @return
 */
// [[Rcpp::export]]
Eigen::Tensor<double, 2> chipAssign(
    Eigen::Tensor<double, 2> & x,
    Eigen::Tensor<double, 1> & y,
    std::size_t cdim,
    std::size_t coffset
) {
    x.chip(coffset,cdim) = y;
    // below will not compile since TensorChipOp's don't have size member fn's.
    //Rcpp::Rcout << x.chip(coffset,cdim).size() << "\n";
    return x;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> mapOfMap(
  Eigen::Tensor<double, 2> & x,
  Eigen::Tensor<double, 2> & y
) {
  Eigen::TensorMap<Eigen::Tensor<double, 2> > xm(x);
  // i think this might work b/c an implicit copy constructor is being called 
  Eigen::TensorMap<Eigen::Tensor<double, 2> > xm2(xm);
  xm2 += y;
  return xm2;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  nCompilerMapOfMap (
    Eigen::Tensor<double, 2> x,
    int xmin, int xmax, int ymin, int ymax
)  {
    //Eigen::Tensor<int, 2> s1;
    //Eigen::Tensor<int, 2> s2;

    // // will not compile since there is no default constructor
    //Eigen::StridedTensorMap<Eigen::Tensor<int, 2> > s1;
    //Eigen::StridedTensorMap<Eigen::Tensor<int, 2> > s2;

    typedef Eigen::StridedTensorMap<Eigen::Tensor<double, 2> > STM;


    STM s1 = Eigen::MakeStridedTensorMap<2>::make(
        x, Eigen::MakeIndexBlocks(b__(xmin-1, xmax-1), b__())
    );

    // strided tensor map of a strided tensor map is a valid object
    STM s2 = Eigen::MakeStridedTensorMap<2>::make(
        s1, Eigen::MakeIndexBlocks(b__(), b__(ymin-1, ymax-1))
    );

    // s2 = (s2).cast<double>()+1.0;
    return(s2);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> chipOfChip (
    Eigen::Tensor<double, 3> x,
    std::size_t cdim1, std::size_t coffset1,
    std::size_t cdim2, std::size_t coffset2
)  {
    auto y = x.chip(coffset1, cdim1);
    auto z = y.chip(coffset2, cdim2);
    return(z);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> tensorView (
    Eigen::Tensor<double, 2> x,
    Eigen::Index cminx, Eigen::Index cmaxx,
    Eigen::Index cminy, Eigen::Index cmaxy
) {
    // TODO: implement subsetting as TensorSlicingOp's, which
    // we follow with TensorChippingOp's as needed to drop dimensions.
    // this strategy would reuse existing Eigen operations and also mimic
    // R's dimension dropping behavior, where it feels like it is done after
    // the initial sub-view.
    Eigen::array<Eigen::Index, 2> offsets = {cminx, cminy};
    Eigen::array<Eigen::Index, 2> extents = {
        cmaxx - cminx + 1, cmaxy - cminy + 1
    };
    bool chipx = cminx == cmaxx;
    //auto slice = x.slice(offsets, extents).chip(0,chipx ? 0 : 1);

    // mimics multiple subsetting operations with ,drop=FALSE
    // dropping will be a bit more difficult
    auto s1 = x.slice(
        Eigen::array<Eigen::Index, 2> {cminx, 0},
        Eigen::array<Eigen::Index, 2> {cmaxx - cminx + 1, x.dimensions()[1]}
    );
    Eigen::TensorRef<Eigen::Tensor<double, 2> > s1Info(s1);
    auto slice = s1.slice(
        Eigen::array<Eigen::Index, 2> {0, cminy},
        Eigen::array<Eigen::Index, 2> {s1Info.dimensions()[0], cmaxy - cminy + 1}
    );
    return slice;
}