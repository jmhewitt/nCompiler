#include <RcppEigen.h>
#include <unsupported/Eigen/CXX11/Tensor>
#include <tuple>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(RcppEigen)]]

/**
 *  vector block to select elements based on logical tests
 *  
 *  the data for the logical tests only need to be stored in a container that 
 *  supports the subsetting operator[], and has elements that can be 
 *  contextually converted to bool
 */
template<typename Index, typename Scalar, typename LogicalExprVector>
class LogicalSubset {
  
private:
  
  const LogicalExprVector &subsetted_inds;
  Index len;
  
public:
  
  LogicalSubset(const LogicalExprVector &v) : subsetted_inds(v), 
    len(subsetted_inds.size()) { };
  
  // return the index of the ith true element in subsetted_inds
  const Index operator[](Index i) const { 
    Index ntrue = 0;
    Index entry;
    for(entry = 0; entry < len; ++entry) {
      ntrue += subsetted_inds[entry] == true;
      if(ntrue > i) {
        break;
      }
    }
    return entry; 
  }
  
  const Index size() const {
    Index ntrue = 0;
    for(Index entry = 0; entry < len; ++entry) {
      ntrue += subsetted_inds[entry] == true;
    }
    return ntrue;
  }
  
};

// vector block to access consecutive indices, i.e., 3:10
template<typename Index, typename Scalar>
class b__ {
  
private:
  
  const Index &m_start, &m_stop, &m_dimsize;
  const bool m_empty, m_single;
  
public:
  
  b__(const Index &start, const Index &stop, const Index &dimsize) : 
    m_start(start), m_stop(stop), m_dimsize(dimsize), m_empty(false),
    m_single(false) { }
  
  b__(const Index &single, const Index &dimsize) : m_start(single), 
    m_stop(single), m_dimsize(dimsize), m_empty(false), m_single(true) { }
  
  b__(const Index &dimsize) : m_start(dimsize), m_stop(dimsize), 
    m_dimsize(dimsize), m_empty(true), m_single(false) { }
  
  const Index operator[](Index i) const { return m_start + i; }
  
  const Index size() const {
    if(m_single) { 
      return 1;
    } else if(m_empty) {
      return m_dimsize;
    } else {
      return m_stop - m_start + 1;
    }
  }
  
};


// heterogenous collection of index vector types
template<typename... BlockTypes>
struct IndexBlocks {
  
  // internal storage type for the index vectors that define the blocks
  typedef std::tuple<const BlockTypes&...> Container;
  
  // tensor dimension
  typedef typename std::tuple_size<Container>::value_type size_t;
  const static size_t nInd = std::tuple_size<Container>::value;
  
  // index vectors
  Container blocks;
  
  IndexBlocks(const BlockTypes&... b) : blocks(b...) { };
  
  /**
   *  retrieve the Ith index value of the Jth block
   *  
   *  For example, if the Jth block spans indices 3--5 of the subsetted object,
   *  then XXX.block<J>(I) will return 3, 4, or 5 if I=0, 1, or 2, respectively.
   *  
   *  Since the block types are templated, the type of each block must have the
   *  subsetting operator[] implemented, and potentially overloaded.  For
   *  example, for a logical block, the third element of the logical block will
   *  be the index of the third TRUE entry in the blocking vector.
   */
  template<size_t J, typename Index>
  const Index block(const Index & I) const {
    return std::get<J>(blocks)[I]; 
  }
  
  // recursive implementation to retrieve index values for all blocks
  template<typename Indices, size_t dim = 0>
  struct IndexMapper {
    static Indices run(const Indices & inds, 
                       const IndexBlocks & blocks, 
                       Indices & vals) { 
      vals[dim] = blocks.block<dim>(inds[dim]);
      return IndexMapper<Indices, dim + 1>::run(inds, blocks, vals);
    };
  };
  
  // final recursion via partial specialization
  template<typename Indices>
  struct IndexMapper<Indices, nInd> {
    static Indices run(const Indices & inds,
                       const IndexBlocks & blocks,
                       Indices & vals) { 
      return vals;
    };
  };
  
  // retrieve index values for all blocks
  template<typename Indices>
  Indices index(const Indices & indices) { 
    Indices ans;
    return IndexMapper<Indices>::run(indices, *this, ans);
  }
  
};

// TODO: consider deprecating this constructor wrapper
template<typename... BlockTypes>
IndexBlocks<BlockTypes...> MakeIndexBlocks(BlockTypes... b) {
  return IndexBlocks<BlockTypes...>(b...);
}

/*
 * demonstrate extracting data from a subsetted object when the dimensions are 
 * specified.  this is an efficient strategy b/c we don't need to recurse 
 * through the tuple; instead, we can pull out exactly the translation pieces
 * we require
 */
// [[Rcpp::export]]
double Operator_i0_i1(
    NumericMatrix x,         // object to subset
    IntegerVector inds1,     // subset in first dimension
    IntegerVector inds2,     // subset in second dimension
    int i0,                  // 1st coord of element to extract from subset
    int i1                   // 1st coord of element to extract from subset
) {
  IndexBlocks<IntegerVector, IntegerVector> blocks(inds1, inds2);
  return x(blocks.block<0>(i0), blocks.block<1>(i1));
}

/*
 * demonstrate extracting data from a subsetted object when given a vector.  
 * this is a generic strategy for when we need to recurse through the tuple
 */
// [[Rcpp::export]]
double Operator_indVec(
    NumericMatrix x,         // object to subset
    IntegerVector inds1,     // subset in first dimension
    IntegerVector inds2,     // subset in second dimension
    IntegerVector indsOut    // vector of coords for element to extract
) {
  IndexBlocks<IntegerVector, IntegerVector> blocks(inds1, inds2);
  IntegerVector inds = blocks.index(indsOut);
  return x(inds[0], inds[1]);
}

/*
 * demonstrate extracting data from a subsetted object when given a mixture of 
 * subsetting types
 */
// [[Rcpp::export]]
double Operator_Mixtures(
    std::vector<double> data,   // flat-object to subset
    std::vector<long> dim,      // dimensions for flat object
    IntegerVector inds1,        // subset in first dimension
    IntegerVector inds2,        // subset in second dimension
    IntegerVector inds3range, 
    IntegerVector indsOut       // vector of coords for element to extract
) {
  
  Eigen::TensorMap<Eigen::Tensor<double, 3>> m(
      data.data(), dim[0], dim[1], dim[2]
  );
  
  typedef b__<long, double> RangeType;
  RangeType inds3(inds3range[0], inds3range[1], dim[2]);
  
  IndexBlocks<IntegerVector, IntegerVector, RangeType> blocks(
      inds1, inds2, inds3
  );
  
  IntegerVector inds = blocks.index(indsOut);
  return m({inds[0], inds[1], inds[2]});
}

/*
 * demonstrate extracting data from a subsetted object when given a mixture of 
 * subsetting types, including logical
 */
// [[Rcpp::export]]
double Operator_LogicalMixtures(
    std::vector<double> data,         // object to subset
    std::vector<long> dim,
    IntegerVector inds1,              // subset in first dimension
    LogicalVector inds2Logical,   // subset in second dimension
    IntegerVector inds3range,
    IntegerVector indsOut             // vector of coords for element to extract
) {
  
  Eigen::TensorMap<Eigen::Tensor<double, 3>> m(
      data.data(), dim[0], dim[1], dim[2]
  );
  
  typedef LogicalSubset<long, double, LogicalVector> LogicalType;
  LogicalType inds2Subset(inds2Logical);
  
  typedef b__<long, double> RangeType;
  RangeType inds3(inds3range[0], inds3range[1], dim[2]);
  
  IndexBlocks<IntegerVector, LogicalType, RangeType> blocks(
      inds1, inds2Subset, inds3
  );
  
  IntegerVector inds = blocks.index(indsOut);
  return m({inds[0], inds[1], inds[2]});
}
