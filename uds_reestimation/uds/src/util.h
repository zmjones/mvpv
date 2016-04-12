#ifndef UTIL_H
#define UTIL_H

#include "matrix.h"

using namespace scythe;

/* A function that takes a matrix and a boolean matrix of the same
 * size.  It returns a column vector of elements in the data matrix
 * corresponding to true values in the selection matrix.  Note that it
 * traverses each matrix according to its own order (so, if ordering
 * differs across matrices, things can be a bit strange).
 */
template <typename T, matrix_order O1, matrix_style S1,
          matrix_order O2, matrix_style S2>
Matrix<> select (const Matrix<T, O1, S1>& data,
                 const Matrix<bool, O2, S2>& keep)
{
  SCYTHE_CHECK_10(data.size() != keep.size(), scythe_conformation_error,
    "Data and selection matrices have differing sizes");
  unsigned int n = 
    accumulate(keep.begin_f(), keep.end_f(), (unsigned int) 0);
  Matrix<> res(n, 1, false);
  unsigned int cnt = 0;
  for (unsigned int i = 0; i < data.size(); ++i)
    if (keep(i))
      res(cnt++) = data(i);
  
  return res;
}

Matrix<> rvec (const Matrix<>& m)
{
  Matrix<> res(1, m.size(), m.begin());
  return res;
}

Matrix<> vec(const Matrix<>& m)
{
  Matrix<> res(m.size(), 1, m.begin());
  return res;
}

/* A little functor for incrementing */
template <typename T>
struct incrementer {
  T i_; 
  incrementer (T start) : i_ (start) {}
  T operator() () { return i_++; }
};

struct bool_adder {
//: public std::binary_function<unsigned int, bool, unsigned int>{
  unsigned int operator() (unsigned int result, bool cur) const
  { return (result + (unsigned int) cur); }
};

#endif
