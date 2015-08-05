#pragma once

#include <memory>
#include <array>
#include <cstdlib>

namespace ogl {

  template<class T, std::size_t N, std::size_t M>
  class Matrix;

  /**
     A Vector class derived from the Matrix class.  Use make_vector
     for easier construction.
   */
  template<class T, std::size_t M>
  using Vector = Matrix<T, 1, M>;

  using Vector4f = Vector<float, 4>;
  using Vector3f = Vector<float, 3>;
  using Vector2f = Vector<float, 2>;
  using Vector1f = Vector<float, 1>;

  using Vector4i = Vector<int, 4>;
  using Vector3i = Vector<int, 3>;
  using Vector2i = Vector<int, 2>;
  using Vector1i = Vector<int, 1>;

  template<class T>
  std::array<T, 1> make_singleton_array(T t);

  /**
     A variadic function template to construct Vectors of arbitrary
     size and type.  A (probably very unreadable) error is thrown if
     the types given are not homogenous.
   */
  template<class T, class... Ts>
  Vector<T, sizeof...(Ts) + 1> make_vector(T v1, Ts... args);
}

#include "vector.tpp"
#include "matrix.hpp"
