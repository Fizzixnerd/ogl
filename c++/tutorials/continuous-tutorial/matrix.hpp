#pragma once

#include <cstdlib>
#include <array>
#include <iostream>

#include "vector.hpp"

namespace ogl {

  template<class T, std::size_t N, std::size_t M>
  class Matrix {
    std::array<std::array<T, M>, N> m_elements;
  public:
    Matrix();
    explicit Matrix(std::array<std::array<T, M>, N>& elements);
    explicit Matrix(std::array<T, M>& column);
    Matrix(const Matrix<T, N, M>& m);
    Matrix(Matrix<T, N, M>&& m);
    ~Matrix() noexcept = default;
    Matrix<T, N, M>& operator =(const Matrix<T, N, M>& m);
    Matrix<T, N, M>& operator =(Matrix<T, N, M>&& m);
    std::array<T, M>& operator [](std::size_t i);
  };

  template<class T>
  class Matrix<T, 1, 1> {
    std::array<std::array<T, 1>, 1> m_elements;
  public:
    explicit Matrix(std::array<std::array<T, 1>, 1>& elements);
    explicit Matrix(std::array<T, 1>& column);
    Matrix(const Matrix<T, 1, 1>& m);
    Matrix(Matrix<T, 1, 1>&& m);
    ~Matrix() noexcept = default;
    Matrix<T, 1, 1>& operator =(const Matrix<T, 1, 1>& m);
    Matrix<T, 1, 1>& operator =(Matrix<T, 1, 1>&& m);
    std::array<T, 1>& operator [](std::size_t i);

    // implicit conversions for single-element matrices making them
    // equivalent to scalars.
    Matrix(const T& t);
    operator T();
  };

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator +(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator -(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator -(const Matrix<T, N, M>& v);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator *(const Matrix<T, N, M>& v, T scalar);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator *(T scalar, const Matrix<T, N, M>& v);
  
  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator /(const Matrix<T, N, M>& v, T scalar);

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M> operator *(const Matrix<T, N, L>& lhs, const Matrix<T, L, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  bool operator ==(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  bool operator !=(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator +=(Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator -=(Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator *=(Matrix<T, N, M>& lhs, T rhs);

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator /=(Matrix<T, N, M>& lhs, T rhs);

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M> multiply_add(const Matrix<T, N, L>& lhs,
			       const Matrix<T, L, M>& rhs,
			       const Matrix<T, N, M>& adder);

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M>& multiply_add_assign(const Matrix<T, N, L>& lhs,
				       Matrix<T, L, M>& rhs,
				       const Matrix<T, N, M>& adder);

  template<class T, std::size_t N, std::size_t M>
  std::ostream& operator <<(std::ostream& os, const Matrix<T, N, M>& m);
}

#include "matrix.tpp"
#include "matrix1.tpp"
