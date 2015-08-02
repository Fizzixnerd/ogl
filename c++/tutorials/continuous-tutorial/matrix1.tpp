#pragma once

#include <array>
#include <cstdlib>

#include "matrix.hpp"

namespace ogl {

  template<class T>
  Matrix<T, 1, 1>::Matrix(std::array<std::array<T, 1>, 1>& elements) :
    m_elements(elements) {
  }

  template<class T>
  Matrix<T, 1, 1>::Matrix(std::array<T, 1>& column) {
    m_elements[0][0] = column[0];
  }

  template<class T>
  Matrix<T, 1, 1>::Matrix(const Matrix<T, 1, 1>& m) :
    m_elements(m.m_elements) {
  }

  template<class T>
  Matrix<T, 1, 1>::Matrix(Matrix<T, 1, 1>&& m) {
    swap(m_elements, m.m_elements);
  }

  template<class T>
  Matrix<T, 1, 1>& Matrix<T, 1, 1>::operator =(const Matrix<T, 1, 1>& m) {
    m_elements[0][0] = m.m_elements[0][0];
    return *this;
  }

  template<class T>
  Matrix<T, 1, 1>& Matrix<T, 1, 1>::operator =(Matrix<T, 1, 1>&& m) {
    swap(m_elements, m.m_elements);
    return *this;
  }

  template<class T>
  std::array<T, 1>& Matrix<T, 1, 1>::operator [](std::size_t i) {
    return m_elements[i];
  }

  template<class T>
  Matrix<T, 1, 1>::Matrix(const T& t) {
    m_elements[0][0] = t;
  }

  template<class T>
  Matrix<T, 1, 1>::operator T() {
    return m_elements[0][0];
  }
}
