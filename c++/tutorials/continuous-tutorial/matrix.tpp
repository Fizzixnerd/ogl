#pragma once

#include <cstdlib>
#include <array>
#include <iostream>

#include "matrix.hpp"

namespace ogl {

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>::Matrix() :
    m_elements(std::array<std::array<T, M>, N>()) {
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>::Matrix(std::array<std::array<T, M>, N>& elements) :
    m_elements(elements) {
  }


  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>::Matrix(std::array<T, M>& column) {
    for (std::size_t i = 0; i < N; ++i) {
      m_elements[i] = column;
    }
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>::Matrix(const Matrix<T, N, M>& m) :
    m_elements(m.m_elements) {
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>::Matrix(Matrix<T, N, M>&& m) {
    swap(m_elements, m.m_elements);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& Matrix<T, N, M>::operator=(const Matrix<T, N, M> &m) {
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	m_elements[i][j] = m.m_elements[i][j];
      }
    }
    return *this;
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& Matrix<T, N, M>::operator =(Matrix<T, N, M>&& m) {
    swap(m_elements, m.m_elements);
    return *this;
  }

  template<class T, std::size_t N, std::size_t M>
  std::array<T, M>& Matrix<T, N, M>::operator [](std::size_t i) {
    return m_elements[i];
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator +(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = lhs[i][j] + rhs[i][j];
      }
    }
    return Matrix<T, N, M>(new_elements);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator -(const Matrix<T, N, M>& m) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = -m[i][j];
      }
    }
    return Matrix<T, N, M>(new_elements);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator -(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = lhs[i][j] - rhs[i][j];
      }
    }
    return Matrix<T, N, M>(new_elements);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator *(const Matrix<T, N, M>& m, T scalar) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = m[i][j] * scalar;
      }
    }
    return Matrix<T, N, M>(new_elements);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator *(T scalar, const Matrix<T, N, M>& m) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = scalar * m[i][j];
      }
    }
    return Matrix<T, N, M>(new_elements);
  }
  
  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M> operator /(const Matrix<T, N, M>& m, T scalar) {
    std::array<std::array<T, M>, N> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	new_elements[i][j] = m[i][j] / scalar;
      }
    }
    return Matrix<T, N, M>(new_elements);
  }

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M> operator *(const Matrix<T, N, L>& lhs, const Matrix<T, L, M>& rhs) {
    std::array<std::array<T, N>, M> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	T sum = T();
	for (std::size_t k = 0; k < L; ++k) {
	  sum += lhs[i][k] * rhs[k][j];
	}
	new_elements[i][j] = sum;
      }
    }
  }

  template<class T, std::size_t N, std::size_t M>
  bool operator ==(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	if (lhs[i][j] != rhs[i][j]) {
	  return false;
	}
      }
    }
    return true;
  }

  template<class T, std::size_t N, std::size_t M>
  bool operator !=(const Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	if (lhs[i][j] != rhs[i][j]) {
	  return true;
	}
      }
    }
    return false;
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator +=(Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    return (lhs = lhs + rhs);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator -=(Matrix<T, N, M>& lhs, const Matrix<T, N, M>& rhs) {
    return (lhs = lhs - rhs);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator *=(Matrix<T, N, M>& lhs, T rhs) {
    return (lhs = lhs * rhs);
  }

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M>& operator *=(Matrix<T, N, L>& lhs, const Matrix<T, L, M>& rhs) {
    return (lhs = lhs * rhs);
  }

  template<class T, std::size_t N, std::size_t M>
  Matrix<T, N, M>& operator /=(Matrix<T, N, M>& lhs, T rhs) {
    return (lhs = lhs / rhs);
  }

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M> multiply_add(const Matrix<T, N, L>& lhs,
			       const Matrix<T, L, M>& rhs,
			       const Matrix<T, N, M>& adder) {
    std::array<std::array<T, N>, M> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	T sum = T();
	for (std::size_t k = 0; k < L; ++k) {
	  sum += lhs[i][k] * rhs[k][j];
	}
	new_elements[i][j] = sum + adder[i][j];
      }
    }
  }

  template<class T, std::size_t N, std::size_t M, std::size_t L>
  Matrix<T, N, M>& multiply_add_assign(const Matrix<T, N, L>& lhs,
				       Matrix<T, L, M>& rhs,
				       const Matrix<T, N, M>& adder) {
    std::array<std::array<T, N>, M> new_elements;
    for (std::size_t i = 0; i < N; ++i) {
      for (std::size_t j = 0; j < M; ++j) {
	T sum = T();
	for (std::size_t k = 0; k < L; ++k) {
	  sum += lhs[i][k] * rhs[k][j];
	}
	rhs[i][j] = sum + adder[i][j];
      }
    }
  }

  template<class T, std::size_t N, std::size_t M>
  std::ostream& operator <<(std::ostream& os, Matrix<T, N, M>& m) {
    for (std::size_t i = 0; i < N; ++i) {
      os << "[";
      for (std::size_t j = 0; j < M; ++j) {
	os << m[i][j];
	if (j < M) {
	  os << " ";
	}
      }
      os << "]\n";
    }
    return os;
  }
}
