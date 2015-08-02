#pragma once

#include <array>
#include <cstdlib>
#include <cmath>

#include "vector.hpp"

namespace ogl {

  template<class T, class... Ts>
  Vector<T, sizeof...(Ts) + 1> make_vector(T v1, Ts... args) {
    const std::size_t sz = sizeof...(Ts) + 1;
    std::array<T, sz> vals = {v1, args...};
    return Vector<T, sz>{vals};
  }
    

  template<class T, std::size_t n>
  Vector<T, n>::Vector() : elements(std::array<T, n>()) {
  }

  template<class T, std::size_t n>
  Vector<T, n>::Vector(std::array<T, n>& elements_) : elements(elements_) {
  }

  template<class T, std::size_t n>
  Vector<T, n>::Vector(const Vector<T, n>& v) : elements(v.elements) {
  }

  template<class T, std::size_t n>
  Vector<T, n>::Vector(Vector<T, n>&& v) : elements(std::move(v.elements)) {
  }
 
  template<class T, std::size_t n>
  Vector<T, n>& Vector<T, n>::operator =(const Vector<T, n>& v) {
    return Vector<T, n>(v);
  }

  template<class T, std::size_t n>
  Vector<T, n>& Vector<T, n>::operator =(Vector<T, n>&& v) {
    swap(elements, v.elements);
    return *this;
  }

  template<class T, std::size_t n>
  T& Vector<T, n>::operator [](std::size_t i) {
    return elements[i];
  }

  template<class T, std::size_t n>
  Vector<T, n> operator +(const Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = lhs[i] + rhs[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n> operator -(const Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = lhs[i] - rhs[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n> operator -(const Vector<T, n>& v) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = -v[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n> operator *(const Vector<T, n>& v, T scalar) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = scalar * v[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n> operator *(T scalar, const Vector<T, n>& v) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = scalar * v[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n> operator /(const Vector<T, n>& v, T scalar) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = v[i] / scalar;
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  bool operator ==(const Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    for (auto i = 0; i < n; ++i) {
      if (lhs[i] != rhs[i]) {
	return false;
      }
    }
    return true;
  }

  template<class T, std::size_t n>
  bool operator !=(const Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    for (auto i = 0; i < n; ++i) {
      if (lhs[i] != rhs[i]) {
	return true;
      }
    }
    return false;
  }

  template<class T, std::size_t n>
  Vector<T, n>& operator +=(Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    return (lhs = lhs + rhs);
  }

  template<class T, std::size_t n>
  Vector<T, n>& operator -=(Vector<T, n>& lhs, const Vector<T, n>& rhs) {
    return (lhs = lhs - rhs);
  }

  template<class T, std::size_t n>
  Vector<T, n>& operator *=(Vector<T, n>& lhs, T scalar) {
    return (lhs = scalar * lhs);
  }

  template<class T, std::size_t n>
  Vector<T, n>& operator /=(Vector<T, n>& lhs, T scalar) {
    return (lhs = lhs / scalar);
  }

  template<class T, std::size_t n>
  T dot(const Vector<T, n>& u, const Vector<T, n>& v) {
    auto sum = T();
    for (auto i = 0; i < n; ++i) {
      sum += u[i] * v[i];
    }
    return sum;
  }

  template<class T, std::size_t n>
  T norm(const Vector<T, n>& v) {
    return std::sqrt(dot(v, v));
  }

  template<class T>
  Vector<T, 3> cross(const Vector<T, 3>& u, const Vector<T, 3>& v) {
    return Vector<T, 3>({u[2] * v[3] - u[3] - v[2],
			 u[3] * v[1] - u[1] - v[3],
	                 u[1] * v[2] - u[2] - v[1]});
  }

  template<class T, std::size_t n>
  Vector<T, n> multiply_add(const Vector<T, n>& u, T scalar, const Vector<T, n>& v) {
    std::array<T, n> new_elements;
    for (auto i = 0; i < n; ++i) {
      new_elements[i] = scalar * u[i] + v[i];
    }
    return Vector<T, n>(new_elements);
  }

  template<class T, std::size_t n>
  Vector<T, n>& multiply_add_assign(Vector<T, n>& u, T scalar, const Vector<T, n>& v) {
    for (auto i = 0; i < n; ++i) {
      u[i] = scalar * u[i] + v[i];
    }
    return u;
  }

  template<class T, std::size_t n>
  std::ostream& operator <<(std::ostream& os, Vector<T, n>& v) {
    os << "(";
    for (std::size_t i = 0; i < n; ++i) {
      os << v[i];
      if (i == (n - 1)) {
	break;
      }
      os << ", ";
    }
    os << ")";
    return os;
  }
}
