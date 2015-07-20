#pragma once

#include <cstdlib>
#include <array>

namespace ogl {

  template<class T, std::size_t n>
  class Vector {
    std::array<T, n> elements;

  public:
    Vector();
    explicit Vector(std::array<T, n> elements_);
    explicit Vector(Vector<T, n>& v);
    explicit Vector(Vector<T, n>&& v);
    ~Vector() noexcept = default;
    Vector<T, n>& operator =(Vector<T, n> v);
    T& operator [](std::size_t i);
  };

  template<class T, std::size_t n>
  Vector<T, n> operator +(const Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  Vector<T, n> operator -(const Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  Vector<T, n> operator -(const Vector<T, n>& v);

  template<class T, std::size_t n>
  Vector<T, n> operator *(const Vector<T, n>& v, T scalar);

  template<class T, std::size_t n>
  Vector<T, n> operator *(T scalar, const Vector<T, n>& v);

  template<class T, std::size_t n>
  Vector<T, n> operator /(const Vector<T, n>& v, T scalar);

  template<class T, std::size_t n>
  bool operator ==(const Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  bool operator !=(const Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  Vector<T, n>& operator +=(Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  Vector<T, n>& operator -=(Vector<T, n>& lhs, const Vector<T, n>& rhs);

  template<class T, std::size_t n>
  Vector<T, n>& operator *=(Vector<T, n>& lhs, T rhs);

  template<class T, std::size_t n>
  Vector<T, n>& operator /=(Vector<T, n>& lhs, T rhs);

  template<class T, std::size_t n>
  T dot(const Vector<T, n>& u, const Vector<T, n>& v);

  template<class T, std::size_t n>
  T norm(const Vector<T, n>& v);

  template<class T>
  Vector<T, 3> cross(const Vector<T, 3>& u, const Vector<T, 3>& v);

  template<class T, std::size_t n>
  Vector<T, n> multiply_add(const Vector<T, n>& u, T scalar, const Vector<T, n>& v);

  template<class T, std::size_t n>
  Vector<T, n>& multiply_add_assign(Vector<T, n>& u, T scalar, const Vector<T, n>& v);
  
}

#include "vector.tpp"
