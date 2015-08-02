#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "uniform.hpp"

namespace ogl {

  template<class T>
  Uniform<T>::Uniform(GLint handle) : m_handle(handle) {
  }

  template<class T>
  Uniform<T>::operator bool() const {
    return (m_handle != -1);
  }

  template<class T>
  Uniform<T>& Uniform<T>::operator =(const T& value) {
    m_value = value;
    update();
    return *this;
  }

  template<class T>
  Uniform<T>& Uniform<T>::operator+=(const T &value) {
    return (*this = m_value + value);
  }

  template<class T>
  GLint Uniform<T>::handle() {
    return m_handle;
  }

  template<class T>
  const T& Uniform<T>::view() const {
    return m_value;
  }

  template<class T>
  T& Uniform<T>::value() {
    return m_value;
  }
}
