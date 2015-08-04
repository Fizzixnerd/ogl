#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <string>

namespace ogl {

  /** A Uniform<T> is a reference to a uniform variable in the OpenGL
      context, of the corresponding type T.  Updating the uniform in
      the OpenGL context can be done by simply assigning to the
      uniform a value of type T.
   */
  template<class T>
  class Uniform {
    T m_value;
    GLint m_handle;

  public:

    Uniform(GLint handle);
    ~Uniform() noexcept = default;
    operator bool() const;
    Uniform<T>& operator =(const T& value);
    Uniform<T>& operator +=(const T& value);
    Uniform<T>& operator *=(const T& value);
    GLint handle();
    const T& view() const;
    T& value();
    auto operator [](std::size_t i) -> decltype(m_value[i]);
    void update();
  };
}

#include "uniform.tpp"
