#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <string>

namespace ogl {
  
  template <class T>
  class Attribute {
    T m_value;
    GLint m_handle;
    void update();

  public:
    Attribute(GLint handle);
    ~Attribute() noexcept = default;
    operator bool() const;
    Attribute<T>& operator =(const T& value);
    Attribute<T>& operator +=(const T& value);
    GLint handle();
    const T& value() const;
  };
}
