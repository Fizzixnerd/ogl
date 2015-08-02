#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cstdlib>

namespace ogl {

  template<GLenum BufferType, GLenum UsageType>
  class Buffer {
    GLuint m_handle;

  public:
    
    Buffer();
    Buffer(GLuint handle);
    ~Buffer();
    GLuint handle();
    void buffer(std::size_t size, const void* data);
    void bind();
  };
}

#include "buffer.tpp"
