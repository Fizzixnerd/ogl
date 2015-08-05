#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cstdlib>

#include <array>

#include "vector.hpp"

namespace ogl {

  template<GLenum BufferType, GLenum UsageType>
  class Buffer {
    /**
       The OpenGL handle to the actual Buffer Object.
     */
    GLuint m_handle;

  public:
    
    Buffer();
    Buffer(GLuint handle);
    Buffer(Buffer& b) = delete;
    Buffer(Buffer&& b);
    ~Buffer();

    /**
       Buffer data pointed to by data of size size into the buffer.
    */
    void buffer(std::size_t size, const void* data);

    /**
       Buffer the array.
    */
    template<class T, std::size_t N>
    void buffer(std::array<T, N>& data);

    /**
       Buffer the Vector.
    */
    template<class T, std::size_t N>
    void buffer(Vector<T, N>& data);
    GLuint handle();

    /**
       Bind the buffer to the current context.
     */
    void bind();
  };
}

#include "buffer.tpp"
