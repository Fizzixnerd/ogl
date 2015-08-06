#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cstdlib>

#include <array>

#include "vector.hpp"

namespace ogl {

  /**
     An OpenGL Buffer object.  BufferType should be one of the valid
     GL_*_BUFFER types, and UsageType should be a GL_*_DRAW type.
   */
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
    void buffer(const std::array<T, N>& data);

    /**
       Buffer the Vector.
    */
    template<class T, std::size_t N>
    void buffer(const Vector<T, N>& data);

    /**
       Return the OpenGL handle for the buffer.
     */
    GLuint handle();

    /**
       Bind the buffer to the current context.
     */
    void bind();
  };
}

#include "buffer.tpp"
