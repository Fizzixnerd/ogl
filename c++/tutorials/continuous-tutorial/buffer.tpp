#pragma once

#include "buffer.hpp"

namespace ogl {

  template<GLenum BufferType, GLenum UsageType>
  Buffer<BufferType, UsageType>::Buffer() {
    glGenBuffers(1, &m_handle);
  }

  template<GLenum BufferType, GLenum UsageType>
  Buffer<BufferType, UsageType>::Buffer(GLuint handle) : m_handle(handle) {
  }
  
  template<GLenum BufferType, GLenum UsageType>
  Buffer<BufferType, UsageType>::~Buffer() {
    glDeleteBuffers(1, &m_handle);
  }
  
  template<GLenum BufferType, GLenum UsageType>
  void Buffer<BufferType, UsageType>::buffer(std::size_t size, const void *data) {
    bind();
    glBufferData(BufferType, size, data, UsageType);
  }
  
  template<GLenum BufferType, GLenum UsageType>
  GLuint Buffer<BufferType, UsageType>::handle() {
    return m_handle;
  }

  template<GLenum BufferType, GLenum UsageType>
  void Buffer<BufferType, UsageType>::bind() {
    glBindBuffer(BufferType, m_handle);
  }
}
