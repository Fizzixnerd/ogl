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

  // FIXME: I think this is broken; shouldnt I initialize b to like,
  // 0xFFFFFFFF?  But then what about move-to-self?
  template<GLenum BufferType, GLenum UsageType>
  Buffer<BufferType, UsageType>::Buffer(Buffer&& b) {
    std::swap(m_handle, b.m_handle);
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
  template<class T, std::size_t N>
  void Buffer<BufferType, UsageType>::buffer(const std::array<T, N>& data) {
    bind();
    glBufferData(BufferType, N * sizeof(T), data.data(), UsageType);
  }

  template<GLenum BufferType, GLenum UsageType>
  template<class T, std::size_t N>
  void Buffer<BufferType, UsageType>::buffer(const Vector<T, N>& data) {
    bind();
    glBufferData(BufferType, N * sizeof(T), data.data(), UsageType);
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
