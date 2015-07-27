#include <exception>
#include <string>
#include <fstream>
#include <sstream>

#include "shader.hpp"
#include "shaderprogram.hpp"

namespace ogl {

  template<GLenum ShaderType>
  const std::string Shader<ShaderType>::read() {
    std::ifstream ifs(m_filename);
    std::stringstream data;
    if (not ifs) {
      m_good = false;
      return NULL;
    }
    data << ifs.rdbuf();
    //    std::cerr << data.str() << std::endl;
    return data.str();
  }

  template<GLenum ShaderType>
  void Shader<ShaderType>::compile(const std::string& shader_text) {
    GLint len = shader_text.length();
    const GLchar* data = shader_text.c_str();
    glShaderSource(m_handle, 1, &data, &len);
    glCompileShader(m_handle);
    GLint success;
    glGetShaderiv(m_handle, GL_COMPILE_STATUS, &success);
    if (not success) {
      m_good = false;
    }
  }

  template<GLenum ShaderType>
  Shader<ShaderType>::Shader(const std::string& filename_) : m_filename(filename_),
							     m_good(true) {
    m_handle = glCreateShader(ShaderType);
    if (m_handle == 0) {
      m_good = false;
      m_infolog = "Failed to create shader.";
      throw ShaderCreationException();
      return;
    }
    
    auto shader_text = read();
    if (not m_good) {
      m_infolog = "Failed to read shader from file.";
      return;
    }
    
    compile(shader_text);
    if (not m_good) {
      GLchar infolog[1024];
      glGetShaderInfoLog(m_handle, 1024, NULL, infolog);
      m_infolog = infolog;
      return;
    }
  }

  template<GLenum ShaderType>
  Shader<ShaderType>::~Shader() noexcept {
    for (auto& sp : m_attached_shaderprograms) {
      glDetachShader(sp->m_handle, m_handle);
    }
    glDeleteShader(m_handle);
  }

  template<GLenum ShaderType>
  Shader<ShaderType>::operator bool() const noexcept {
    return m_good;
  }
  
  template<GLenum ShaderType>
  const std::string& Shader<ShaderType>::infolog() const noexcept {
    return m_infolog;
  }

  template<GLenum ShaderType>
  GLuint Shader<ShaderType>::handle() const noexcept {
    return m_handle;
  }

  template<GLenum ShaderType>
  const std::string& Shader<ShaderType>::filename() const noexcept {
    return m_filename;
  }

}
  
  
