#include "shaderprogram.hpp"

namespace ogl {

  ShaderProgram::ShaderProgram() : m_good(true) {
    m_handle = glCreateProgram();
    if (m_handle == 0) {
      m_good = false;
      m_infolog = "Failed to create ShaderProgram.";
      throw ShaderProgramCreationException();
    }
  }

  template<> void ShaderProgram::attach(Shader<GL_VERTEX_SHADER> s) {
    glAttachShader(m_handle, s.handle());
    m_vertex_shaders.push_back(s);
  }

  template<> void ShaderProgram::attach(Shader<GL_FRAGMENT_SHADER> s) {
    glAttachShader(m_handle, s.handle());
    m_fragment_shaders.push_back(s);
  }

  template<> void ShaderProgram::attach(Shader<GL_GEOMETRY_SHADER> s) {
    glAttachShader(m_handle, s.handle());
    m_geometry_shaders.push_back(s);
  }


  ShaderProgram::operator bool() const noexcept {
    return m_good;
  }

  void ShaderProgram::link() noexcept {
    GLint success = 0;
    GLchar infolog[1024];
    
    glLinkProgram(m_handle);
    glGetProgramiv(m_handle, GL_LINK_STATUS, &success);
    if (success == 0) {
      m_good = false;
      glGetProgramInfoLog(m_handle, sizeof(infolog), NULL, infolog);
      m_infolog = infolog;
      return;
    }
    
    glValidateProgram(m_handle);
    glGetProgramiv(m_handle, GL_VALIDATE_STATUS, &success);
    if (success == 0) {
      m_good = false;
      glGetProgramInfoLog(m_handle, sizeof(infolog), NULL, infolog);
      m_infolog = infolog;
      return;
    }
    // everything was okay!
    m_infolog = "Program linked successfully.";
  }

  const std::string& ShaderProgram::infolog() const noexcept {
    return m_infolog;
  }

  void ShaderProgram::use() const noexcept {
    glUseProgram(m_handle);
  }

  ShaderProgramCreationException::ShaderProgramCreationException() : std::runtime_error("") {
  }
  
  const char * ShaderProgramCreationException::what() const noexcept {
    return "Failed to create ShaderProgram.";
  }

}
