#include <memory>
#include <iostream>

#include "shader.hpp"
#include "make_unique.tpp"

namespace ogl {

  ShaderProgram::ShaderProgram() : m_good(true) {
    m_handle = glCreateProgram();
    if (m_handle == 0) {
      m_good = false;
      m_infolog = "Failed to create ShaderProgram.";
      throw ShaderProgramCreationException();
    }
  }

  ShaderProgram::~ShaderProgram() noexcept {
    glDeleteProgram(m_handle);
    std::cerr << "Killed the program." << std::endl;
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

  void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp) {
    return;
  }

  ShaderProgramCreationException::ShaderProgramCreationException() : std::runtime_error("") {
  }
  
  const char * ShaderProgramCreationException::what() const noexcept {
    return "Failed to create ShaderProgram.";
  }

}
