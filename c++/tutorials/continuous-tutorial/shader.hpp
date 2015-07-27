#pragma once

#include <GL/glew.h>
#include <GL/freeglut.h>
#include <exception>
#include <stdexcept>
#include <string>

namespace ogl {

  template<GLenum ShaderType>
  class Shader {
    GLuint m_handle;
    const std::string m_filename;
    std::string m_infolog;
    bool m_good;
    
    const std::string read();
    void compile(const std::string& shader_text);

  public:
    Shader(const std::string& filename_);
    ~Shader() noexcept = default;
    operator bool() const noexcept;
    const std::string& infolog() const noexcept;
    GLuint handle() const noexcept;
    const std::string& filename() const noexcept;
  };

  class ShaderCreationException : public std::runtime_error {
  public:
    ShaderCreationException();
    virtual const char * what() const noexcept override;
  };

}

#include "shader.tpp"
