#pragma once

#include <GL/glew.h>
#include <GL/freeglut.h>
#include <vector>
#include <exception>
#include <stdexcept>

#include "shader.hpp"

namespace ogl {
  
  class ShaderProgram {
    GLuint m_handle;
    bool m_good;
    std::string m_infolog;
    std::vector<Shader<GL_VERTEX_SHADER>> m_vertex_shaders;
    std::vector<Shader<GL_FRAGMENT_SHADER>> m_fragment_shaders;
    std::vector<Shader<GL_GEOMETRY_SHADER>> m_geometry_shaders;

  public:
    ShaderProgram();
    ~ShaderProgram() noexcept = default;
    operator bool() const noexcept;
    template<GLenum ShaderType> void attach(Shader<ShaderType> s);
    void link() noexcept;
    const std::string& infolog() const noexcept;
    void use() const noexcept;
  };

  class ShaderProgramCreationException : public std::runtime_error {
  public:
    ShaderProgramCreationException();
    virtual const char * what() const noexcept override;
  };
}

