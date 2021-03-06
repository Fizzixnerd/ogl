#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <exception>
#include <stdexcept>
#include <string>
#include <vector>
#include <memory>

#include "make_unique.tpp"
#include "uniform.hpp"

namespace ogl {

  template<GLenum ShaderType> class Shader;
  class ShaderProgram;

  /** 
      An OpenGL class representing a Shader Program.  Usses RAII to
      manage its resources.
   */
  class ShaderProgram {
    template<GLenum ShaderType>
    friend class Shader;
    template<class T, class... Args> friend
    std::unique_ptr<T> mem::make_unique(Args... args);
    template<GLenum... ShaderTypes> friend
    std::unique_ptr<ShaderProgram> make_shaderprogram(Shader<ShaderTypes>&... shaders);
    template<GLenum ShaderType, GLenum... ShaderTypes> friend
    void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp,
				  Shader<ShaderType>& shader,
				  Shader<ShaderTypes>&... shaders);
    friend
    void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp);

    
    GLuint m_handle;
    bool m_good;
    std::string m_infolog;
    
    template<GLenum ShaderType>
    void attach(Shader<ShaderType>& s);
    void link() noexcept;

  public:
    ShaderProgram();
    ShaderProgram(ShaderProgram& sp) = delete;
    ~ShaderProgram() noexcept;
    void use() const noexcept;
    operator bool() const noexcept;
    const std::string& infolog() const noexcept;
    template<class T>
    Uniform<T> get_uniform(const std::string& name);
    GLuint handle();
  };
  
  class ShaderProgramCreationException : public std::runtime_error {
  public:
    ShaderProgramCreationException();
    virtual const char * what() const noexcept override;
  };
  
  // FIXME: Add a move constructor
  //
  // FIXME: Fix the RAII so that the ShaderProgram keeps a list of all
  // attached shaders and detaches them once it compiles.  This would
  // be better.
  /**
     An OpenGL class representing a Shader of a particular type.
     Valid types are GL_VERTEX_SHADER, GL_FRAMENT_SHADER, or
     GL_GEOMETRY_SHADER.  Uses RAII to manage its resources.
   */
  template<GLenum ShaderType>
  class Shader {
    friend class ShaderProgram;
    
    GLuint m_handle;
    const std::string m_filename;
    std::string m_infolog;
    std::vector<ShaderProgram*> m_attached_shaderprograms;
    bool m_good;
    
    const std::string read();
    void compile(const std::string& shader_text);

  public:
    Shader(const std::string& filename_);
    ~Shader() noexcept;
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
#include "shaderprogram.tpp"
