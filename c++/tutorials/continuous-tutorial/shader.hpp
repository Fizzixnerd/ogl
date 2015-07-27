#pragma once

#include <GL/glew.h>
#include <GL/freeglut.h>

#include <exception>
#include <stdexcept>
#include <string>
#include <vector>
#include <memory>

#include "make_unique.tpp"

namespace ogl {

  template<GLenum ShaderType> class Shader;
  class ShaderProgram;

  class ShaderProgram {
    template<GLenum ShaderType>
    friend class Shader;
    
    GLuint m_handle;
    bool m_good;
    std::string m_infolog;
    
    template<GLenum ShaderType>
    void attach(Shader<ShaderType>& s);
    void link() noexcept;

  public:
    ShaderProgram();
    ~ShaderProgram() noexcept;
    void use() const noexcept;
    operator bool() const noexcept;
    const std::string& infolog() const noexcept;
    template<class T> friend
    std::unique_ptr<T> mem::make_unique();
    template<GLenum... ShaderTypes> friend
    std::unique_ptr<ShaderProgram> make_shaderprogram(Shader<ShaderTypes>&... shaders);
    template<GLenum ShaderType, GLenum... ShaderTypes> friend
    void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp,
				  Shader<ShaderType>& shader,
				  Shader<ShaderTypes>&... shaders);
    friend
    void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp);
  };
  
  class ShaderProgramCreationException : public std::runtime_error {
  public:
    ShaderProgramCreationException();
    virtual const char * what() const noexcept override;
  };
  
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
