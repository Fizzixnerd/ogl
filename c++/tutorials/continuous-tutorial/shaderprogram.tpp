#include "shader.hpp"
#include "make_unique.tpp"

namespace ogl {
  
  template<GLenum ShaderType>
  void ShaderProgram::attach(Shader<ShaderType>& s) {
    glAttachShader(m_handle, s.m_handle);
    s.m_attached_shaderprograms.push_back(this);
  }

  template<GLenum... ShaderTypes>
  std::unique_ptr<ShaderProgram> make_shaderprogram(Shader<ShaderTypes>&... shaders) {
    auto sp = mem::make_unique<ShaderProgram>();
    initialize_shaderprogram(sp, shaders...);
    if (sp) {
      sp->link();
    } else {
      return nullptr;
    }
    return sp;
 }

  template<GLenum ShaderType, GLenum... ShaderTypes>
  void initialize_shaderprogram(std::unique_ptr<ShaderProgram>& sp,
				Shader<ShaderType>& shader,
				Shader<ShaderTypes>&... shaders) {
    sp->attach(shader);
    initialize_shaderprogram(sp, shaders...);
  }
}

