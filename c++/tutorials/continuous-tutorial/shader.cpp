#include "shader.hpp"

namespace ogl {

  ShaderCreationException::ShaderCreationException() : std::runtime_error("") {
  };

  const char * ShaderCreationException::what() const noexcept {
    return "Creation of shader failed.";
  }

}
