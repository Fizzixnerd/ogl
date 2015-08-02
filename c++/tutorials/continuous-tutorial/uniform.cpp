#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "uniform.hpp"
#include "vector.hpp"

namespace ogl {

  template<>
  void Uniform<Vector<float, 1> >::update() {
    glUniform1f(m_handle, m_value);
  }

  template<>
  void Uniform<Matrix<float, 4, 4> >::update() {
    glUniformMatrix4fv(m_handle, 1, GL_TRUE, &m_value[0][0]);
  }
}
