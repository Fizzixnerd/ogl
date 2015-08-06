#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cmath>
#include <cstdio>
#include <cassert>

#include <memory>
#include <array>
#include <iostream>

#include "vector.hpp"
#include "matrix.hpp"
#include "shader.hpp"
#include "context.hpp"
#include "buffer.hpp"
#include "uniform.hpp"
#include "attribute.hpp"

/**
   Check the Shader and if it's in a good state print "good!" to
   stderr, else print "bad!!" to stderr and then the infolog.
 */
template<GLenum ShaderType>
void check_shader(ogl::Shader<ShaderType>& s) {
  if (s) {
    std::cerr << "good!" << std::endl;
  } else {
    std::cerr << "bad!!" << std::endl;
    std::cerr << s.infolog() << std::endl;
  }
}

/**
   Check the ShaderProgram and if it's in a good state print "good!"
   to stderr, else print "bad!!" to stderr and then the infolog.
*/
void check_program(std::unique_ptr<ogl::ShaderProgram>& sp) {
  if (sp && *sp) {
    std::cerr << "good!" << std::endl;
  } else {
    std::cerr << "bad!!" << std::endl;
    std::cerr << (sp ? sp->infolog() : "ShaderProgram is nullptr.") << std::endl;
  }
}

int main(int argc, char** argv) {
  using namespace ogl;
  
  auto status = glfwInit();
  if (!status) {
    return 1;
  }

  auto context = Context(1024, 768, "Tutorial 10", nullptr, nullptr);
  if (!context) {
    return 3;
  }
  context.use();
  
  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
    return 2;
  }

  glClearColor(1.0f, 1.0f, 1.0f, 1.0f);

  auto vertex_buffer = Buffer<GL_ARRAY_BUFFER, GL_STATIC_DRAW>();
  auto index_buffer = Buffer<GL_ELEMENT_ARRAY_BUFFER, GL_STATIC_DRAW>();


  std::array<Vector3f, 4> vertices;
  vertices[0] = make_vector(-1.0f, -1.0f, 0.0f);
  vertices[1] = make_vector(0.0f, -1.0f, 1.0f);
  vertices[2] = make_vector(1.0f, -1.0f, 0.0f);
  vertices[3] = make_vector(0.0f, 1.0f, 0.0f);
  

  std::array<Vector3ui, 4> indices;
  indices[0] = make_vector(0u, 3u, 1u);
  indices[1] = make_vector(1u, 3u, 2u);
  indices[2] = make_vector(2u, 3u, 0u);
  indices[3] = make_vector(0u, 1u, 2u);

  vertex_buffer.buffer(vertices);
  index_buffer.buffer(indices);

  auto vertex_shader = Shader<GL_VERTEX_SHADER>("shader.vs");
  auto fragment_shader = Shader<GL_FRAGMENT_SHADER>("shader.fs");
  check_shader(vertex_shader);
  check_shader(fragment_shader);
  auto shader_program = make_shaderprogram(vertex_shader, fragment_shader);
  check_program(shader_program);
  shader_program->use();

  std::printf("GL version %s\n", glGetString(GL_VERSION));

  auto gWorld = shader_program->get_uniform<Matrix<float, 4, 4> >("gWorld");
  if (not gWorld) {
    return 4;
  }
    
  auto gScale = 0.0f;
  int time_scale = 10;
  std::array<std::array<float, 4>, 4> new_gWorld = {{0.0}};
  new_gWorld[0][0] = 1.0f;
  new_gWorld[1][1] = 1.0f;
  new_gWorld[2][2] = 1.0f;
  new_gWorld[3][3] = 1.0f;

  while (not context.should_close()) {
    glClear(GL_COLOR_BUFFER_BIT);

    gScale += 0.001f * time_scale;

    new_gWorld[2][0] = std::sin(gScale);
    new_gWorld[0][0] = std::cos(gScale);
    new_gWorld[0][2] = -std::sin(gScale);
    new_gWorld[2][2] = std::cos(gScale);

    auto gworld_mat = Matrix<float, 4, 4>(new_gWorld);
    gWorld = gworld_mat;



    glEnableVertexAttribArray(0);
    vertex_buffer.bind();
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
    index_buffer.bind();
    glDrawElements(GL_TRIANGLES, 12, GL_UNSIGNED_INT, nullptr);
    glDisableVertexAttribArray(0);

    context.swap_buffers();
    glfwPollEvents();
  }

  glfwTerminate();
  return 0;
}
