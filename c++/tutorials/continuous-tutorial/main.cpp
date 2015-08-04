#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cmath>
#include <cstdio>
#include <cassert>

#include <array>
#include <iostream>

#include "vector.hpp"
#include "matrix.hpp"
#include "shader.hpp"
#include "context.hpp"
#include "buffer.hpp"
#include "uniform.hpp"
#include "attribute.hpp"

/** This function renders the scene properly and is overall a good
    thing to be having yes yes. */

template<GLenum ShaderType>
void check_shader(ogl::Shader<ShaderType>& s) {
  if (s) {
    std::cerr << "good!" << std::endl;
  } else {
    std::cerr << "bad!!" << std::endl;
    std::cerr << s.infolog() << std::endl;
  }
}

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

  auto context = Context(1024, 768, "Tutorial 05", nullptr, nullptr);
  if (!context) {
    return 3;
  }
  context.use();
  
  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
    return 2;
  }

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

  auto vertex_buffer = Buffer<GL_ARRAY_BUFFER, GL_STATIC_DRAW>();
  // float vertices[3][3] = {{0.0f}};
  // vertices[0][0] = -1.0f;
  // vertices[0][1] = -1.0f;
  // vertices[1][0] = 1.0f;
  // vertices[1][1] = -1.0f;
  // vertices[2][1] = 1.0f;
  

  // for (auto i = 0; i < 3; ++i) {
  //   for (auto j = 0; j < 3; ++j) {
  //     std::cout << vertices[i][j] << " ";
  //   }
  //   std::cout << std::endl;
  // }
  // std::cout << std::endl;

  Vector3f vertices[3];
  
  vertices[0] = make_vector(-1.0f, -1.0f, 0.0f);
  vertices[1] = make_vector(1.0f, -1.0f, 0.0f);
  vertices[2] = make_vector(0.0f, 1.0f, 0.0f);

  vertex_buffer.buffer(sizeof(vertices), vertices);

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

    new_gWorld[0][3] = std::sin(gScale);

    auto gworld_mat = Matrix<float, 4, 4>(new_gWorld);
    gWorld = gworld_mat;

    vertex_buffer.bind();
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glDisableVertexAttribArray(0);

    context.swap_buffers();
    glfwPollEvents();
  }

  glfwTerminate();
  return 0;
}
