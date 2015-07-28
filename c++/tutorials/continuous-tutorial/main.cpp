#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cmath>
#include <cstdio>
#include <iostream>
#include <cassert>

#include "oglmath.hpp"
#include "shader.hpp"
#include "context.hpp"
#include "buffer.hpp"

GLuint VBO;
GLuint gScaleLocation;

/** This function renders the scene properly and is overall a good
    thing to be having yes yes. */
static void RenderSceneCB() {
  glClear(GL_COLOR_BUFFER_BIT);

  static float scale = 0.0f;
  scale += 0.001f;
  glUniform1f(gScaleLocation, std::sin(scale));
  assert(gScaleLocation != 0xFFFFFFFF);

  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  glDisableVertexAttribArray(0);

  glutSwapBuffers();
}

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
  auto context = Context(argc, argv);
  context.set_init_display_mode(GLUT_DOUBLE | GLUT_RGBA);
  context.set_init_window_size(1024, 768);
  context.set_init_window_position(100, 100);
  context.create_window("Tutorial 05");
  context.set_display_func(RenderSceneCB);
  context.set_idle_func(RenderSceneCB);

  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
    return 1;
  }

  std::printf("GL version %s\n", glGetString(GL_VERSION));

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

  auto vertex_buffer = Buffer<GL_ARRAY_BUFFER, GL_STATIC_DRAW>();
  Vector3f vertices[3];
  vertices[0] = make_vector(-1.0f, -1.0f, 0.0f);
  vertices[1] = make_vector(1.0f, -1.0f, 0.0f);
  vertices[2] = make_vector(0.0f, 1.0f, 0.0f);
  vertex_buffer.buffer(sizeof(vertices), vertices);

  VBO = vertex_buffer.handle();

  auto vertex_shader = Shader<GL_VERTEX_SHADER>("shader.vs");
  auto fragment_shader = Shader<GL_FRAGMENT_SHADER>("shader.fs");
  check_shader(vertex_shader);
  check_shader(fragment_shader);
  auto shader_program = make_shaderprogram(vertex_shader, fragment_shader);
  check_program(shader_program);
  shader_program->use();
  
  context.main();

  return 0;
}
