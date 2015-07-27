#include <cstdio>
#include <iostream>
#include <cassert>
#include <GL/glew.h>
#include <GL/freeglut.h>

#include "oglmath.hpp"
#include "shader.hpp"
#include "shaderprogram.hpp"

GLuint VBO;

/** This function renders the scene properly and is overall a good
    thing to be having yes yes. */
static void RenderSceneCB() {
  glClear(GL_COLOR_BUFFER_BIT);

  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

  glDrawArrays(GL_TRIANGLES, 0, 3);

  glDisableVertexAttribArray(0);

  glutSwapBuffers();
}

static void InitializeGlutCallbacks() {
  glutDisplayFunc(RenderSceneCB);
}

static void CreateVertexBuffer() {
  using namespace ogl;
  
  //Vector3f vertices[3];
  //vertices[0] = make_vector(-1.0f, -1.0f, 0.0f);
  //vertices[1] = make_vector(1.0f, -1.0f, 0.0f);
  //vertices[2] = make_vector(0.0f, 1.0f, 0.0f);
  //std::cerr << vertices[0] << vertices[1] << vertices[2] << std::endl;

  GLfloat vertices[9] = {-1.0f, -1.0f, 0.0f,
			 1.0f, -1.0f, 0.0f,
  			 0.0f, 1.0f, 0.0f};

  glGenBuffers(1, &VBO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
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
  if (sp) {
    std::cerr << "good!" << std::endl;
  } else {
    std::cerr << "bad!!" << std::endl;
    std::cerr << sp->infolog() << std::endl;
  }
}

int main(int argc, char** argv) {
  using namespace ogl;
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
  glutInitWindowSize(1024, 768);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("Tutorial 04");

  InitializeGlutCallbacks();

  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
    return 1;
  }

  std::printf("GL version %s\n", glGetString(GL_VERSION));

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

  CreateVertexBuffer();

  auto vertex_shader = Shader<GL_VERTEX_SHADER>("shader.vs");
  auto fragment_shader = Shader<GL_FRAGMENT_SHADER>("shader.fs");
  check_shader(vertex_shader);
  check_shader(fragment_shader);
  auto shader_program = make_shaderprogram(vertex_shader, fragment_shader);
  check_program(shader_program);
  shader_program->use();

  glutMainLoop();

  return 0;
}
