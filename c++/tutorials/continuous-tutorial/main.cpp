#include <cstdio>
#include <iostream>
#include <cassert>
#include <GL/glew.h>
#include <GL/freeglut.h>

#include "oglmath.hpp"

GLuint VBO;

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
  
  Vector3f vertices[3];
  vertices[0] = make_vector(-1.0f, -1.0f, 0.0f);
  vertices[1] = make_vector(1.0f, -1.0f, 0.0f);
  vertices[2] = make_vector(0.0f, 1.0f, 0.0f);
  std::cout << vertices[0] << vertices[1] << vertices[2] << std::endl;

  glGenBuffers(1, &VBO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
}

int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
  glutInitWindowSize(1024, 768);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("Tutorial 02");
  
  InitializeGlutCallbacks();

  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
    return 1;
  }

  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

  CreateVertexBuffer();

  glutMainLoop();

  return 0;
}
