// This program doesn't actually work.  It correctly opens and closes
// the window, but doesn't draw the fucking triangle.  I am giving up
// on it and going to try another example, from
// www.arcsynthesis.org/gltut/Basics/Tutorial 01.html.

#include <stdio.h>
#include <stdlib.h>
#include <GLES3/gl3.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>

using namespace glm;
int main (int argv, char** argc) {
  if (!glfwInit()) {
    fprintf(stderr, "Failed to initialize GLFW\n.");
    return 1;
  }

  glfwWindowHint(GLFW_SAMPLES, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  GLFWwindow* window = glfwCreateWindow(1024, 768, "Tutorial 1", NULL, NULL);
  if (!window) {
    fprintf(stderr, "Failed to open GLFW window.\n");
    glfwTerminate();
    return 2;
  }

  glfwMakeContextCurrent(window);
  glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);

  GLuint vertex_array_id;
  glGenVertexArrays(1, &vertex_array_id);
  glBindVertexArray(vertex_array_id);

  static const GLfloat g_vertex_buffer_data [] = {
    -1.0f, -1.0f, 0.0f,
    1.0f, -1.0f, 0.0f,
    0.0f, 1.0f, 0.0f,
  };
  GLuint vertex_buffer_id;
  glGenBuffers(1, &vertex_buffer_id);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer_id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data),
  	       g_vertex_buffer_data, GL_STATIC_DRAW);

  glViewport(0, 0, 1024, 768);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glClearColor(0, 0, 0, 0);

  do {
    // Draw here.
    glColor3b(1, 1, 1);
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer_id);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glDisableVertexAttribArray(0);

    glfwSwapBuffers(window);
    glfwPollEvents();
  } while ((glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS)
	   and !glfwWindowShouldClose(window));
  
  return 0;
}
