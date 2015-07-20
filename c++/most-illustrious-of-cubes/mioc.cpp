#define _USE_MATH_DEFINES
#define GLM_FORCE_RADIANS

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <GL/glew.h>
#include <GL/freeglut.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "include/shader_utils.hpp"

using namespace std;
using namespace glm;

GLuint program;
GLuint vbo_cube;
GLuint ibo_cube;
GLint attribute_pos, attribute_color;
GLint uniform_mvp;
int width = 1024, height = 768;

struct Vertex {
  vec3 pos;
  vec3 color;
};

int check_bind(int bind_ret_val, const char* bind_name, const char* attr) {
  if (bind_ret_val == -1) {
    fprintf(stderr, "Could not bind %s %s.\n", attr, bind_name);
    return 0;
  }else {
    return 1;
  }
}

int check_attribute_bind(int bind_ret_val, const char* attribute_name) {
  return check_bind(bind_ret_val, attribute_name, "attribute");
}

int check_uniform_bind(int bind_ret_val, const char* uniform_name) {
  return check_bind(bind_ret_val, uniform_name, "uniform");
}

int init_resources() {
  GLuint vs, fs;
  if ((vs = create_shader("vs.glsl", GL_VERTEX_SHADER)) == 0) return 0;
  if ((fs = create_shader("fs.glsl", GL_FRAGMENT_SHADER)) == 0) return 0;

  program = glCreateProgram();
  glAttachShader(program, vs);
  glAttachShader(program, fs);
  glLinkProgram(program);
  
  GLint link_ok;
  glGetProgramiv(program, GL_LINK_STATUS, &link_ok);
  if (!link_ok) {
    fprintf(stderr, "glLinkProgram:");
    print_log(program);
    return 0;
  }

  Vertex cube_attributes[] = {
    // front
    {{-1.0, -1.0,  1.0}, {1.0, 0.0, 0.0}},
    {{ 1.0, -1.0,  1.0}, {0.0, 1.0, 0.0}},
    {{ 1.0,  1.0,  1.0}, {1.0, 0.0, 1.0}},
    {{-1.0,  1.0,  1.0}, {1.0, 1.0, 1.0}},
    // back
    {{-1.0, -1.0, -1.0}, {1.0, 0.0, 0.0}},
    {{ 1.0, -1.0, -1.0}, {0.0, 1.0, 0.0}},
    {{ 1.0,  1.0, -1.0}, {0.0, 0.0, 1.0}},
    {{-1.0,  1.0, -1.0}, {1.0, 1.0, 1.0}}
  };

  GLushort cube_elements[] = {
    // front
    0, 1, 2,
    2, 3, 0,
    // top
    3, 2, 6,
    6, 7, 3,
    // back
    7, 6, 5,
    5, 4, 7,
    // bottom
    4, 5, 1,
    1, 0, 4,
    // left
    4, 0, 3,
    3, 7, 4,
    // right
    1, 5, 6,
    6, 2, 1,
  };

  const char* uniform_name = "mvp";
  uniform_mvp = glGetUniformLocation(program, uniform_name);
  if (!check_uniform_bind(uniform_mvp, uniform_name)) {
    return 0;
  }
  
  const char* attribute_name = "pos";
  attribute_pos = glGetAttribLocation(program, attribute_name);
  if (!check_attribute_bind(attribute_pos, attribute_name)) {
    return 0;
  }

  attribute_name = "color";
  attribute_color = glGetAttribLocation(program, attribute_name);
  if (!check_attribute_bind(attribute_color, attribute_name)) {
    return 0;
  }

  glGenBuffers(1, &ibo_cube);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo_cube);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(cube_elements), cube_elements,
	       GL_STATIC_DRAW);

  glGenBuffers(1, &vbo_cube);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_cube);
  glBufferData(GL_ARRAY_BUFFER, sizeof(cube_attributes), cube_attributes,
	       GL_STATIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  return 1;
}

void onIdle() {
  float current_time = glutGet(GLUT_ELAPSED_TIME) / 1000.0;

  mat4 model = translate(mat4(1.0f), vec3(0.0, 0.0, -4.0));
  mat4 view = lookAt(vec3(0.0, 2.0, 0.0), vec3(0.0, 0.0, -4.0), vec3(0.0, 1.0, 0.0));
  mat4 projection = perspective(static_cast<float>(M_PI/4.0f), 1.0f*width/height, 0.1f, 10.0f);

  float angle = current_time * M_PI / 4.0;
  vec3 axis_y(0.0, 1.0, 0.0);
  mat4 anim = rotate(mat4(1.0f), angle, axis_y);

  mat4 mvp = projection * view * model * anim;
  glUniformMatrix4fv(uniform_mvp, 1, GL_FALSE, value_ptr(mvp));

  glUseProgram(program);
  glutPostRedisplay();
}

void onDisplay() {
  glClearColor(1.0, 1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glEnable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram(program);

  glBindBuffer(GL_ARRAY_BUFFER, vbo_cube);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo_cube);
  int size;
  glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);

  glEnableVertexAttribArray(attribute_pos);
  glVertexAttribPointer(attribute_pos,
			3,
			GL_FLOAT,
			GL_FALSE,
			sizeof(Vertex),
			0
			);
  glEnableVertexAttribArray(attribute_color);
  glVertexAttribPointer(attribute_color,
			3,
			GL_FLOAT,
			GL_FALSE,
			sizeof(Vertex),
			(void*) offsetof(Vertex,color)
			);

  glDrawElements(GL_TRIANGLES, size/sizeof(GLushort), GL_UNSIGNED_SHORT, 0);

  glDisableVertexAttribArray(attribute_pos);
  glDisableVertexAttribArray(attribute_color);

  glutSwapBuffers();
}

void onReshape(int new_width, int new_height) {
  width = new_width;
  height = new_height;
  glViewport(0, 0, width, height);
}

void free_resources() {
  glDeleteProgram(program);
  glDeleteBuffers(1, &vbo_cube);
}

int main (int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitContextVersion(3, 0);
  glutInitDisplayMode(GLUT_RGBA | GLUT_ALPHA | GLUT_DOUBLE | GLUT_DEPTH);
  glutInitWindowSize(width, height);
  glutCreateWindow("BEHOLD!!!!");

  glewExperimental = true;
  GLenum glew_status = glewInit();
  if (glew_status != GLEW_OK) {
    fprintf(stderr, "Error: %s\n", glewGetErrorString(glew_status));
    return EXIT_FAILURE;
  }
  if (!GLEW_VERSION_3_3 && !GLEW_VERSION_3_2 && !GLEW_VERSION_3_1) {
    fprintf(stderr, "Error: GLEW is shit and doesn't support OpenGL 3.1+ on this computer.\n");
  }

  if (init_resources() == 1) {
    glutDisplayFunc(onDisplay);
    glutIdleFunc(onIdle);
    glutReshapeFunc(onReshape);
    glutMainLoop();
  }

  free_resources();
  return EXIT_SUCCESS;
}
