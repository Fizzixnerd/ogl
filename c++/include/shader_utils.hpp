#pragma once

#include <GL/glew.h>
char* read_file(const char* filename);
char* get_log(GLuint object);
void print_log(GLuint object);
GLuint create_shader(const char* filename, GLenum type);
