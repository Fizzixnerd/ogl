
#include "shader_utils.hpp"

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <GL/glew.h>
#include <GL/freeglut.h>

char* get_log(GLuint object) {
  GLint log_length = 0;
  char* log = NULL;
  if (glIsShader(object)) {
    glGetShaderiv(object, GL_INFO_LOG_LENGTH, &log_length);
    log = (char*)malloc(log_length);
    glGetShaderInfoLog(object, log_length, NULL, log);
  }else if (glIsProgram(object)) {
    glGetProgramiv(object, GL_INFO_LOG_LENGTH, &log_length);
    log = (char*)malloc(log_length);
    glGetShaderInfoLog(object, log_length, NULL, log);
  }else {
    fprintf(stderr, "get_log: Not a shader or a program.\n");
  }
  
  return log;
}

void print_log(GLuint object) {
  char* log;
  if ((log = get_log(object))) {
    fprintf(stderr, "%s", log);
    free(log);
  }else {
    fprintf(stderr, "Unable to retrieve log.\n");
  }
}

char* read_file(const char* filename) {
  FILE* input = fopen(filename, "rb");
  if (!input) return NULL;

  if (fseek(input, 0, SEEK_END) == -1) return NULL;
  size_t size = ftell(input);
  if (size == -1) return NULL;
  if (fseek(input, 0, SEEK_SET) == -1) return NULL;
  
  char* content = (char*) malloc(size+1);
  if (!content) return NULL;
  
  fread(content, 1, size, input);
  if (ferror(input)) {
    free(content);
    return NULL;
  }

  fclose(input);
  content[size] = '\0';
  return content;
}

GLuint create_shader(const char* filename, GLenum type) {
  const GLchar* source = read_file(filename);
  if (!source) {
  fprintf(stderr, "Error opening %s: ", filename); perror("");
  return 0;
  }
  GLuint res = glCreateShader(type);
  const GLchar* sources[2] = {
#ifdef GL_ES_VERSION_2_0
    "#version 100\n"
    "#defined GLES2\n",
#else
    "#version 120\n",
#endif
    source };
  glShaderSource(res, 2, sources, NULL);
  free((void*)source);

  glCompileShader(res);
  GLint compile_ok = GL_FALSE;
  glGetShaderiv(res, GL_COMPILE_STATUS, &compile_ok);
  if (compile_ok == GL_FALSE) {
    fprintf(stderr, "%s:", filename);
    print_log(res);
    glDeleteShader(res);
    return 0;
  }

  return res;
}
