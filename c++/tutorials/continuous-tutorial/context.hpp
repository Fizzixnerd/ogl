#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <functional>
#include <string>

namespace ogl {

  /** An OpenGL Context object representing the current context.  A
      simple wrapper around glut. */
  class Context {
    GLFWwindow* m_window;

  public:
    
    Context(int width,
	    int height,
	    const std::string& title,
	    GLFWmonitor* monitor,
	    GLFWwindow* share);
    ~Context() noexcept;
    GLFWwindow* window();
    operator bool();
    void use();
    void swap_buffers();
    bool should_close();
  };
}
