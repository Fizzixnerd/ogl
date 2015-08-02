#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <functional>

#include "context.hpp"

namespace ogl {

  Context::Context(int width,
		   int height,
		   const std::string& title,
		   GLFWmonitor* monitor,
		   GLFWwindow* share) {
    m_window = glfwCreateWindow(width, height, title.c_str(), monitor, share);
  }

  Context::~Context() noexcept {
    glfwDestroyWindow(m_window);
  }

  Context::operator bool() {
    return static_cast<bool>(m_window);
  }

  GLFWwindow* Context::window() {
    return m_window;
  }

  void Context::use() {
    glfwMakeContextCurrent(m_window);
  }

  void Context::swap_buffers() {
    glfwSwapBuffers(m_window);
  }

  bool Context::should_close() {
    return glfwWindowShouldClose(m_window);
  }
}
