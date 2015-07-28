add_executable(main main.cpp scratch.cpp shader.cpp shaderprogram.cpp context.cpp)

target_link_libraries(main glut GL GLEW)