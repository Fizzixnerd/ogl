# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.2

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/matt/src/ogl/c++/tutorials/continuous-tutorial

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build

# Include any dependencies generated for this target.
include CMakeFiles/main.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/main.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/main.dir/flags.make

CMakeFiles/main.dir/main.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/main.cpp.o: ../main.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/main.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/main.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/main.cpp

CMakeFiles/main.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/main.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/main.cpp > CMakeFiles/main.dir/main.cpp.i

CMakeFiles/main.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/main.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/main.cpp -o CMakeFiles/main.dir/main.cpp.s

CMakeFiles/main.dir/main.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/main.cpp.o.requires

CMakeFiles/main.dir/main.cpp.o.provides: CMakeFiles/main.dir/main.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/main.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/main.cpp.o.provides

CMakeFiles/main.dir/main.cpp.o.provides.build: CMakeFiles/main.dir/main.cpp.o

CMakeFiles/main.dir/shader.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/shader.cpp.o: ../shader.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/shader.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/shader.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shader.cpp

CMakeFiles/main.dir/shader.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/shader.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shader.cpp > CMakeFiles/main.dir/shader.cpp.i

CMakeFiles/main.dir/shader.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/shader.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shader.cpp -o CMakeFiles/main.dir/shader.cpp.s

CMakeFiles/main.dir/shader.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/shader.cpp.o.requires

CMakeFiles/main.dir/shader.cpp.o.provides: CMakeFiles/main.dir/shader.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/shader.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/shader.cpp.o.provides

CMakeFiles/main.dir/shader.cpp.o.provides.build: CMakeFiles/main.dir/shader.cpp.o

CMakeFiles/main.dir/shaderprogram.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/shaderprogram.cpp.o: ../shaderprogram.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/shaderprogram.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/shaderprogram.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shaderprogram.cpp

CMakeFiles/main.dir/shaderprogram.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/shaderprogram.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shaderprogram.cpp > CMakeFiles/main.dir/shaderprogram.cpp.i

CMakeFiles/main.dir/shaderprogram.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/shaderprogram.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/shaderprogram.cpp -o CMakeFiles/main.dir/shaderprogram.cpp.s

CMakeFiles/main.dir/shaderprogram.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/shaderprogram.cpp.o.requires

CMakeFiles/main.dir/shaderprogram.cpp.o.provides: CMakeFiles/main.dir/shaderprogram.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/shaderprogram.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/shaderprogram.cpp.o.provides

CMakeFiles/main.dir/shaderprogram.cpp.o.provides.build: CMakeFiles/main.dir/shaderprogram.cpp.o

CMakeFiles/main.dir/context.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/context.cpp.o: ../context.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/context.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/context.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/context.cpp

CMakeFiles/main.dir/context.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/context.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/context.cpp > CMakeFiles/main.dir/context.cpp.i

CMakeFiles/main.dir/context.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/context.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/context.cpp -o CMakeFiles/main.dir/context.cpp.s

CMakeFiles/main.dir/context.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/context.cpp.o.requires

CMakeFiles/main.dir/context.cpp.o.provides: CMakeFiles/main.dir/context.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/context.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/context.cpp.o.provides

CMakeFiles/main.dir/context.cpp.o.provides.build: CMakeFiles/main.dir/context.cpp.o

CMakeFiles/main.dir/uniform.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/uniform.cpp.o: ../uniform.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_5)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/uniform.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/uniform.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/uniform.cpp

CMakeFiles/main.dir/uniform.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/uniform.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/uniform.cpp > CMakeFiles/main.dir/uniform.cpp.i

CMakeFiles/main.dir/uniform.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/uniform.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/uniform.cpp -o CMakeFiles/main.dir/uniform.cpp.s

CMakeFiles/main.dir/uniform.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/uniform.cpp.o.requires

CMakeFiles/main.dir/uniform.cpp.o.provides: CMakeFiles/main.dir/uniform.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/uniform.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/uniform.cpp.o.provides

CMakeFiles/main.dir/uniform.cpp.o.provides.build: CMakeFiles/main.dir/uniform.cpp.o

CMakeFiles/main.dir/matrix.cpp.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/matrix.cpp.o: ../matrix.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles $(CMAKE_PROGRESS_6)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/main.dir/matrix.cpp.o"
	clang++-3.6   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/main.dir/matrix.cpp.o -c /home/matt/src/ogl/c++/tutorials/continuous-tutorial/matrix.cpp

CMakeFiles/main.dir/matrix.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/main.dir/matrix.cpp.i"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/matt/src/ogl/c++/tutorials/continuous-tutorial/matrix.cpp > CMakeFiles/main.dir/matrix.cpp.i

CMakeFiles/main.dir/matrix.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/main.dir/matrix.cpp.s"
	clang++-3.6  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/matt/src/ogl/c++/tutorials/continuous-tutorial/matrix.cpp -o CMakeFiles/main.dir/matrix.cpp.s

CMakeFiles/main.dir/matrix.cpp.o.requires:
.PHONY : CMakeFiles/main.dir/matrix.cpp.o.requires

CMakeFiles/main.dir/matrix.cpp.o.provides: CMakeFiles/main.dir/matrix.cpp.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/matrix.cpp.o.provides.build
.PHONY : CMakeFiles/main.dir/matrix.cpp.o.provides

CMakeFiles/main.dir/matrix.cpp.o.provides.build: CMakeFiles/main.dir/matrix.cpp.o

# Object files for target main
main_OBJECTS = \
"CMakeFiles/main.dir/main.cpp.o" \
"CMakeFiles/main.dir/shader.cpp.o" \
"CMakeFiles/main.dir/shaderprogram.cpp.o" \
"CMakeFiles/main.dir/context.cpp.o" \
"CMakeFiles/main.dir/uniform.cpp.o" \
"CMakeFiles/main.dir/matrix.cpp.o"

# External object files for target main
main_EXTERNAL_OBJECTS =

main: CMakeFiles/main.dir/main.cpp.o
main: CMakeFiles/main.dir/shader.cpp.o
main: CMakeFiles/main.dir/shaderprogram.cpp.o
main: CMakeFiles/main.dir/context.cpp.o
main: CMakeFiles/main.dir/uniform.cpp.o
main: CMakeFiles/main.dir/matrix.cpp.o
main: CMakeFiles/main.dir/build.make
main: CMakeFiles/main.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX executable main"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/main.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/main.dir/build: main
.PHONY : CMakeFiles/main.dir/build

CMakeFiles/main.dir/requires: CMakeFiles/main.dir/main.cpp.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/shader.cpp.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/shaderprogram.cpp.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/context.cpp.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/uniform.cpp.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/matrix.cpp.o.requires
.PHONY : CMakeFiles/main.dir/requires

CMakeFiles/main.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/main.dir/cmake_clean.cmake
.PHONY : CMakeFiles/main.dir/clean

CMakeFiles/main.dir/depend:
	cd /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matt/src/ogl/c++/tutorials/continuous-tutorial /home/matt/src/ogl/c++/tutorials/continuous-tutorial /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build /home/matt/src/ogl/c++/tutorials/continuous-tutorial/build/CMakeFiles/main.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/main.dir/depend

