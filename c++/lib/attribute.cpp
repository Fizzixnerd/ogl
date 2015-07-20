#include <cstdio>
#include <cstdlib>
#include <string>
#include <array>
#include <iostream>
#include <glm/glm.hpp>

using namespace std;

template<typename T, int N> class Attribute {
  array<T, N> values;
  string name;
  static const size_t size = sizeof(T) * N;
};

template<> class Attribute<float, 3> {
  glm::vec3 values;
  string name;
  static const size_t size = sizeof(float) * 3;
};

template<> class Attribute<float, 4> {
  glm::vec4 values;
  string name;
  static const size_t size = sizeof(float) * 4;
};


typedef Attribute<float, 3> Position3;
typedef Attribute<float, 4> Position4;
typedef Attribute<float, 3> Color3;
typedef Attribute<float, 4> Color4;

