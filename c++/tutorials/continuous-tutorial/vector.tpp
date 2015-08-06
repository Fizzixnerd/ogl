#pragma once

#include "vector.hpp"

namespace ogl {

  template<class T>
  std::array<T, 1> make_singleton_array(T t) {
    std::array<T, 1> ret = {t};
    return ret;
  }
  
  template<class T, class... Ts>
  Vector<T, sizeof...(Ts) + 1> make_vector(T v1, Ts... args) {
    const std::size_t sz = sizeof...(Ts) + 1;
    std::array<std::array<T, sz>, 1> vals = {{v1, args...}};
    return Vector<T, sz>{vals};
  }
}
