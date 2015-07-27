#pragma once

#include <memory>

namespace mem {
  template<class T>
  std::unique_ptr<T> make_unique() {
    return std::unique_ptr<T>(new T());
  }
}
