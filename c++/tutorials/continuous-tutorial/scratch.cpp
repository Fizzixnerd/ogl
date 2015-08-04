#include <cstdlib>

#include <array>
#include <iostream>

namespace ogl {

  template<class T, std::size_t n, std::size_t m>
  std::array<std::array<T, m>, n> make_matrix(T init) {
    std::array<std::array<T, m>, n> arr;
    for (auto i = 0; i < n; ++i) {
      for (auto j = 0; j < m; ++j) {
	arr[i][j] = init;
      }
    }
    return arr;
  }

  template<class T, std::size_t n, std::size_t m>
  class Matrix {
  public:
    std::array<std::array<T, m>, n> m_matrix;
    T* to_ptr();
    Matrix(T init);
  };

  template<class T, std::size_t n, std::size_t m>
  Matrix<T, n, m>::Matrix(T init) : m_matrix(make_matrix<T, n, m>(init)) {
  }

  template<class T, std::size_t n, std::size_t m>
  T* Matrix<T, n, m>::to_ptr() {
    return &m_matrix[0][0];
  }

	
}

int main() {
    auto matrix = ogl::Matrix<char, 4, 1>('a');
    std::cout << sizeof(matrix) << std::endl;
    std::cout << matrix.to_ptr() << std::endl;
    return 0;
  }

