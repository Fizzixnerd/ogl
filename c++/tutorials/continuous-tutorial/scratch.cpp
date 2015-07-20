
#include <iostream>
#include <cstdlib>


namespace ogl {

  const std::size_t byte_size = 8;

  template<std::size_t n>
  class Foo {
    char array[n / byte_size + 1];
  };

  template<class T>
  class Bar : public T {
  };

  int scratch() {
    Foo<1000> foo = Foo<1000>();
    Bar<Foo<1> > bar = Bar<Foo<1> >();
    std::cout << sizeof(foo) << std::endl;
    
    return 0;
  }
}
  
