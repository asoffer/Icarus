#include <iostream>

#include "Lexer.h"
#include "AST/Node.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr
      << "Provide exactly one argument for the file name."
      << std::endl;
      return 1;
  }

  Lexer lexer(argv[1]);

  AST::Node node;
  while (lexer >> node) {
    std::cout << node << std::endl;
  }

  return 0;
}
