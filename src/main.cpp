#include <iostream>

#include "Lexer.h"
#include "AST/Node.h"

int main() {
  Lexer lexer("programs/test.ic");

  AST::Node node;
  while (lexer >> node) {
    std::cout << node << std::endl;
  }

  return 0;
}
