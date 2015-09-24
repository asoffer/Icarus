#include <iostream>

#include "Parser.h"
#include "AST/Node.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr
      << "Provide exactly one argument for the file name."
      << std::endl;
      return 1;
  }

  Parser parser(argv[1]);

  parser.parse();

  return 0;
}
