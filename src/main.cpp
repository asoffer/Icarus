#include <iostream>

#include "Parser.h"
#include "AST/Node.h"
#include "AST/Scope.h"
#include "typedefs.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr
      << "Provide exactly one argument for the file name."
      << std::endl;
      return 1;
  }

  Parser parser(argv[1]);

  NPtr root = parser.parse();
  root->separate_declarations_and_assignments();

  for(const auto& scope : AST::Scope::all_scopes) {
    scope->register_declared_variables();
  }

  return 0;
}
