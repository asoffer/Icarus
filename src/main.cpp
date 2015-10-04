#include <iostream>

#include "Parser.h"
#include "AST/Node.h"
#include "AST/Scope.h"
#include "AST/AnonymousScope.h"
#include "typedefs.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr
      << "Provide exactly one argument for the file name."
      << std::endl;
    return 1;
  }


  Parser parser(argv[1]);
  auto global_scope = AST::AnonymousScope::build_empty();
  global_scope->add_statements(parser.parse());

  global_scope->verify_scope();

  std::cout << global_scope->to_string(0) << std::endl;

  return 0;
}
