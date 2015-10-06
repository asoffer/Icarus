#include <iostream>

#include "Parser.h"
#include "AST.h"
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
  global_scope->register_scopes();

  for (auto& s : AST::Scope::scope_registry) {
    s->verify_scope();
//    std::cout << s->to_string(0) << std::endl;
  }
 
  return 0;
}
