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

  for (const auto& sc : AST::Scope::all_scopes) {
    sc->join_identifiers(*AST::Scope::all_scopes.begin());
    sc->find_all_decls(*AST::Scope::all_scopes.begin());

    if (sc->log_undeclared_identifiers()) {
      return 0;
    }
  }

  root->verify_types();
  //std::cout << root->to_string(0) << std::endl;


  return 0;
}
