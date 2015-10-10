#include <iostream>

#include "Parser.h"
#include "AST.h"
#include "typedefs.h"

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cerr
      << "Provide exactly one flag and one argument for the file name."
      << std::endl;
    return 1;

  }

  // TODO check if file exists

  if (std::strcmp(argv[1], "-l") == 0) {
    Lexer lexer(argv[2]);
    AST::Node token;
    while (lexer) {
      lexer >> token;
      std::cout << token;
    }

  } else if (std::strcmp(argv[1], "-p") == 0) {
    Parser parser(argv[2]);

    auto global_scope = AST::AnonymousScope::build_empty();

    global_scope->add_statements(parser.parse());
    global_scope->register_scopes(nullptr);

    for (auto& s : AST::Scope::scope_registry) {
      s->verify_scope();
    }

    global_scope->verify_types();

    std::cout << global_scope->to_string(0) << std::endl;

    AST::TheModule = llvm::make_unique<llvm::Module>(
        "icarus program", llvm::getGlobalContext());

    std::cout
      << "========================================"
      << "========================================" << std::endl;
    AST::TheModule->dump();

    std::cout
      << "========================================"
      << "========================================" << std::endl;

  } else {
    std::cerr << "Invalid flag" << std::endl;
  }

  return 0;
}
