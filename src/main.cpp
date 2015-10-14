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

    // TODO write the language rules to guarantee that the parser produces a
    // Statements node at top level.
    std::unique_ptr<AST::Statements> global_statements(
        static_cast<AST::Statements*>(parser.parse().release()));

    AST::Scope* global_scope = AST::Scope::make_global();
    global_statements->find_all_decls(global_scope);
    global_statements->join_identifiers(global_scope);
    global_scope->verify_no_shadowing();

    // TODO verify_scope no longer a decent name
    global_scope->verify_scope();
    global_statements->verify_types();

    std::cout << global_statements->to_string(0) << std::endl;

    global_statements->generate_code(global_scope);


  } else {
    std::cerr << "Invalid flag" << std::endl;
  }

  return 0;
}
