#include <iostream>
#include <fstream>

#include "Parser.h"
#include "AST.h"
#include "typedefs.h"
#include "ScopeDB.h"

// GENERAL TODO LIST:
// * populate_declaration_dependencies runs over the whole AST. This seems like
// overkill. Shouldn't we just record pointers to all declarations and then
// iterate over those?

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cerr
      << "Provide exactly one flag and one argument for the file name."
      << std::endl;
    return 1;

  }

  // Check if file exists
  std::ifstream infile(argv[2]);
  if (!infile.good()) {
    std::cerr
      << "File '"
      << argv[2]
      << "' does not exist or cannot be accessed."
      << std::endl;
    return 2;
  }


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
    auto global_statements =
        std::static_pointer_cast<AST::Statements>(parser.parse());

    size_t global_scope_id = ScopeDB::add_global();


    global_statements->join_identifiers(global_scope_id);

    ScopeDB::fill_db();

    // ScopeDB::verify_no_shadowing();
    //global_statements->populate_declaration_dependencies();
    //ScopeDB::determine_declared_types();

//    global_statements->verify_types();

    std::cout << global_statements->to_string(0) << std::endl;

    //global_statements->generate_code(&AST::Scope::Global);
    std::cout << "-------------------- CLEANUP --------------------" << std::endl;

  } else {
    std::cerr << "Invalid flag" << std::endl;
  }

  return 0;
}
