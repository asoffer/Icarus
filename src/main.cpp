#include <iostream>
#include <fstream>
#include <string>

#include "Parser.h"
#include "AST.h"
#include "Type.h"
#include "typedefs.h"
#include "ScopeDB.h"
#include "ErrorLog.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

extern llvm::Module* global_module;
extern llvm::Function* global_function;
extern llvm::IRBuilder<> builder;
extern ErrorLog error_log;

#include <cstdio>
extern "C" double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

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
    error_log.set_file(argv[2]);

    Parser parser(argv[2]);
    auto root_node = parser.parse();
    if (error_log.num_errors() != 0) {
      std::cout << error_log;

      return 0;
    }

    // Init global module, function, etc.
    global_module = new llvm::Module("global_module", llvm::getGlobalContext());

    global_function = llvm::Function::Create(
        Type::get_function(Type::get_void(), Type::get_int())->llvm(),
        llvm::Function::ExternalLinkage, "main", global_module);


    // TODO write the language rules to guarantee that the parser produces a
    // Statements node at top level.
    auto global_statements =
        std::static_pointer_cast<AST::Statements>(root_node);

    ScopeDB::Scope* global_scope = ScopeDB::Scope::build();

    global_statements->join_identifiers(global_scope);
    if (error_log.num_errors() != 0) {
      std::cout << error_log;
      return 0;
    }

    ScopeDB::fill_db();
    ScopeDB::assign_decl_order();
    if (error_log.num_errors() != 0) {
      std::cout << error_log;
      return 0;
    }


    ScopeDB::Scope::verify_no_shadowing();
    ScopeDB::Scope::determine_declared_types();
    if (error_log.num_errors() != 0) {
      std::cout << error_log;
      return 0;
    }

    global_statements->verify_types();
    if (error_log.num_errors() != 0) {
      std::cout << error_log;
      return 0;
    }

    std::cout << global_statements->to_string(0) << std::endl;

    global_scope->set_entry(llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "entry", global_function));

    builder.SetInsertPoint(global_scope->entry());

    global_scope->allocate();

    global_statements->generate_code(global_scope);

    builder.CreateRet(llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(32, 0, false)));

    global_module->dump();

//    std::ofstream output_file_stream("foo.ll");
//    llvm::raw_os_ostream output_file(output_file_stream);
//    global_module->print(output_file, nullptr);
//    system("llvm-as foo.ll");
//    system("llc foo.bc");
//    system("clang foo.s -o foo");

    // std::cout << "-------------------- CLEANUP --------------------" << std::endl;
  } else {
    std::cerr << "Invalid flag" << std::endl;
  }

  return 0;
}
