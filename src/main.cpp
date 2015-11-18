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

namespace cstdlib {
  extern llvm::Constant* printf;
  extern llvm::Constant* putchar;
  extern llvm::Constant* puts;
}  // namespace cstdlib

extern ErrorLog error_log;

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr
      << "Provide exactly one file name."
      << std::endl;
    return 1;
  }

  // Check if file exists
  std::ifstream infile(argv[1]);
  if (!infile.good()) {
    std::cerr
      << "File '"
      << argv[2]
      << "' does not exist or cannot be accessed."
      << std::endl;
    return 2;
  }

  error_log.set_file(argv[1]);

  Parser parser(argv[1]);
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

  global_statements->assign_decl_to_scope(global_scope);
  global_statements->join_identifiers(global_scope);
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return 0;
  }

  ScopeDB::fill_db();
  global_statements->record_dependencies(nullptr);
  ScopeDB::assign_type_order();
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return 0;
  }

  // std::cout << global_statements->to_string(0) << std::endl;
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


  global_scope->set_entry(llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "entry", global_function));

  builder.SetInsertPoint(global_scope->entry());

  // Declaration for call to putchar for printing characters
  cstdlib::putchar = global_module->getOrInsertFunction("putchar",
      llvm::FunctionType::get(Type::get_int()->llvm(), { Type::get_char()->llvm() }, false));
  cstdlib::printf = global_module->getOrInsertFunction("printf",
      llvm::FunctionType::get(Type::get_int()->llvm(), { llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) }, true));
  cstdlib::puts = global_module->getOrInsertFunction("puts",
      llvm::FunctionType::get(Type::get_int()->llvm(), { llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) }, false));
 
  global_scope->allocate();

  global_statements->generate_code(global_scope);

  builder.CreateRet(llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(32, 0, false)));

  {
    std::ofstream output_file_stream("ir.ll");
    llvm::raw_os_ostream output_file(output_file_stream);
    global_module->print(output_file, nullptr);
  } // Ensure the stream writes before system calls
  

  std::string input_file_name(argv[1]);
  std::string link_string = "gcc ir.o -o bin/";
  size_t start = input_file_name.find('/', 0) + 1;
  size_t end = input_file_name.find('.', 0);
  link_string += input_file_name.substr(start, end - start);

  std::cout << link_string << std::endl;
  system("llc -filetype=obj ir.ll");
  system(link_string.c_str());
  system("rm ir.o");

  return 0;
}
