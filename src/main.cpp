#include <iostream>
#include <fstream>
#include <string>

#include "Parser.h"
#include "AST.h"
#include "Type.h"
#include "typedefs.h"
#include "Scope.h"
#include "ErrorLog.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

extern llvm::Module* global_module;
namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data

extern ErrorLog error_log;

namespace debug {
  extern bool parser;
}  // namespace


// This is an enum so we can give meaningful names for error codes.
// However, at the end of the day, we must return ints. Thus, we need
// to use the implicit cast from enum to int, and so we cannot get the
// added type safety of an enum class.
namespace error_code {
  enum {
    success = 0,  // returning 0 denotes succes

    cyclic_dependency,
    file_does_not_exist,
    invalid_arguments,
    parse_error,
    shadow_or_type_error,
    undeclared_identifier
  };
}  // namespace error_code

int main(int argc, char *argv[]) {
  int arg_num = 1; // iterator over argv
  int file_index = -1;  // Index of where file name is in argv
  while (arg_num < argc) {
    auto arg = argv[arg_num];

    if (strcmp(arg, "-D") == 0 ||
        strcmp(arg, "-d") == 0) {
      debug::parser = true;
    } else if (file_index == -1) {
      // If we haven't seen a file yet, point to it
      file_index = arg_num;

    } else {
      // If we have found a file already, error out.
      std::cerr
        << "Provide exactly one file name."
        << std::endl;
      return error_code::invalid_arguments;
    }

    ++arg_num;
  }

  // If the file name has no "." in it, append ".ic"
  std::string file_name(argv[file_index]);
  auto found_dot = file_name.find('.');
  if (found_dot == std::string::npos) {
    file_name += ".ic";
  }

  // Check if file exists
  std::ifstream infile(file_name);
  if (!infile.good()) {
    std::cerr
      << "File '" << file_name << "' does not exist or cannot be accessed."
      << std::endl;
    return error_code::file_does_not_exist;
  }

  error_log.set_file(file_name);

  Parser parser(file_name);
  auto root_node = parser.parse();
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return error_code::parse_error;
  }

  // Init global module, function, etc.
  global_module = new llvm::Module("global_module", llvm::getGlobalContext());

  // TODO write the language rules to guarantee that the parser produces a
  // Statements node at top level.
  auto global_statements =
    std::static_pointer_cast<AST::Statements>(root_node);

  auto global_scope = Scope::build_global();

  // COMPILATION STEP:
  //
  // Determine which declarations go in which scopes. Store that information
  // with the scopes. Note that assign_decl_to_scope cannot possibly generate
  // compilation errors, so we don't check for them here.
  global_statements->assign_decl_to_scope(global_scope);

  // COMPILATION STEP:
  //
  // Join the identifiers turning the syntax tree into a syntax DAG. This must
  // happen after the declarations are assigned to each scope so we have a
  // specific identifier to point to that is easy to find. This can generate an
  // undeclared identifier error.
  global_statements->join_identifiers(global_scope);

  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return error_code::undeclared_identifier;
  }

  // COMPILATION STEP:
  //
  // fill_db() has several housekeeping functionalities. It ensures that the
  // vector of declarations ordered by dependency in each scope is cleared at
  // this point. It initializes the table of dependencies. It also populates
  // the decl_of_ database, so that we can quickly find a declaration from the
  // identifier being declared. fill_db() cannot generate compilation errors.
  Scope::fill_db();
  // For each identifier, figure out which other identifiers are needed in
  // order to declare this one. This cannot generate compilation errors.
  global_statements->record_dependencies(nullptr);
  // To assign type orders, we traverse the dependency graph looking for a
  // valid ordering in which we can determine the types of the nodes. This can
  // generate compilation errors if no valid ordering exists.
  Scope::assign_type_order();
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return error_code::cyclic_dependency;
  }

  // COMPILATION STEP:
  //
  // The name should be self-explanatory. This function looks through the
  // scope tree for a node and its ancestor with declared identifiers of the
  // same name. We do not allow shadowing of any kind whatsoever. Errors are
  // generated if shadows are encountered. However, we are still able to
  // continue on with determining the declared types (and give useful error
  // messages, so we don't exit just yet.
  Scope::Scope::verify_no_shadowing();
  // Associate to each identifier its type.
  Scope::Scope::determine_declared_types();
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return error_code::shadow_or_type_error;
  }

  // Program has been verified. We can now proceed with code generation.
  // Initialize the global_scope.

  // Generate LLVM intermediate representation.
  global_scope->initialize();
  global_statements->generate_code(global_scope);

  Scope::Scope::determine_declared_types();
  if (error_log.num_errors() != 0) {
    std::cout << error_log;
    return error_code::shadow_or_type_error;
  }

  // TODO Optimization.


  {
    // In this anonymous scope we write the LLVM IR to a file. The point
    // of the anonymous scope is to ensure that the file is written and
    // closed before we make system calls on it (e.g., for linking).
    std::ofstream output_file_stream("ir.ll");
    llvm::raw_os_ostream output_file(output_file_stream);
    global_module->print(output_file, nullptr);
  }
  

  std::string input_file_name(argv[1]);
  std::string link_string = "gcc ir.o -o bin/";
  size_t start = input_file_name.find('/', 0) + 1;
  size_t end = input_file_name.find('.', 0);
  link_string += input_file_name.substr(start, end - start);

  system("llc -filetype=obj ir.ll");
  system(link_string.c_str());
  system("rm ir.o");

  return error_code::success;
}
