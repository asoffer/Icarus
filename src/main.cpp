#ifndef ICARUS_UNITY
#include "Parser.h"
#include "Type/Type.h"
#include "Scope.h"
#endif

#include <iomanip>
#include <mach/mach.h>
#include <mach/mach_time.h>

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/MC/SubTargetFeature.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

#include "IR/IR.h"

#include <ncurses.h>
static size_t start_time;
static size_t end_time;
static size_t total_time;
static size_t saved_time;

// Abusing a for-loop to do timings correctly.
#define TIME(msg)                                                              \
  for (bool TIME_FLAG = true;                                                  \
      start_time  = mach_absolute_time(), TIME_FLAG;                           \
      end_time    = mach_absolute_time(),                                      \
      saved_time  = end_time - start_time,                                     \
      total_time += saved_time,                                                \
      debug::timing &&                                                         \
      (std::cout << std::setw(25) << (msg + std::string(":"))                  \
                << std::setw(15) << saved_time << "ns" << std::endl),          \
      TIME_FLAG = false)

extern llvm::Module *global_module;

namespace TypeSystem {
extern void GenerateLLVM();
} // namespace TypeSystem

extern llvm::IRBuilder<> builder;
std::queue<AST::Node *> VerificationQueue;

namespace TypeSystem {
void initialize();
extern Type *get(const std::string &name);
} // namespace TypeSystem


namespace debug {
extern bool timing;
extern bool parser;
extern bool parametric_struct;
extern bool ct_eval;
} // namespace debug

// The keys in this map represent the file names, and the values represent the
// syntax trees from the parsed file.
//
// TODO This is NOT threadsafe! If someone edits the map, it may rebalance and
// a datarace will corrupt the memory. When we start threading, we need to lock
// the map before usage.
extern std::map<std::string, AST::Statements *> ast_map;

// This is an enum so we can give meaningful names for error codes. However, at
// the end of the day, we must return ints. Thus, we need to use the implicit
// cast from enum to int, and so we cannot get the added type safety of an enum
// class.
namespace error_code {
enum {
  success = 0, // returning 0 denotes succes
  cyclic_dependency,
  file_does_not_exist,
  parse_error,
  timing_or_lvalue,
  undeclared_identifier
};
} // namespace error_code

// If the file name does not have an extension, add ".ic" to the end of it.
//
// TODO this is extremely not robust and probably has system dependencies.
std::string canonicalize_file_name(const std::string &filename) {
  auto found_dot = filename.find('.');
  return (found_dot == std::string::npos) ? filename + ".ic" : filename;
}

void ParseArguments(int argc, char *argv[]) {
  for (int arg_num = 1; arg_num < argc; ++arg_num) {
    auto arg = argv[arg_num];

    if (strcmp(arg, "-P") == 0 || strcmp(arg, "-p") == 0) {
      debug::parser = true;

    } else if (strcmp(arg, "-T") == 0 || strcmp(arg, "-t") == 0) {
      debug::timing = true;

    } else if (strcmp(arg, "-S") == 0 || strcmp(arg, "-s") == 0) {
      debug::parametric_struct = true;

    } else if (strcmp(arg, "-E") == 0 || strcmp(arg, "-e") == 0) {
      debug::ct_eval = true;

    } else {
      // Add the file to the queue
      file_queue.emplace(arg);
    }
  }
}

int main(int argc, char *argv[]) {
  TIME("Argument parsing") { ParseArguments(argc, argv); }

  if (debug::ct_eval) { initscr(); }

  llvm::TargetMachine *target_machine = nullptr;
  TIME("Icarus initialization") {
    // Initialize the names for all the primitive types. Used by the lexer.
    TypeSystem::initialize();

    // Initialize the global scope
    Scope::Global = new BlockScope(ScopeType::Global);
    builder.SetInsertPoint(Scope::Global->entry);

  }

  TIME("LLVM initialization") {
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmPrinters();
    LLVMInitializeAllAsmParsers();

    llvm::StringMap<bool> host_features;
    llvm::SubtargetFeatures features;
    if (llvm::sys::getHostCPUFeatures(host_features)) {
      for (auto &feat: host_features) {
        features.AddFeature(feat.first(), feat.second);
      }
    }

    llvm::TargetOptions opt;
    std::string error = "";
    auto target = llvm::TargetRegistry::lookupTarget(
        llvm::sys::getDefaultTargetTriple(), error);
    if (error != "") {
      std::cout << error << std::endl;
      assert(false && "Error in target string lookup");
    }

    target_machine = target->createTargetMachine(
        llvm::sys::getDefaultTargetTriple(), llvm::sys::getHostCPUName(),
        features.getString(), opt);
    assert(target_machine);
  }

  while (!file_queue.empty()) {
    std::string file_name = canonicalize_file_name(file_queue.front());
    file_queue.pop();
    auto iter = ast_map.find(file_name);

    // If we've already parsed this file, don't parse it again.
    if (iter != ast_map.end()) continue;

    // Check if file exists
    std::ifstream infile(file_name);
    if (!infile.good()) {
      // TODO do this with the error log
      std::cerr
        << "File '" << file_name << "' does not exist or cannot be accessed."
        << std::endl;
    }

    TIME(file_name + "\n              ...parsing") {
      Parser parser(file_name);
      ast_map[file_name] = (AST::Statements *)parser.parse();
    }
    
  }

  if (error_log.num_errors() != 0) {
    std::cout << error_log;

    if (debug::ct_eval) { endwin(); }
    return error_code::parse_error;
  }

  AST::Statements *global_statements;

  TIME("AST Setup") {
    // Init global module, function, etc.
    global_module = new llvm::Module("global_module", llvm::getGlobalContext());

    global_module->setDataLayout(target_machine->createDataLayout());

    // TODO write the language rules to guarantee that the parser produces a
    // Statements node at top level.

    // Combine all statement nodes from separately-parsed files.
    global_statements = new AST::Statements;

    // Reserve enough space for all of them to avoid unneeded copies
    size_t num_statements = 0;
    for (const auto &kv : ast_map) { num_statements += kv.second->size(); }
    global_statements->reserve(num_statements);

    for (auto &kv : ast_map) { global_statements->add_nodes(kv.second); }

    // COMPILATION STEP:
    //
    // Determine which declarations go in which scopes. Store that information
    // with the scopes. Note that assign_scope cannot possibly generate
    // compilation errors, so we don't check for them here.
    Scope::Stack.push(Scope::Global);
    global_statements->assign_scope();

    // COMPILATION STEP:
    //
    // Join the identifiers turning the syntax tree into a syntax DAG. This must
    // happen after the declarations are assigned to each scope so we have a
    // specific identifier to point to that is easy to find. This can generate
    // an
    // undeclared identifier error.
    global_statements->join_identifiers();
    Scope::Stack.pop();

    if (error_log.num_errors() != 0) {
      std::cout << error_log;

      if (debug::ct_eval) { endwin(); }
      return error_code::undeclared_identifier;
    }
  }

  // COMPILATION STEP:
  //
  // For each identifier, figure out which other identifiers are needed in
  // order to declare this one. This cannot generate compilation errors.
  // Dependency::record(global_statements);
  // To assign type orders, we traverse the dependency graph looking for a
  // valid ordering in which we can determine the types of the nodes. This can
  // generate compilation errors if no valid ordering exists.
  // Dependency::assign_order();
  TIME("Type verification") {
    VerificationQueue.push(global_statements);
    while (!VerificationQueue.empty()) {
      auto node_to_verify = VerificationQueue.front();
      node_to_verify->verify_types();
      VerificationQueue.pop();
    }
    TypeSystem::GenerateLLVM();

    if (error_log.num_errors() != 0) {
      std::cout << error_log;

      if (debug::ct_eval) { endwin(); }
      return error_code::cyclic_dependency;
    }
  }

  TIME("(L/R)value checking") {
    global_statements->determine_time();
    global_statements->lrvalue_check();

    if (error_log.num_errors() != 0) {
      std::cout << error_log;

      if (debug::ct_eval) { endwin(); }
      return error_code::timing_or_lvalue;
    }
  }

  TIME("Code-gen") {
    { // Program has been verified. We can now proceed with code generation.
      for (auto decl : Scope::Global->ordered_decls_) {

        auto id = decl->identifier;
        if (id->arg_val) { continue; }

        auto type = decl->type;
        if (type->time() == Time::compile) { continue; }

        Scope::Stack.push(Scope::Global);

        if (type->is_primitive() || type->is_array() || type->is_pointer()) {
          auto gvar = new llvm::GlobalVariable(
              /*      Module = */ *global_module,
              /*        Type = */ *type,
              /*  isConstant = */ decl->HasHashtag("const"),
              /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
              /* Initializer = */ 0, // might be specified below
              /*        Name = */ id->token);

          if (decl->IsInferred()) {
            // TODO meld this into GetGlobal
            // decl->evaluate();

            auto global_val = decl->init_val->GetGlobal();
            assert(llvm::isa<llvm::Constant>(global_val) &&
                   "Value is not a constant");
            gvar->setInitializer(global_val);

          } else {
            gvar->setInitializer(type->InitialValue());
          }

          id->alloc = gvar;

        } else if (type->is_array()) {

        } else if (type->is_function()) {
          auto fn_type      = static_cast<Function *>(type);
          auto mangled_name = Mangle(fn_type, decl->identifier);

          if (!type->has_vars) {
            id->alloc = type->allocate();
            id->alloc->setName(mangled_name);
            decl->generate_code();
          }

        } else {
          std::cerr << *type << std::endl;
          assert(false && "Global variables not currently allowed.");
        }

        Scope::Stack.pop();
      }
    }

    { // Generate code for everything else
      Scope::Stack.push(Scope::Global);
      for (auto stmt : global_statements->statements) {
        if (stmt->is_declaration()) { continue; }
        stmt->generate_code();
      }
      Scope::Stack.pop();
    }
  }

  TIME("LLVM") {
    std::error_code EC;
    llvm::raw_fd_ostream destination("obj.o", EC, llvm::sys::fs::F_None);
    if (EC) { assert(false && "Not yet implemented: Write error handling."); }

    llvm::legacy::PassManager pass;

    assert(!target_machine->addPassesToEmitFile(
               pass, destination, llvm::TargetMachine::CGFT_ObjectFile) &&
           "TargetMachine can't emit a file of this type");

    pass.run(*global_module);

    destination.flush();
  }

  TIME("Link/system") {
    std::string input_file_name(argv[1]);

    size_t start = input_file_name.find('/', 0) + 1;
    size_t end   = input_file_name.find('.', 0);

    system(("gcc obj.o -o bin/" + input_file_name.substr(start, end - start)).c_str());
  }

  if (debug::timing) {
    std::cout << std::setw(25) << "TOTAL:" << std::setw(15) << total_time
              << "ns" << std::endl;
  }

  if (debug::ct_eval) { endwin(); }
  return error_code::success;
}

#undef TIME
