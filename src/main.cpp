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

#include "clargs.h"
static size_t start_time;
static size_t end_time;

struct Timer {
  Timer(){};

  std::vector<const char *> msgs;
  std::vector<size_t> times;

  ~Timer() {
    if (debug::timer) {
      size_t total = 0;
      for (auto time : times) { total += time; }

      for (size_t i = 0; i < msgs.size(); ++i) {
        char percent_buffer[10];
        sprintf(percent_buffer, "(%.2f%%)", ((double)(100 * times[i])) / total);
        fprintf(stderr, "%25s:%15luns %9s\n", msgs[i], times[i],
                percent_buffer);
      }

      fprintf(stderr, "%25s:%15luns\n", "Total", total);
    }
  }
} timer;
// Abusing a for-loop to do timings correctly.
#define TIME(msg)                                                              \
  for (bool TIME_FLAG = true;                                                  \
                                                                               \
      start_time  = mach_absolute_time(), TIME_FLAG;                           \
                                                                               \
      end_time    = mach_absolute_time(),                                      \
      timer.msgs.push_back(msg),                                               \
      timer.times.push_back(end_time - start_time),                            \
      TIME_FLAG = false)

extern llvm::Module *global_module;
extern llvm::TargetMachine *target_machine;
namespace TypeSystem {
extern void GenerateLLVM();
} // namespace TypeSystem

extern llvm::IRBuilder<> builder;
std::queue<AST::Node *> VerificationQueue;

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
  CL_arg_failure,
  cyclic_dependency,
  file_does_not_exist,
  parse_error,
  lvalue,
  undeclared_identifier
};
} // namespace error_code

void WriteObjectFile(const char *out_file) {
  TIME("LLVM") {
    std::error_code EC;
    llvm::raw_fd_ostream destination(out_file, EC, llvm::sys::fs::F_None);
    if (EC) { assert(false && "Not yet implemented: Write error handling."); }

    llvm::legacy::PassManager pass;

    assert(!target_machine->addPassesToEmitFile(
               pass, destination, llvm::TargetMachine::CGFT_ObjectFile) &&
           "TargetMachine can't emit a file of this type");

    pass.run(*global_module);

    destination.flush();
  }
}

int main(int argc, char *argv[]) {

  TIME("Argument parsing") {
    switch(ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return error_code::success;
      case CLArgFlag::QuitWithFailure: return error_code::CL_arg_failure;
      case CLArgFlag::Continue:;
    }
  }

  TIME("Icarus Initialization") {
    if (debug::ct_eval) { initscr(); }

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
      std::cerr << error << std::endl;
      assert(false && "Error in target string lookup");
    }

    // Hack to get the right target triple on my system:
    std::string triple_string = llvm::sys::getDefaultTargetTriple();
    auto last_pos             = triple_string.rfind("15");
    triple_string             = triple_string.substr(0, last_pos) + "10.11";

    target_machine = target->createTargetMachine(
        triple_string, llvm::sys::getHostCPUName(), features.getString(), opt);
    assert(target_machine);
  }

  while (!file_queue.empty()) {
    std::string file_name = file_queue.front();
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

    TIME("Parsing a file") {
      Parser parser(file_name);
      ast_map[file_name] = (AST::Statements *)parser.parse();
    }
  }

  if (error_log.num_errors() != 0) {
    std::cerr << error_log;

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
    Scope::Stack.pop();
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
      std::cerr << error_log;

      if (debug::ct_eval) { endwin(); }
      return error_code::cyclic_dependency;
    }
  }

  TIME("(L/R)value checking") {
    global_statements->determine_time();
    global_statements->lrvalue_check();

    if (error_log.num_errors() != 0) {
      std::cerr << error_log;

      if (debug::ct_eval) { endwin(); }
      return error_code::lvalue;
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

          id->decl->alloc = gvar;

        } else if (type->is_array()) {

        } else if (type->is_function()) {
          assert(decl->identifier);
          assert(decl->identifier->decl == decl);
          auto fn_type      = (Function *)type;
          auto mangled_name = Mangle(fn_type, decl->identifier);

          if (!type->has_vars) {
            id->decl->alloc = type->allocate();
            id->decl->alloc->setName(mangled_name);
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

  switch (file_type) {
  case FileType::IR: {
    TIME("Writing IR") {
      std::ofstream output_file_stream(output_file_name);
      llvm::raw_os_ostream output_file(output_file_stream);
      global_module->print(output_file, nullptr);
    }
  } break;

  case FileType::Nat: {
    WriteObjectFile(output_file_name);

  } break;

  case FileType::Bin: {
    WriteObjectFile("obj.o");

    TIME("Link/system") {
      int buff_size  = (int)(strlen(output_file_name) + 24);
      char *link_str = new char[buff_size];
      int num_bytes_written =
          sprintf(link_str, "gcc obj.o -o %s; rm obj.o", output_file_name);
      assert(buff_size = num_bytes_written + 1);
      system(link_str);
      delete[] link_str;
    }
  } break;
  }

  if (debug::ct_eval) { endwin(); }
  return error_code::success;
}

#undef TIME
