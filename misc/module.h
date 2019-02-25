#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <filesystem>
#include <future>
#include <map>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast/bound_constants.h"
#include "ast/dispatch/table.h"
#include "ast/fn_params.h"
#include "ast/node_lookup.h"
#include "ast/statements.h"
#include "error/log.h"
#include "scope.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Module;
class LLVMContext;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace ir {
struct Func;
}  // namespace ir

namespace ast {
struct Expression;
struct FunctionLiteral;
struct StructLiteral;
}  // namespace ast

struct PendingModule;

struct Module {
  Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) = delete;

  static PendingModule Schedule(
      error::Log *log, std::filesystem::path const &src,
      std::filesystem::path const &requestor = std::filesystem::path{""});

  ir::Func *AddFunc(type::Function const *fn_type,
                    ast::FnParams<ast::Expression *> params);
  type::Type const *GetType(std::string const &name) const;
  ast::Declaration *GetDecl(std::string const &name) const;

  std::map<ast::BoundConstants, std::unordered_set<ast::Expression const *>>
      completed_;

  std::queue<std::function<void()>> deferred_work_;
  void CompleteAllDeferredWork();
  void CompleteAll();

  error::Log error_log_;

  std::unique_ptr<DeclScope> global_;

  // Holds all constants defined in the module (both globals and scoped
  // constants). These are the values in the map. They're keyed on conditional
  // constants. So we have options for mulitple meanings of things depending on
  // context.
  //
  // TODO Almost surely this needs to be even deeper, treating it as a tree
  // of arbitrary depth.
  std::map<ast::BoundConstants, ast::BoundConstants> constants_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  ast::Statements statements_;

#ifdef ICARUS_USE_LLVM
  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
#endif  // ICARUS_USE_LLVM

  std::vector<std::unique_ptr<ir::Func>> fns_;

  type::Type const *set_type(ast::BoundConstants const &bc,
                             ast::Expression const *expr, type::Type const *);
  type::Type const *type_of(ast::BoundConstants const &bc,
                            ast::Expression const *expr) const;
  ir::Register addr(ast::BoundConstants const &bc,
                    ast::Declaration *decl) const;
           
  // TODO support more than just a single type argument to generic structs.
  struct GenericStructCache {
    std::map<std::vector<type::Type const *>, type::Type const *> fwd_;
    std::unordered_map<type::Type const *,
                       std::vector<type::Type const *> const *>
        back_;
  };

  std::unordered_map<ast::StructLiteral const *, GenericStructCache>
      generic_struct_cache_;

  struct DependentData {
    ast::NodeLookup<type::Type const *> types_;
    std::unordered_map<ast::Declaration *, ir::Register> addr_;

    std::unordered_map<ast::Expression const *, ir::Func *> ir_funcs_;

    std::unordered_map<ast::Declaration const *, ast::VerifyResult>
        verify_results_;

    std::unordered_map<ast::Expression const *, ast::DispatchTable>
        dispatch_tables_;
    // For use with expression nodes that have more than one dispatch table.
    std::unordered_map<ast::Node const *, std::vector<ast::DispatchTable>>
        repeated_dispatch_tables_;
  };
  std::map<ast::BoundConstants, DependentData> data_;

  std::filesystem::path const *path_ = nullptr;
};

void AwaitAllModulesTransitively();

struct PendingModule {
 public:
  PendingModule() = default;
  explicit PendingModule(Module *mod)
      : data_(reinterpret_cast<uintptr_t>(mod)) {}
  explicit PendingModule(std::shared_future<Module *> *mod)
      : data_(reinterpret_cast<uintptr_t>(mod) | 0x01) {}

  // Returns the compiled module, possibly blocking if `get` is called before
  // the module has finished compiling.
  Module *get();

  bool valid() const { return data_ != 0; }

 private:
  uintptr_t data_ = 0;
};
#endif  // ICARUS_MODULE_H
