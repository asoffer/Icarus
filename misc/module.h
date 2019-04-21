#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <filesystem>
#include <future>
#include <map>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/container/node_hash_map.h"
#include "ast/dispatch_table.h"
#include "ast/expression.h"
#include "ast/statements.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "error/log.h"
#include "misc/constant_binding.h"

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
struct CompiledFn;
}  // namespace ir

namespace ast {
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

  ir::CompiledFn *AddFunc(type::Function const *fn_type,
                    core::FnParams<type::Typed<ast::Expression *>> params);
  type::Type const *GetType(std::string const &name) const;
  ast::Declaration *GetDecl(std::string const &name) const;

  std::queue<std::function<void()>> deferred_work_;
  void CompleteAllDeferredWork();
  void CompleteAll();

  error::Log error_log_;

  core::ModuleScope scope_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  ast::Statements statements_;

#ifdef ICARUS_USE_LLVM
  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
#endif  // ICARUS_USE_LLVM

  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;

  // TODO support more than just a single type argument to generic structs.
  struct GenericStructCache {
    std::map<std::vector<type::Type const *>, type::Type const *> fwd_;
    absl::flat_hash_map<type::Type const *,
                       std::vector<type::Type const *> const *>
        back_;
  };

  absl::flat_hash_map<ast::StructLiteral const *, GenericStructCache>
      generic_struct_cache_;

  struct DependentData {
    // TODO I'm not sure this needs to be dependent? Or stored at all?
    absl::flat_hash_map<ast::Declaration *, ir::Reg> addr_;

    // TODO probably make these funcs constant.
    absl::node_hash_map<ast::Expression const *, ir::CompiledFn *> ir_funcs_;

    // TODO future optimization: the bool determining if it's const is not
    // dependent and can therefore be stored more efficiently (though querying
    // for both simultaneously would be more expensive I guess.
    absl::flat_hash_map<ast::ExprPtr, ast::VerifyResult> verify_results_;

    absl::flat_hash_map<ast::ExprPtr, ast::DispatchTable> dispatch_tables_;
    ConstantBinding constants_;
  };
  // TODO It's possible to have layers of constant bindings in a tree-like
  // structure. For example,
  //   f :: (a :: int64) => (b :: int64) => (c :: int64) => a + b * c
  // has 3 layers. Essentially the number of layers is the number of nested
  // scopes that have constant parameters (at time of writing only functions and
  // struct literals, though struct literals may not be specified as constants
  // syntactically?). For now you just store them flat in this vector and check
  // them potentially many times. Perhaps a tree-like structure would be more
  // efficient? More cache misses, but you're already paying heavily for the
  // equality call, so maybe it's just a simpler structure.
  //
  // Using list because we need to not invalidate pointers to elements on insertion.
  std::list<std::pair<ConstantBinding, DependentData>> dep_data_;

  std::pair<ConstantBinding, DependentData> *insert_constants(
      ConstantBinding const &constant_binding) {
    for (auto iter = dep_data_.begin(); iter != dep_data_.end(); ++iter) {
      auto &[key, val] = *iter;
      if (key == constant_binding) { return &*iter; }
    }
    auto &pair = dep_data_.emplace_back(constant_binding, DependentData{});
    pair.second.constants_ = pair.first;
    return &pair;
  }

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
