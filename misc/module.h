#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <filesystem>
#include <map>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/container/node_hash_map.h"
// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
#include "ast/dispatch_table.h"
#include "ir/scope_def.h"
#endif // ICARUS_VISITOR_EMIT_IR
#include "ast/expression.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "core/pending_module.h"
#include "error/log.h"
#include "ir/register.h"
#include "misc/constant_binding.h"
#include "type/typed_value.h"

namespace type {
struct Type;
struct Function;
struct Jump;
}  // namespace type

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace ast {
struct StructLiteral;
}  // namespace ast

struct Module {
  Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) = delete;

  ir::CompiledFn *AddFunc(
      type::Function const *fn_type,
      core::FnParams<type::Typed<ast::Expression const *>> params);
  ir::CompiledFn *AddJump(
      type::Jump const *jump_type,
      core::FnParams<type::Typed<ast::Expression const *>> params);

  // TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
  type::Type const *GetType(std::string_view name) const;
#endif  // ICARUS_VISITOR_EMIT_IR
  ast::Declaration *GetDecl(std::string_view name) const;

  std::queue<std::function<void()>> deferred_work_;
  void CompleteAllDeferredWork();

  error::Log error_log_;

  core::ModuleScope scope_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  std::vector<std::unique_ptr<ast::Node>> statements_;

  // TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;
#endif  // ICARUS_VISITOR_EMIT_IR

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
    absl::flat_hash_map<ast::Declaration const *, ir::Reg> addr_;

    // TODO probably make these funcs constant.
    absl::node_hash_map<ast::Expression const *, ir::CompiledFn *> ir_funcs_;

    // TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
    // TODO future optimization: the bool determining if it's const is not
    // dependent and can therefore be stored more efficiently (though querying
    // for both simultaneously would be more expensive I guess.
    absl::flat_hash_map<ast::ExprPtr, visitor::VerifyResult> verify_results_;

    absl::flat_hash_map<ast::ExprPtr, ast::DispatchTable> dispatch_tables_;

    // Similar to dispatch tables, but specifically for `jump_handler`s. The
    // tables are keyed first on the pointer to the ScopeLiteral expression and
    // then by the block name (or "" in the case of scope entry).
    absl::flat_hash_map<
        ast::ExprPtr, absl::node_hash_map<std::string_view, ast::DispatchTable>>
        jump_tables_;
    absl::node_hash_map<ast::ScopeLiteral const*, ir::ScopeDef> scope_defs_;

#endif  // ICARUS_VISITOR_EMIT_IR
    ConstantBinding constants_;

    absl::flat_hash_map<ast::Import const *, core::PendingModule> imported_module_;

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
  // Using list because we need to not invalidate pointers to elements on
  // insertion.
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

Module *CompileModule(Module *mod, std::filesystem::path const *path);

#endif  // ICARUS_MODULE_H
