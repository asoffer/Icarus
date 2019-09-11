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
#include "core/fn_params.h"
#include "core/pending_module.h"
#include "core/scope.h"
#include "error/log.h"
#include "ir/register.h"
#include "misc/constant_binding.h"
#include "type/typed_value.h"

#ifdef ICARUS_VISITOR_EMIT_IR
#include "misc/dependent_data.h"
#endif  // ICARUS_VISITOR_EMIT_IR

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

#ifdef ICARUS_VISITOR_EMIT_IR
  std::list<std::pair<ConstantBinding, DependentData>> dep_data_;

  std::pair<ConstantBinding, DependentData> *insert_constants(
      ConstantBinding const &constant_binding);
#endif  // ICARUS_VISITOR_EMIT_IR

  std::filesystem::path const *path_ = nullptr;
};

Module *CompileModule(Module *mod, std::filesystem::path const *path);

#endif  // ICARUS_MODULE_H
