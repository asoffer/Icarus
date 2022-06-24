#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <deque>
#include <memory>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "absl/functional/any_invocable.h"
#include "base/debug.h"
#include "base/iterator.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"
#include "ir/value/module_id.h"
#include "ir/value/scope.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {

struct NativeFunctionInformation {
  type::Function const *type() const { return type_; }
  Subroutine subroutine;
  type::Function const *type_;
};

// Holds all information about generated IR.
struct Module {
  explicit Module(ModuleId m) : module_id_(m) {}

  NativeFunctionInformation const &function(LocalFnId id) const {
    ASSERT(id.value() < functions_.size());
    return functions_[id.value()];
  }

  LocalFnId MakePlaceholder(type::Function const *f) {
    size_t n = functions_.size();
    functions_.push_back({.type_ = f});
    return LocalFnId(n);
  }
  void Insert(LocalFnId fn, Subroutine subroutine) {
    functions_[fn.value()].subroutine = std::move(subroutine);
  }

  Fn InsertFunction(type::Function const *t,
                    absl::FunctionRef<void(ir::Subroutine &)> initializer);

  Scope InsertScope(type::Scope const *scope_type);

  // Inject special member functions. These functions allocate space for, but do
  // not actually compile the functions.
  std::pair<Fn, bool> InsertInit(
      type::Type t, absl::FunctionRef<void(ir::Subroutine &)> initializer);
  std::pair<Fn, bool> InsertDestroy(
      type::Type t, absl::FunctionRef<void(ir::Subroutine &)> initializer);
  std::pair<Fn, bool> InsertMoveAssign(
      type::Type to, type::Type from,
      absl::FunctionRef<void(ir::Subroutine &)> initializer);
  std::pair<Fn, bool> InsertCopyAssign(
      type::Type to, type::Type from,
      absl::FunctionRef<void(ir::Subroutine &)> initializer);
  std::pair<Fn, bool> InsertMoveInit(
      type::Type to, type::Type from,
      absl::FunctionRef<void(ir::Subroutine &)> initializer);
  std::pair<Fn, bool> InsertCopyInit(
      type::Type to, type::Type from,
      absl::FunctionRef<void(ir::Subroutine &)> initializer);

  auto functions() const {
    return base::iterator_range(functions_.begin(), functions_.end());
  }
  auto scopes() const {
    return base::iterator_range(scopes_.begin(), scopes_.end());
  }

  template <base::Serializer S>
  friend void BaseSerialize(S &s, Module const &m) requires(
      base::SupportsDelayed<S, size_t>) {
    using delay_token = decltype(s.template delayed<size_t>());
    std::vector<delay_token> delay_tokens;

    base::Serialize(s, m.functions_.size());

    // Reserve space for us to write the offset of each function.
    delay_tokens.reserve(m.functions_.size());
    for (auto const &f : m.functions_) {
      delay_tokens.push_back(s.template delayed<size_t>());
    }
    auto token_iter = delay_tokens.begin();
    for (auto const &f : m.functions_) {
      token_iter->set(s.size());
      ++token_iter;
      NOT_YET();
    }
  }

 private:
  LocalFnId InsertFunctionIndex(type::Function const *fn_type);

  // All functions in this module, whether they're directly compiled or
  // generated by a generic. We use a std::deque because we rarely
  // iterate through them, but want pointer-stability.
  std::deque<NativeFunctionInformation> functions_;
  std::deque<Subroutine> scopes_;

  absl::node_hash_map<Scope,
                      std::pair<std::string, std::unique_ptr<Scope::Data>>>
      scope_data_;

  ModuleId module_id_;
  absl::node_hash_map<type::Type, LocalFnId> init_, destroy_;
  absl::node_hash_map<std::pair<type::Type, type::Type>, LocalFnId>
      copy_assign_, move_assign_, copy_init_, move_init_;
};

}  // namespace ir

#endif  // ICARUS_IR_MODULE_H
