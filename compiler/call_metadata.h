#ifndef ICARUS_COMPILER_CALL_METADATA_H
#define ICARUS_COMPILER_CALL_METADATA_H

#include <string_view>
#include <variant>

#include "absl/container/flat_hash_set.h"
#include "ast/expression.h"
#include "ast/scope.h"
#include "module/module.h"

namespace compiler {

// Depending on where we are in the call resolution process, we may want to know
// the modules in which we should look up the value, the set of possible
// overloads to check, or the resolved overload actually chosen.

struct CallMetadata {
  using callee_locator_t =
      base::PtrUnion<ast::Expression const,
                     module::Module::SymbolInformation const>;
  explicit CallMetadata(callee_locator_t loc) : data_(loc) {}

  explicit CallMetadata(absl::flat_hash_set<ast::Expression const *> overloads)
      : data_(std::move(overloads)) {}

  explicit CallMetadata(
      std::string_view name, ast::Scope const *primary,
      absl::flat_hash_set<module::BasicModule *> const &modules = {});

  void SetResolved(callee_locator_t loc) {
    ASSERT(std::holds_alternative<absl::flat_hash_set<ast::Expression const *>>(
               data_) == true);
    data_ = loc;
  }

  absl::flat_hash_set<ast::Expression const *> const &overloads() const {
    return std::get<absl::flat_hash_set<ast::Expression const *>>(data_);
  }

  callee_locator_t resolved() const {
    auto loc = std::get_if<callee_locator_t>(&data_);
    return loc ? *loc : static_cast<ast::Expression const *>(nullptr);
  }

 private:
  std::variant<callee_locator_t, absl::flat_hash_set<ast::Expression const *>>
      data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CALL_METADATA_H
