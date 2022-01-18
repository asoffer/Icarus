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
  explicit CallMetadata(ast::Expression const *expr) : data_(expr) {}

  explicit CallMetadata(absl::flat_hash_set<ast::Expression const *> overloads)
      : data_(std::move(overloads)) {}

  explicit CallMetadata(
      std::string_view name, ast::Scope const *primary,
      absl::flat_hash_set<module::BasicModule const *> const &modules = {});

  void SetResolved(ast::Expression const *expr) {
    ASSERT(std::holds_alternative<absl::flat_hash_set<ast::Expression const *>>(
               data_) == true);
    data_ = expr;
  }

  absl::flat_hash_set<ast::Expression const *> const &overloads() const {
    return std::get<absl::flat_hash_set<ast::Expression const *>>(data_);
  }

  ast::Expression const *resolved() const {
    ast::Expression const *const *e =
        std::get_if<ast::Expression const *>(&data_);
    return e ? *e : nullptr;
  }

 private:
  std::variant<ast::Expression const *,
               absl::flat_hash_set<ast::Expression const *>>
      data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CALL_METADATA_H
