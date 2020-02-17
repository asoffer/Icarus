#ifndef ICARUS_COMPILER_DISPATCH_OVERLOAD_H
#define ICARUS_COMPILER_DISPATCH_OVERLOAD_H

#include "ast/ast.h"
#include "core/params.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace internal {

// TODO rename to `Overload` or sometihng similar
struct ExprData {
  // TODO you only really have this for testing purposes (same with the
  // non-const params() method.
  explicit ExprData() = default;

  explicit ExprData(type::Type const *t,
                    core::Params<type::Typed<ast::Declaration const *>> p)
      : type_(t), params_(std::move(p)) {}

  type::Type const *type() const { return type_; }

  core::Params<type::Typed<ast::Declaration const *>> &params() {
    return params_;
  }
  core::Params<type::Typed<ast::Declaration const *>> const &params() const {
    return params_;
  }

 private:
  type::Type const *type_ = nullptr;
  core::Params<type::Typed<ast::Declaration const *>> params_;
};

}  // namespace internal
}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_OVERLOAD_H
