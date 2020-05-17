#ifndef ICARUS_COMPILER_DISPATCH_OVERLOAD_H
#define ICARUS_COMPILER_DISPATCH_OVERLOAD_H

#include "ast/ast.h"
#include "core/params.h"
#include "core/params_ref.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace internal {

// TODO rename to `Overload` or sometihng similar
struct ExprData {
  // TODO you only really have this for testing purposes (same with the
  // non-const params() method.
  explicit ExprData() = default;

  explicit ExprData(type::Type const *t, core::Params<type::QualType> q,
                    std::vector<type::Type const *> return_types = {})
      : type_(t),
        params_(std::move(q)),
        return_types_(std::move(return_types)) {}

  type::Type const *type() const { return type_; }

  std::vector<type::Type const *> const &return_types() {
    return return_types_;
  }

  core::Params<type::QualType> const &params() const & { return params_; }

 private:
  type::Type const *type_ = nullptr;
  core::Params<type::QualType> params_;
  std::vector<type::Type const *> return_types_;
};

}  // namespace internal
}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_OVERLOAD_H
