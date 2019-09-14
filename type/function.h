#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <cstring>

#include "core/fn_params.h"
#include "type/callable.h"
#include "type/typed_value.h"

namespace ast {
struct Expression;
}  // namespace ast

namespace type {
struct GenericFunction : public Callable {
  GenericFunction() {}
  ~GenericFunction() override {}
  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

#include "visitor/type_visitors.xmacro.h"

  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;
};

struct Function : public Callable {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(std::move(in)), output(std::move(out)) {
    for (auto *t : input) { ASSERT(t != nullptr); }
    for (auto *t : output) { ASSERT(t != nullptr); }
  }

#include "visitor/type_visitors.xmacro.h"

  core::FnParams<type::Typed<ast::Expression const *>> AnonymousFnParams()
      const;

  std::vector<const Type *> input, output;
};

Function const *Func(std::vector<Type const *> in,
                     std::vector<Type const *> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
