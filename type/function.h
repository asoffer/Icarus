#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <cstring>

#include "ast/ast.h"
#include "core/fn_params.h"
#include "type/callable.h"
#include "type/typed_value.h"

namespace type {
struct GenericFunction : public Callable {
  GenericFunction() {}
  ~GenericFunction() override {}
  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;
};

struct Function : public Callable {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(std::move(in)), output_(std::move(out)) {
    for (auto *t : input) { ASSERT(t != nullptr); }
    for (auto *t : output_) { ASSERT(t != nullptr); }
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  bool is_big() const override { return false; }

  core::FnParams<type::Typed<ast::Declaration const *>> AnonymousFnParams()
      const;
  absl::Span<Type const * const> output() const { return output_; }

  // TODO remove this in favor of function parameters that can have names.
  std::vector<Type const *> input;

 private:
  core::FnParams<Typed<std::string>> input_;
  std::vector<Type const *> output_;
};

Function const *Func(std::vector<Type const *> in,
                     std::vector<Type const *> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
