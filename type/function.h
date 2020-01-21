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
  Function(core::FnParams<Type const *> in, std::vector<Type const *> out)
      : input_(std::move(in)), output_(std::move(out)) {
#if defined(ICARUS_DEBUG)
    for (auto *t : input) { ASSERT(t != nullptr); }
    for (auto *t : output_) { ASSERT(t != nullptr); }
#endif  // defined(ICARUS_DEBUG)
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  bool is_big() const override { return false; }

  // TODO This doesn't need to be anonymous anymore!
  core::FnParams<type::Typed<ast::Declaration const *>> AnonymousFnParams()
      const;

  core::FnParams<Type const *> const &input() const { return input_; }
  absl::Span<Type const * const> output() const { return output_; }

 private:
  // Each `Param<Type const*>` has a `std::string_view` member representing the
  // parameter name. This is viewing an identifier owned by a declaration in the
  // syntax tree which means it is valid for the lifetime of the syntax tree.
  // However, this type is never destroyed, so it's lifetime is indeed longer
  // than that of the syntax tree.
  //
  // TODO either fix this, or come up with a simple and robust rule we can
  // follow to ensure this is safe. Do we keep the syntax tree around for the
  // lifetime of the program? Any program?
  core::FnParams<Type const*> input_;
  std::vector<Type const *> output_;
};

Function const *Func(core::FnParams<Type const *> in,
                     std::vector<Type const *> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
