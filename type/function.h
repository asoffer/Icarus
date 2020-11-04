#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <vector>

#include "core/params.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Function : public Callable {
  TYPE_FNS(Function);
  Function(core::Params<QualType> in, std::vector<Type> out)
      : params_(std::move(in)), output_(std::move(out)) {
#if defined(ICARUS_DEBUG)
    for (auto const &p : params_) { ASSERT(p.value != QualType::Error()); }
    for (Type t : output_) { ASSERT(t.valid() == true); }
#endif  // defined(ICARUS_DEBUG)
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  std::vector<type::Type> return_types(
      core::Arguments<type::Typed<ir::Value>> const &args) const override {
    return output_;
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override { return Completeness::Complete; }

  core::Params<QualType> const &params() const { return params_; }
  absl::Span<Type const> output() const { return output_; }

 private:
  // Each `Param<LegacyType const*>` has a `std::string_view` member
  // representing the parameter name. This is viewing an identifier owned by a
  // declaration in the syntax tree which means it is valid for the lifetime of
  // the syntax tree. However, this type is never destroyed, so it's lifetime is
  // indeed longer than that of the syntax tree.
  //
  // TODO either fix this, or come up with a simple and robust rule we can
  // follow to ensure this is safe. Do we keep the syntax tree around for the
  // lifetime of the program? Any program?
  core::Params<QualType> params_;
  std::vector<Type> output_;
};

Function const *Func(core::Params<QualType> in, std::vector<Type> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
