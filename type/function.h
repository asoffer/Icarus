#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <vector>

#include "base/extend.h"
#include "core/params.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Function : public Callable {
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

  absl::Span<type::Type const> return_types() const { return output_; }
  std::vector<type::Type> return_types(
      core::Arguments<type::Typed<ir::Value>> const &args) const override {
    return output_;
  }

  bool is_big() const override { return false; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

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

struct FunctionTypeInstruction
    : base::Extend<FunctionTypeInstruction>::With<ir::ByteCodeExtension,
                                                  ir::InlineExtension> {
  Type Resolve() const {
    core::Params<QualType> params;
    params.reserve(inputs.size());
    for (auto const &[name, t] : inputs) {
      params.append(core::AnonymousParam(QualType::NonConstant(t.value())));
    }

    std::vector<Type> outputs_types;
    outputs_types.reserve(outputs.size());
    for (auto const &t : outputs) { outputs_types.push_back(t.value()); }

    return Func(std::move(params), std::move(outputs_types));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result), " = (",
        absl::StrJoin(inputs, ", ",
                      [](std::string *out,
                         std::pair<std::string, ir::RegOr<Type>> const &r) {
                        if (not r.first.empty()) {
                          out->append(r.first);
                          out->append(": ");
                        }
                        out->append(stringify(r.second));
                      }),
        ") -> (",
        absl::StrJoin(outputs, ", ",
                      [](std::string *out, ir::RegOr<Type> const &r) {
                        out->append(stringify(r));
                      }),
        ")");
  }

  std::vector<std::pair<std::string, ir::RegOr<Type>>> inputs;
  std::vector<ir::RegOr<Type>> outputs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
