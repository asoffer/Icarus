#ifndef ICARUS_TYPE_SCOPE_H
#define ICARUS_TYPE_SCOPE_H

#include <ostream>
#include <sstream>
#include <vector>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "core/params.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Scope : Callable {
  Scope(core::Params<QualType> params)
      : Callable(IndexOf<Scope>(),
                 LegacyType::Flags{.is_default_initializable = 0,
                                   .is_copyable              = 1,
                                   .is_movable               = 1,
                                   .has_destructor           = 0},
                 std::move(params)) {}

  bool is_big() const override { return false; }
  void ShowValue(std::ostream &, ir::CompleteResultRef const &) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return Completeness::Complete; }
};

Scope const *Scp(core::Params<QualType> params);

struct ScopeTypeInstruction
    : base::Extend<ScopeTypeInstruction>::With<base::BaseSerializeExtension,
                                               base::BaseTraverseExtension> {
  Type Resolve() const {
    core::Params<QualType> params;
    params.reserve(inputs.size());
    for (auto const &[name, t] : inputs) {
      params.append(
          name.empty()
              ? core::AnonymousParam(QualType::NonConstant(t.value()))
              : core::Param<type::QualType>(std::move(name),
                                            QualType::NonConstant(t.value())));
    }

    std::vector<Type> outputs_types;
    return Scp(std::move(params));
  }

  friend std::ostream &operator<<(std::ostream &os,
                                  ScopeTypeInstruction const &f) {
    os << f.result << "scope (";
    char const *separator = "";
    for (auto const &[name, type] : f.inputs) {
      os << std::exchange(separator, ", ") << name << ": " << type;
    }
    return os << ")";
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  std::vector<std::pair<std::string, ir::RegOr<Type>>> inputs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_SCOPE_H
