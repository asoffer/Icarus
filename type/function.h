#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <ostream>
#include <sstream>
#include <vector>

#include "core/parameters.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Function : ReturningType {
  Function(core::Parameters<QualType> in, std::vector<Type> out, bool eager)
      : ReturningType(IndexOf<Function>(),
                      LegacyType::Flags{.is_default_initializable = 0,
                                        .is_copyable              = 1,
                                        .is_movable               = 1,
                                        .has_destructor           = 0},
                      std::move(in), std::move(out), eager) {}

  bool is_big() const override { return false; }
  void ShowValue(std::ostream &, ir::CompleteResultRef const &) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return Completeness::Complete; }

  template <typename H>
  friend H AbslHashValue(H h, Function const &f) {
    return H::combine(std::move(h), f.eager(), f.parameters(),
                      f.return_types());
  }

  friend bool operator==(Function const &lhs, Function const &rhs);

  friend bool operator!=(Function const &lhs, Function const &rhs) {
    return not(lhs == rhs);
  }
};

Function const *Func(core::Parameters<QualType> in, std::vector<Type> out);
Function const *EagerFunc(core::Parameters<QualType> in, std::vector<Type> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
