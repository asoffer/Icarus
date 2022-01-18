#ifndef ICARUS_TYPE_INTERFACE_IR_H
#define ICARUS_TYPE_INTERFACE_IR_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/interface/interface.h"
#include "type/type.h"

namespace interface {

struct ConvertsToInstruction
    : base::Extend<ConvertsToInstruction>::With<base::BaseSerializeExtension,
                                                base::BaseTraverseExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = converts-to %1$s";

  Interface Resolve() const { return Interface::ConvertsTo(type.value()); }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct JustInstruction
    : base::Extend<JustInstruction>::With<base::BaseSerializeExtension,
                                          base::BaseTraverseExtension,
                                          ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = just %1$s";

  Interface Resolve() const { return Interface::Just(type.value()); }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct CallableInstruction
    : base::Extend<CallableInstruction>::With<base::BaseSerializeExtension,
                                              base::BaseTraverseExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = callable %1$s %2$s";

  Interface Resolve() const {
    std::vector<type::Type> pos_args;
    pos_args.reserve(positional.size());
    for (ir::RegOr<type::Type> p : positional) {
      pos_args.push_back(p.value());
    }
    absl::flat_hash_map<std::string, type::Type> named_args;
    named_args.reserve(named.size());
    for (auto const& [n, v] : named) { named_args.emplace(n, v.value()); }
    return Interface::Callable(
        core::Arguments(std::move(pos_args), std::move(named_args)));
  }

  std::vector<ir::RegOr<type::Type>> positional;
  absl::flat_hash_map<std::string, ir::RegOr<type::Type>> named;
  ir::Reg result;
};

}  // namespace interface

#endif  // ICARUS_TYPE_INTERFACE_IR_H
