#ifndef ICARUS_TYPE_INTERFACE_IR_H
#define ICARUS_TYPE_INTERFACE_IR_H

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/interface/interface.h"
#include "type/type.h"

namespace interface {

struct ConvertsToInstruction
    : base::Extend<ConvertsToInstruction>::With<ir::ByteCodeExtension,
                                                ir::InlineExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = converts-to %1$s";

  interface::Interface Resolve() const {
    return interface::Interface::ConvertsTo(type.value());
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

}  // namespace interface

#endif  // ICARUS_TYPE_INTERFACE_IR_H
