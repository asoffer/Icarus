#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/value/interface.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"

namespace type {

struct ConvertsToInstruction
    : base::Extend<ConvertsToInstruction>::With<ir::ByteCodeExtension,
                                                ir::InlineExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = converts-to %1$s";

  ir::Interface Resolve() const {
    return ir::Interface::ConvertsTo(type.value());
  }

  ir::RegOr<Type> type;
  ir::Reg result;
};

}  // namespace type
