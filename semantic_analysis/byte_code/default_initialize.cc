#include <optional>

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void EmitterBase::EmitDefaultInitialize(core::Type t, FunctionData data) {
  if (std::optional i = t.get_if<core::SizedIntegerType>(type_system())) {
    if (i->bits() == 64) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int64_t>(0);
      } else {
        data.function().AppendConstruct<uint64_t>(0);
      }
    } else if (i->bits() == 32) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int32_t>(0);
      } else {
        data.function().AppendConstruct<uint32_t>(0);
      }
    } else if (i->bits() == 16) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int16_t>(0);
      } else {
        data.function().AppendConstruct<uint16_t>(0);
      }
    } else if (i->bits() == 8) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int8_t>(0);
      } else {
        data.function().AppendConstruct<uint8_t>(0);
      }
    } else {
      NTH_UNIMPLEMENTED();
    }
  } else if (t == Bool) {
    data.function().AppendConstruct<bool>(false);
  } else if (t == Char) {
    data.function().AppendConstruct<data_types::Char>(data_types::Char('0'));
  } else if (t.is<core::PointerType>(type_system()) or
             t.is<BufferPointerType>(type_system())) {
    data.function().AppendConstruct<data_types::addr_t>(nullptr);
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {DebugType(t, type_system())};
  }
}

}  // namespace semantic_analysis
