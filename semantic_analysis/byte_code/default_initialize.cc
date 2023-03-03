#include <optional>

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void EmitterBase::EmitDefaultInitialize(core::Type t, FunctionData data) {
  if (std::optional i = t.get_if<core::SizedIntegerType>(type_system())) {
    if (i->bits() == 64) {
      if (i->is_signed()) {
        data.function().append<Construct<int64_t>>(0);
      } else {
        data.function().append<Construct<uint64_t>>(0);
      }
    } else if (i->bits() == 32) {
      if (i->is_signed()) {
        data.function().append<Construct<int32_t>>(0);
      } else {
        data.function().append<Construct<uint32_t>>(0);
      }
    } else if (i->bits() == 16) {
      if (i->is_signed()) {
        data.function().append<Construct<int16_t>>(0);
      } else {
        data.function().append<Construct<uint16_t>>(0);
      }
    } else if (i->bits() == 8) {
      if (i->is_signed()) {
        data.function().append<Construct<int8_t>>(0);
      } else {
        data.function().append<Construct<uint8_t>>(0);
      }
    } else {
      NOT_YET();
    }
  } else if (t == Bool) {
    data.function().append<Construct<bool>>(false);
  } else if (t == Char) {
    data.function().append<Construct<data_types::Char>>(data_types::Char('0'));
  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
