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
    } else {
      NOT_YET();
    }
  } else if (t == Bool) {
    data.function().append<Construct<bool>>(false);
  } else if (t == Char) {
    data.function().append<Construct<ir::Char>>(ir::Char('0'));
  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
