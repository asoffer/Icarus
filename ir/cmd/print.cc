#include "ir/cmd/print.h"

namespace ir {

std::string PrintCmd::DebugString(base::untyped_buffer::const_iterator* iter) {
  std::string s;
  auto ctrl = iter->read<control_bits>();
  if (ctrl.reg) {
    s.append(stringify(iter->read<Reg>()));
  } else {
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      using base::stringify;
      s.append(stringify(iter->read<T>()));
    });
  }
  return s;
}

}  // namespace ir
