#include "ir/scope.h"

#include "ir/function.h"

namespace ic {

void Scope::AppendTo(IrFunction& f) const {
  for (jasmin::Value v : implementation_.raw_instructions()) {
    f.raw_append(v);
  }
}

}  // namespace ic
