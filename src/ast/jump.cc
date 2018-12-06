#include "ast/jump.h"

#include "ir/cmd.h"
#include "ir/val.h"

namespace ast {
base::vector<ir::Val> Jump::EmitIR(Context *ctx) {
  ir::ReturnJump();
  return {};
}
}  // namespace ast
