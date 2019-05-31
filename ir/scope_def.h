#ifndef ICARUS_IR_SCOPE_DEF_H
#define ICARUS_IR_SCOPE_DEF_H

#include "ir/any_func.h"

namespace ast {
struct ScopeLiteral;
}  // namespace ast

namespace ir {
struct ScopeDef {
  explicit ScopeDef(ast::ScopeLiteral const *lit) : lit_(lit) {}

  void AddInit(AnyFunc f) { inits_.insert(f); }
  void AddDone(AnyFunc f) { dones_.insert(f); }

  ast::ScopeLiteral const *lit_ = nullptr;
  base::bag<AnyFunc> inits_, dones_;
};
}  // namespace ir

#endif  // ICARUS_IR_SCOPE_DEF_H
