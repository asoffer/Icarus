#ifndef ICARUS_IR_COMPILED_JUMP_H
#define ICARUS_IR_COMPILED_JUMP_H

#include "ast/ast.h"
#include "core/params.h"
#include "ir/blocks/group.h"
#include "ir/value/jump.h"
#include "type/jump.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace ir {

struct CompiledJump : BlockGroup<type::Jump> {
  static constexpr CompiledJump const *From(Jump j) { return j.jump_; }

  explicit CompiledJump(type::Jump const *jump_type)
      : BlockGroup(jump_type, jump_type->state() ? 1 : 0) {
    LOG("CompiledJump", "Creating a jump of type %s", jump_type->to_string());
  }
};

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_JUMP_H
