#ifndef ICARUS_IR_JUMP_H
#define ICARUS_IR_JUMP_H

#include "ast/ast.h"
#include "base/move_func.h"
#include "core/fn_params.h"
#include "ir/block_group.h"
#include "type/jump.h"

namespace ir {

struct Jump : internal::BlockGroup {
  explicit Jump(type::Jump const *jump_type,
                core::FnParams<type::Typed<ast::Declaration const *>> p)
      : internal::BlockGroup(std::move(p)), type_(jump_type) {
    AppendBlock();
  }

  type::Jump const *type() const { return type_; }

  base::move_func<void()> *work_item = nullptr;

 private:
  type::Jump const *const type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_JUMP_H
