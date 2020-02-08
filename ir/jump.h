#ifndef ICARUS_IR_JUMP_H
#define ICARUS_IR_JUMP_H

#include "ast/ast.h"
#include "base/move_func.h"
#include "core/fn_params.h"
#include "ir/block_group.h"
#include "type/jump.h"
#include "type/qual_type.h"

namespace ir {

struct Jump : internal::BlockGroup {
  explicit Jump(type::Jump const *jump_type,
                core::FnParams<type::Typed<ast::Declaration const *>> p)
      : internal::BlockGroup(std::move(p)), type_(jump_type) {}

  type::Jump const *type() const { return type_; }

  base::move_func<void()> *work_item = nullptr;

  absl::flat_hash_map<std::string_view,
                      std::vector<core::FnArgs<type::QualType>>>
  ExtractExitPaths() const {
    absl::flat_hash_map<std::string_view,
                        std::vector<core::FnArgs<type::QualType>>>
        result;
    if (work_item) { std::move (*work_item)(); }
    // TODO no reason to repeat this work multiple times.
    for (auto const *block : blocks()) {
      if (auto const *j = block->jump_.IfAsChooseJump()) {
        for (size_t i = 0; i < j->size(); ++i) {
          result[j->names()[i]].push_back(
              j->args()[i].Transform([](auto const &arg) {
                return type::QualType::NonConstant(arg.type());
              }));
        }
      }
    }
    return result;
  }

 private:
  type::Jump const *const type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_JUMP_H
