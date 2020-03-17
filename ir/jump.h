#ifndef ICARUS_IR_JUMP_H
#define ICARUS_IR_JUMP_H

#include "ast/ast.h"
#include "base/move_func.h"
#include "core/params.h"
#include "ir/block_group.h"
#include "type/jump.h"
#include "type/pointer.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace ir {

struct Jump : internal::BlockGroup {
  explicit Jump(type::Jump const *jump_type,
                core::Params<type::Typed<ast::Declaration const *>> p)
      : internal::BlockGroup(std::move(p), jump_type->state() ? 1 : 0),
        type_(jump_type) {}

  type::Jump const *type() const { return type_; }

  base::move_func<void()> *work_item = nullptr;

  absl::flat_hash_map<std::string_view,
                      std::vector<core::FnArgs<type::QualType>>>
  ExtractExitPaths() {
    absl::flat_hash_map<std::string_view,
                        std::vector<core::FnArgs<type::QualType>>>
        result;
    if (work_item and *work_item) {
      std::move (*work_item)();
      work_item = nullptr;
    }
    // TODO no reason to repeat this work multiple times.
    for (auto const *block : blocks()) {
      if (auto const *j = block->jump_.IfAsChooseJump()) {
        for (size_t i = 0; i < j->size(); ++i) {
          result[j->names()[i]].push_back(
              j->args()[i].Transform([](auto const &a) {
                return type::QualType::NonConstant(a.type());
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
