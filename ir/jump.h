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
                core::Params<type::Typed<ast::Declaration const *>> p,
                type::Type const *state_type = nullptr)
      : internal::BlockGroup(std::move(p)),
        type_(jump_type),
        state_type_(state_type) {}

  type::Jump const *type() const { return type_; }

  base::move_func<void()> *work_item = nullptr;

  absl::flat_hash_map<std::string_view,
                      std::vector<core::FnArgs<type::QualType>>>
  ExtractExitPaths(type::Type const *state_type) const {
    absl::flat_hash_map<std::string_view,
                        std::vector<core::FnArgs<type::QualType>>>
        result;
    if (work_item) { std::move (*work_item)(); }
    // TODO no reason to repeat this work multiple times.
    for (auto const *block : blocks()) {
      if (auto const *j = block->jump_.IfAsChooseJump()) {
        for (size_t i = 0; i < j->size(); ++i) {
          core::FnArgs<type::QualType> args;
          if (state_type) {
            args.pos_emplace(type::QualType::NonConstant(type::Ptr(state_type)));
          }
          for (auto const &pos_arg : j->args()[i].pos()) {
            args.pos_emplace(type::QualType::NonConstant(pos_arg.type()));
          }
          for (auto const &[name, arg] : j->args()[i].named()) {
            args.named_emplace(name, type::QualType::NonConstant(arg.type()));
          }
          result[j->names()[i]].push_back(std::move(args));
        }
      }
    }
    return result;
  }

  type::Type const *state_type() const { return state_type_; }

 private:
  type::Jump const *const type_                        = nullptr;
  [[maybe_unused]] type::Type const *const state_type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_JUMP_H
