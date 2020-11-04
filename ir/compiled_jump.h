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

  explicit CompiledJump(type::Jump const *jump_type,
                        core::Params<type::Typed<ast::Declaration const *>> p)
      : BlockGroup(jump_type, std::move(p), jump_type->state() ? 1 : 0) {}

  absl::flat_hash_map<std::string_view,
                      std::vector<core::Arguments<type::QualType>>>
  ExtractExitPaths() const {
    absl::flat_hash_map<std::string_view,
                        std::vector<core::Arguments<type::QualType>>>
        result;
    if (work_item and *work_item) {
      std::move (*work_item)();
      work_item = nullptr;
    }
    // TODO no reason to repeat this work multiple times.
    for (auto const *block : blocks()) {
      if (auto const *j = block->jump().IfAsChooseJump()) {
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
};

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_JUMP_H
