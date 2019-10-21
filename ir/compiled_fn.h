#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include "base/move_func.h"
#include "core/fn_params.h"
#include "ir/block_group.h"
#include "type/typed_value.h"

namespace ast {
struct Expression;
}  // namespace ast

namespace type {
struct Function;
}  // namespace type

namespace ir {

struct CompiledFn : internal::BlockGroup {
  CompiledFn(type::Function const *fn_type,
             core::FnParams<type::Typed<ast::Expression const *>> p);

  std::string name() const;

  type::Function const *type() const { return type_; }
  type::Function const *const type_ = nullptr;

  base::move_func<void()> *work_item = nullptr;

  bool must_inline_ = false;
};

static_assert(alignof(CompiledFn) > 1);

std::ostream &operator<<(std::ostream &, CompiledFn const &);

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_FN_H
