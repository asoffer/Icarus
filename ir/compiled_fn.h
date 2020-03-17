#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include "base/move_func.h"
#include "core/params.h"
#include "ir/block_group.h"
#include "type/function.h"

namespace ir {

struct CompiledFn : internal::BlockGroup {
  CompiledFn(type::Function const *fn_type,
             core::Params<type::Typed<ast::Declaration const *>> p);
  type::Function const *type() const { return type_; }

  base::move_func<void()> *work_item = nullptr;

  friend std::ostream &operator<<(std::ostream &, CompiledFn const &);

 private:
  type::Function const *type_ = nullptr;
};

static_assert(alignof(CompiledFn) > 1);


}  // namespace ir

#endif  // ICARUS_IR_COMPILED_FN_H
