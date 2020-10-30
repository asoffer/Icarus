#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/fn.h"
#include "ir/value/value.h"
#include "type/primitive.h"

namespace compiler {

enum SpecialFunctionCategory { Copy, Move };

template <SpecialFunctionCategory Cat>
static ir::NativeFn CreateAssign(Compiler *compiler, type::Struct const *s) {
  auto &bldr       = compiler->builder();
  type::QualType q = type::QualType::NonConstant(type::Ptr(s));
  auto fn_type =
      type::Func(core::Params<type::QualType>{core::AnonymousParam(q),
                                              core::AnonymousParam(q)},
                 {});
  ir::NativeFn fn = compiler->AddFunc(
      fn_type, fn_type->params().Transform([](type::QualType q) {
        return type::Typed<ast::Declaration const *>(nullptr, q.type());
      }));
  ICARUS_SCOPE(ir::SetCurrent(fn)) {
    bldr.CurrentBlock() = fn->entry();
    auto val            = ir::Reg::Arg(0);
    auto var            = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields_.size(); ++i) {
      auto field_type = s->fields_.at(i).type;
      auto from       = ir::Value(
          compiler->builder().PtrFix(bldr.Field(val, s, i).get(), field_type));
      auto to = bldr.Field(var, s, i).get();

      // TODO use the tag in place of `Cat`.
      if constexpr (Cat == Copy) {
        compiler->EmitCopyAssign(
            type::Typed<ir::RegOr<ir::Addr>>(to, field_type),
            type::Typed<ir::Value>(from, field_type));
      } else if constexpr (Cat == Move) {
        compiler->EmitMoveAssign(
            type::Typed<ir::RegOr<ir::Addr>>(to, field_type),
            type::Typed<ir::Value>(from, field_type));
      } else {
        UNREACHABLE();
      }
    }

    bldr.ReturnJump();
  }
  fn->WriteByteCode<interpretter::instruction_set_t>();
  return fn;
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  to.type()->copy_assign_func_.init(
      [=]() { return CreateAssign<Copy>(this, to.type()); });
  builder().Copy(to.type(), from->get<ir::Reg>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  to.type()->move_assign_func_.init(
      [=]() { return CreateAssign<Move>(this, to.type()); });
  builder().Move(to.type(), from->get<ir::Reg>(), *to);
}

}  // namespace compiler
