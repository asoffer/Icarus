#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Index const *node) {
  auto const *qt = data().qual_type(node->lhs());
  if (qt->quals() >= type::Quals::Ref()) {
    return ir::Value(builder().PtrFix(EmitRef(node).reg(), qt->type()));
  }

  if (qt->type() == type::ByteView) {
    auto data = builder().ByteViewData(
        EmitValue(node->lhs()).get<ir::RegOr<ir::String>>());
    auto addr = builder().PtrIncr(
        data, EmitValue(node->rhs()).get<ir::RegOr<int64_t>>(),
        type::Ptr(type::Nat8));
    return builder().Load(addr, type::Nat8);
  } else if (auto const *array_type = qt->type()->if_as<type::Array>()) {
    auto index = builder().CastTo<int64_t>(type::Typed<ir::Value>(
        EmitValue(node->rhs()), data().qual_type(node->rhs())->type()));

    return ir::Value(builder().PtrFix(
        builder().Index(type::Ptr(type_of(node->lhs())),
                        EmitValue(node->lhs()).get<ir::Reg>(), index),
        qt->type()));
  } else {
    UNREACHABLE(*this, *qt);
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Index const *node) {
  auto const &lhs_type = *ASSERT_NOT_NULL(type_of(node->lhs()));
  auto const &rhs_type = *ASSERT_NOT_NULL(type_of(node->rhs()));

  if (lhs_type.is<type::Array>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), &rhs_type));

    auto lval = EmitRef(node->lhs());
    ASSERT(lval.is_reg() == true);
    return builder().Index(type::Ptr(type_of(node->lhs())), lval.reg(), index);
  } else if (auto *buf_ptr_type = lhs_type.if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), &rhs_type));

    return builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (&lhs_type == type::ByteView) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), &rhs_type));
    auto str = EmitValue(node->lhs()).get<ir::RegOr<ir::String>>();
    if (str.is_reg()) {
      return builder().PtrIncr(str.reg(), index, type::Ptr(type::Nat8));
    } else {
      return builder().PtrIncr(str.value().addr(), index,
                               type::Ptr(type::Nat8));
    }
  } else if (auto *tup = lhs_type.if_as<type::Tuple>()) {
    auto maybe_val = Evaluate(type::Typed(node->rhs(), &rhs_type));
    if (not maybe_val) {
      diag().Consume(diagnostic::EvaluationFailure{
          .failure = maybe_val.error(),
          .range   = node->rhs()->range(),
      });
      return ir::Addr::Null();
    }
    auto index =
        builder().CastTo<int64_t>(type::Typed(*maybe_val, &rhs_type)).value();
    return builder().Field(EmitRef(node->lhs()), tup, index).get();
  }
  UNREACHABLE(*this, lhs_type.to_string());
}

}  // namespace compiler
