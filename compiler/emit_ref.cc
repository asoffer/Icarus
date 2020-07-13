#include "compiler/compiler.h"

#include "ast/ast.h"

#include "ir/interpretter/evaluate.h"
#include "ir/value/addr.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

using ::matcher::InheritsFrom;

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = data().decls(node);
  ASSERT(decl_span.size() == 1u);
  return data().addr(decl_span[0]);
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Index const *node) {
  auto *lhs_type = type_of(node->lhs());
  auto *rhs_type = type_of(node->rhs());

  if (lhs_type->is<type::Array>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    auto lval = EmitRef(node->lhs());
    if (not lval.is_reg()) { NOT_YET(this, type_of(node)); }
    return builder().Index(type::Ptr(type_of(node->lhs())), lval.reg(), index);
  } else if (auto *buf_ptr_type = lhs_type->if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    return builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (lhs_type == type::ByteView) {
    // TODO interim until you remove string_view and replace it with Addr
    // entirely.
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));
    auto str = EmitValue(node->lhs()).get<ir::RegOr<ir::String>>();
    if (str.is_reg()) {
      return builder().PtrIncr(str.reg(), index, type::Ptr(type::Nat8));
    } else {
      return builder().PtrIncr(str.value().addr(), index,
                               type::Ptr(type::Nat8));
    }
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto maybe_val = Evaluate(type::Typed(node->rhs(), rhs_type));
    if (not maybe_val) { NOT_YET(); }
    auto index =
        builder().CastTo<int64_t>(type::Typed(*maybe_val, rhs_type)).value();
    return builder().Field(EmitRef(node->lhs()), tup, index).get();
  }
  UNREACHABLE(*this);
}

}  // namespace compiler
