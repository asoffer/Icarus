#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Access const *node) {
  type::QualType qt = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
  ASSERT(qt.ok() == true);
  if (qt.type() == type::Module) {
    auto const &mod =
        *ASSERT_NOT_NULL(EvaluateModuleWithCache(node->operand()));
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: return mod.ExportedValue(decls[0]);
      default: NOT_YET();
    }
  }

  auto *this_type = ASSERT_NOT_NULL(data().qual_type(node))->type();
  if (auto const *enum_type = this_type->if_as<type::Enum>()) {
    return ir::Value(*enum_type->EmitLiteral(node->member_name()));
  } else if (auto const *flags_type = this_type->if_as<type::Flags>()) {
    return ir::Value(*flags_type->EmitLiteral(node->member_name()));
  } else if (this_type == type::ByteView) {
    ASSERT(node->member_name() == "length");
    return ir::Value(builder().ByteViewLength(
        EmitValue(node->operand()).get<ir::RegOr<ir::String>>()));
  } else {
    // TODO: Can this be an address?
    return ir::Value(builder().PtrFix(EmitRef(node).reg(), this_type));
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Access const *node) {
  auto op_qt         = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
  size_t deref_count = (op_qt.quals() >= type::Quals::Ref())
                           ? size_t{0}
                           : static_cast<size_t>(-1);
  auto const *t  = op_qt.type();
  auto const *tp = t->if_as<type::Pointer>();
  while (tp) {
    t = tp->pointee();
    tp = t->if_as<type::Pointer>();
    DEBUG_LOG()(deref_count);
    ++deref_count;
  }

  ir::Value reg = (op_qt.quals() >= type::Quals::Ref())
                      ? ir::Value(EmitRef(node->operand()))
                      : EmitValue(node->operand());
  for (size_t i = 0; i < deref_count; ++i) {
    reg = ir::Value(builder().Load<ir::Addr>(reg.get<ir::RegOr<ir::Addr>>()));
  }

  auto const &struct_type = t->as<type::Struct>();
  return builder()
      .Field(reg.get<ir::RegOr<ir::Addr>>(), &struct_type,
             struct_type.index(node->member_name()))
      .get();
}

}  // namespace compiler
