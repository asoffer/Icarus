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

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType qt = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
  ASSERT(qt.ok() == true);
  if (qt.type() == type::Module) {
    auto const &mod =
        *ASSERT_NOT_NULL(EvaluateModuleWithCache(node->operand()));
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO: should actually be an initialization, not assignment.
        Visit(qt.type(), *to[0],
              type::Typed(mod.ExportedValue(decls[0]), qt.type()),
              EmitMoveAssignTag{});
        return;
      default: NOT_YET();
    }
  }

  auto const &this_type = *ASSERT_NOT_NULL(data().qual_type(node))->type();
  if (auto const *enum_type = this_type.if_as<type::Enum>()) {
    // TODO: should actually be an initialization, not assignment.
    Visit(enum_type, *to[0],
          type::Typed(ir::Value(*enum_type->EmitLiteral(node->member_name())),
                      enum_type),
          EmitMoveAssignTag{});
  } else if (auto const *flags_type = this_type.if_as<type::Flags>()) {
    // TODO: should actually be an initialization, not assignment.
    Visit(flags_type, *to[0],
          type::Typed(ir::Value(*flags_type->EmitLiteral(node->member_name())),
                      flags_type),
          EmitMoveAssignTag{});
  } else if (&this_type == type::ByteView) {
    // TODO: should actually be an initialization, not assignment.
    ASSERT(node->member_name() == "length");
    Visit(type::ByteView, *to[0],
          type::Typed(
              ir::Value(builder().ByteViewLength(
                  EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
              type::ByteView),
          EmitMoveAssignTag{});
  } else {
    // TODO: should actually be an initialization, not assignment.
    Visit(to[0].type(), *to[0],
          type::Typed(
              ir::Value(builder().PtrFix(EmitRef(node).reg(), &this_type)),
              &this_type),
          EmitMoveAssignTag{});
  }
}

// TODO: Unit tests
void Compiler::EmitCopyInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType qt = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
  ASSERT(qt.ok() == true);
  if (qt.type() == type::Module) {
    auto const &mod =
        *ASSERT_NOT_NULL(EvaluateModuleWithCache(node->operand()));
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO: should actually be an initialization, not assignment.
        Visit(qt.type(), *to[0],
              type::Typed(mod.ExportedValue(decls[0]), qt.type()),
              EmitMoveAssignTag{});
        return;
      default: NOT_YET();
    }
  }

  auto const &this_type = *ASSERT_NOT_NULL(data().qual_type(node))->type();
  if (auto const *enum_type = this_type.if_as<type::Enum>()) {
    // TODO: should actually be an initialization, not assignment.
    Visit(enum_type, *to[0],
          type::Typed(ir::Value(*enum_type->EmitLiteral(node->member_name())),
                      enum_type),
          EmitMoveAssignTag{});
  } else if (auto const *flags_type = this_type.if_as<type::Flags>()) {
    // TODO: should actually be an initialization, not assignment.
    Visit(flags_type, *to[0],
          type::Typed(ir::Value(*flags_type->EmitLiteral(node->member_name())),
                      flags_type),
          EmitMoveAssignTag{});
  } else if (&this_type == type::ByteView) {
    // TODO: should actually be an initialization, not assignment.
    ASSERT(node->member_name() == "length");
    Visit(type::ByteView, *to[0],
          type::Typed(
              ir::Value(builder().ByteViewLength(
                  EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
              type::ByteView),
          EmitMoveAssignTag{});
  } else {
    // TODO: should actually be an initialization, not assignment.
    Visit(to[0].type(), *to[0],
          type::Typed(
              ir::Value(builder().PtrFix(EmitRef(node).reg(), &this_type)),
              &this_type),
          EmitMoveAssignTag{});
  }
}

// TODO: Unit tests
void Compiler::EmitAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType qt = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
  ASSERT(qt.ok() == true);
  if (qt.type() == type::Module) {
    auto const &mod =
        *ASSERT_NOT_NULL(EvaluateModuleWithCache(node->operand()));
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO: should actually be an initialization, not assignment.
        Visit(qt.type(), *to[0],
              type::Typed(mod.ExportedValue(decls[0]), qt.type()),
              EmitMoveAssignTag{});
        return;
      default: NOT_YET();
    }
  }

  auto const &this_type = *ASSERT_NOT_NULL(data().qual_type(node))->type();
  if (auto const *enum_type = this_type.if_as<type::Enum>()) {
    Visit(enum_type, *to[0],
          type::Typed(ir::Value(*enum_type->EmitLiteral(node->member_name())),
                      enum_type),
          EmitMoveAssignTag{});
  } else if (auto const *flags_type = this_type.if_as<type::Flags>()) {
    Visit(flags_type, *to[0],
          type::Typed(ir::Value(*flags_type->EmitLiteral(node->member_name())),
                      flags_type),
          EmitMoveAssignTag{});
  } else if (&this_type == type::ByteView) {
    ASSERT(node->member_name() == "length");
    Visit(type::ByteView, *to[0],
          type::Typed(
              ir::Value(builder().ByteViewLength(
                  EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
              type::ByteView),
          EmitMoveAssignTag{});
  } else {
    // TODO: should actually be an initialization, not assignment.
    Visit(to[0].type(), *to[0],
          type::Typed(
              ir::Value(builder().PtrFix(EmitRef(node).reg(), &this_type)),
              &this_type),
          EmitMoveAssignTag{});
  }
}

}  // namespace compiler
