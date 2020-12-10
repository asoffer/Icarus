#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Access const *node) {
  LOG("Access", "Emitting value for %s", node->DebugString());
  type::QualType operand_qt =
      *ASSERT_NOT_NULL(context().qual_type(node->operand()));
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: return mod.ExportedValue(decls[0]);
      default: NOT_YET();
    }
  }

  type::QualType node_qt = *ASSERT_NOT_NULL(context().qual_type(node));
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    return ir::Value(*enum_type->EmitLiteral(node->member_name()));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    return ir::Value(*flags_type->EmitLiteral(node->member_name()));
  } else if (operand_qt.type() == type::ByteView) {
    ASSERT(node->member_name() == "length");
    return ir::Value(builder().ByteViewLength(
        EmitValue(node->operand()).get<ir::RegOr<ir::String>>()));
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const * a = t->if_as<type::Array>()) {
        ASSERT(node->member_name() == "length");
        return ir::Value(a->length());
      } else {
        UNREACHABLE(*t);
      }
    } else {
      UNREACHABLE(node->DebugString());
    }
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      // TODO: Can this be an address?
      return ir::Value(builder().PtrFix(EmitRef(node).reg(), node_qt.type()));
    } else {
      type::Typed<ir::RegOr<ir::Addr>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      return *builder().FieldValue(*temp, &struct_type,
                                   struct_type.index(node->member_name()));
    }
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Access const *node) {
  auto op_qt = *ASSERT_NOT_NULL(context().qual_type(node->operand()));
  // TODO: This trick is good except that parameters look like references when
  // really they're by value for small types.
  size_t deref_count = (op_qt.quals() >= type::Quals::Ref())
                           ? size_t{0}
                           : static_cast<size_t>(-1);
  auto t = op_qt.type();

  auto ref = EmitRef(node->operand());
  if (ref.is_reg() and ref.reg().is_arg() and not t.get()->is_big()) {
    --deref_count;
  }

  auto const *tp = t.if_as<type::Pointer>();
  while (tp) {
    t  = tp->pointee();
    tp = t.if_as<type::Pointer>();
    ++deref_count;
  }

  ir::Value reg = (op_qt.quals() >= type::Quals::Ref())
                      ? ir::Value(ref)
                      : EmitValue(node->operand());
  for (size_t i = 0; i < deref_count; ++i) {
    reg = ir::Value(builder().Load<ir::Addr>(reg.get<ir::RegOr<ir::Addr>>()));
  }

  auto const &struct_type = t.as<type::Struct>();
  return *builder().FieldRef(reg.get<ir::RegOr<ir::Addr>>(), &struct_type,
                             struct_type.index(node->member_name()));
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType operand_qt =
      *ASSERT_NOT_NULL(context().qual_type(node->operand()));
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: {
        // TODO: should actually be an initialization, not assignment.
        type::QualType node_qt = *ASSERT_NOT_NULL(context().qual_type(node));
        EmitMoveAssign(to[0], type::Typed<ir::Value>(
                                  mod.ExportedValue(decls[0]), node_qt.type()));
      }
        return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = *ASSERT_NOT_NULL(context().qual_type(node));
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    // TODO: should actually be an initialization, not assignment.
    EmitMoveAssign(type::Typed<ir::RegOr<ir::Addr>>(*to[0], enum_type),
                   type::Typed<ir::Value>(
                       ir::Value(*enum_type->EmitLiteral(node->member_name())),
                       enum_type));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    // TODO: should actually be an initialization, not assignment.
    EmitMoveAssign(type::Typed<ir::RegOr<ir::Addr>>(*to[0], flags_type),
                   type::Typed<ir::Value>(
                       ir::Value(*flags_type->EmitLiteral(node->member_name())),
                       flags_type));
  } else if (operand_qt.type() == type::ByteView) {
    // TODO: should actually be an initialization, not assignment.
    ASSERT(node->member_name() == "length");
    EmitMoveAssign(
        to[0],
        type::Typed<ir::Value>(
            ir::Value(builder().ByteViewLength(
                EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
            type::U64));
  } else {
    // TODO: should actually be an initialization, not assignment.
    if (operand_qt.quals() >= type::Quals::Ref()) {
      EmitMoveAssign(
          to[0],
          type::Typed<ir::Value>(
              ir::Value(builder().PtrFix(EmitRef(node).reg(), node_qt.type())),
              node_qt.type()));

    } else {
      type::Typed<ir::RegOr<ir::Addr>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      EmitMoveAssign(
          to[0], builder().FieldValue(*temp, &struct_type,
                                      struct_type.index(node->member_name())));
    }
  }
}

// TODO: Unit tests
void Compiler::EmitCopyInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType operand_qt =
      *ASSERT_NOT_NULL(context().qual_type(node->operand()));
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO: should actually be an initialization, not assignment.
        EmitMoveAssign(to[0],
                       type::Typed<ir::Value>(mod.ExportedValue(decls[0]),
                                              operand_qt.type()));
        return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = *ASSERT_NOT_NULL(context().qual_type(node));
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    // TODO: should actually be an initialization, not assignment.
    EmitCopyAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(*enum_type->EmitLiteral(node->member_name())),
                       enum_type));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    // TODO: should actually be an initialization, not assignment.
    EmitCopyAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(*flags_type->EmitLiteral(node->member_name())),
                       flags_type));
  } else if (operand_qt.type() == type::ByteView) {
    // TODO: should actually be an initialization, not assignment.
    ASSERT(node->member_name() == "length");
    EmitCopyAssign(
        to[0],
        type::Typed<ir::Value>(
            ir::Value(builder().ByteViewLength(
                EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
            type::U64));
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      EmitCopyAssign(
          to[0],
          type::Typed<ir::Value>(
              ir::Value(builder().PtrFix(EmitRef(node).reg(), node_qt.type())),
              node_qt.type()));

    } else {
      type::Typed<ir::RegOr<ir::Addr>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      EmitMoveAssign(
          to[0], builder().FieldValue(*temp, &struct_type,
                                      struct_type.index(node->member_name())));
    }
  }
}

// TODO: Unit tests
void Compiler::EmitAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType operand_qt =
      *ASSERT_NOT_NULL(context().qual_type(node->operand()));
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decls = mod.ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO: should actually be an initialization, not assignment.
        EmitMoveAssign(to[0],
                       type::Typed<ir::Value>(mod.ExportedValue(decls[0]),
                                              operand_qt.type()));
        return;
      default: NOT_YET();
    }
  }

  if (auto const *enum_type = operand_qt.type().if_as<type::Enum>()) {
    EmitMoveAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(*enum_type->EmitLiteral(node->member_name())),
                       enum_type));
  } else if (auto const *flags_type = operand_qt.type().if_as<type::Flags>()) {
    EmitMoveAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(*flags_type->EmitLiteral(node->member_name())),
                       flags_type));
  } else if (operand_qt.type() == type::ByteView) {
    ASSERT(node->member_name() == "length");
    EmitMoveAssign(
        to[0],
        type::Typed<ir::Value>(
            ir::Value(builder().ByteViewLength(
                EmitValue(node->operand()).get<ir::RegOr<ir::String>>())),
            type::U64));
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
    type::Type t = context().qual_type(node)->type();
    EmitMoveAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(builder().PtrFix(EmitRef(node).reg(), t)), t));
    } else {
      type::Typed<ir::RegOr<ir::Addr>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      EmitMoveAssign(
          to[0], builder().FieldValue(*temp, &struct_type,
                                      struct_type.index(node->member_name())));
    }
  }
}

}  // namespace compiler
