#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

bool EmitAssignForAlwaysCopyTypes(Compiler &c, ast::Access const *node,
                                  type::Type t, ir::RegOr<ir::Addr> to) {
  if (auto const *enum_type = t.if_as<type::Enum>()) {
    c.builder().Store(ir::RegOr<type::Enum::underlying_type>(
                          *enum_type->EmitLiteral(node->member_name())),
                      to);
    return true;
  } else if (auto const *flags_type = t.if_as<type::Flags>()) {
    c.builder().Store(ir::RegOr<type::Flags::underlying_type>(
                          *flags_type->EmitLiteral(node->member_name())),
                      to);
    return true;
  } else if (auto const *s = t.if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      c.builder().Store(
          c.builder().Load<type::Slice::length_t>(
              c.current_block()->Append(type::SliceLengthInstruction{
                  .slice =
                      c.EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                  .result = c.builder().CurrentGroup()->Reserve(),
              })),
          to);
    } else if (node->member_name() == "data") {
      c.builder().Store(
          c.builder().Load<ir::Addr>(
              c.current_block()->Append(type::SliceDataInstruction{
                  .slice =
                      c.EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                  .result = c.builder().CurrentGroup()->Reserve(),
              })),
          to);

    } else {
      UNREACHABLE(node->member_name());
    }
    return true;
  } else {
    return false;
  }
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Access const *node) {
  LOG("Access", "Emitting value for %s", node->DebugString());
  type::QualType operand_qt = context().qual_type(node->operand());
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: return mod.ExportedValue(decl_ids[0]);
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_type(node);
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    return ir::Value(*enum_type->EmitLiteral(node->member_name()));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    return ir::Value(*flags_type->EmitLiteral(node->member_name()));
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      return ir::Value(builder().Load<type::Slice::length_t>(
          current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
              .result = builder().CurrentGroup()->Reserve(),
          })));

    } else if (node->member_name() == "data") {
      return ir::Value(builder().Load<ir::Addr>(
          current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
              .result = builder().CurrentGroup()->Reserve(),
          })));
    } else {
      UNREACHABLE(node->member_name());
    }
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const *a = t->if_as<type::Array>()) {
        if (node->member_name() == "length") {
          return ir::Value(a->length());
        } else if (node->member_name() == "element_type") {
          return ir::Value(a->data_type());
        } else {
          UNREACHABLE(node->member_name());
        }
      } else {
        UNREACHABLE(*t);
      }
    } else {
      UNREACHABLE(node->DebugString());
    }
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      return ir::Value(builder().PtrFix(EmitRef(node), node_qt.type()));
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

ir::Reg Compiler::EmitRef(ast::Access const *node) {
  auto op_qt = context().qual_type(node->operand());
  // TODO: This trick is good except that parameters look like references when
  // really they're by value for small types.
  size_t deref_count = (op_qt.quals() >= type::Quals::Ref())
                           ? size_t{0}
                           : static_cast<size_t>(-1);
  auto t = op_qt.type();

  auto ref = EmitRef(node->operand());
  if (ref.is_arg() and not t.get()->is_big()) { --deref_count; }

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
  type::QualType operand_qt = context().qual_type(node->operand());
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: {
        type::QualType node_qt = context().qual_type(node);
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                     type::Typed<ir::Value>(mod.ExportedValue(decl_ids[0]),
                                            node_qt.type()));
      }
        return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_type(node);
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), enum_type),
                 type::Typed<ir::Value>(
                     ir::Value(*enum_type->EmitLiteral(node->member_name())),
                     enum_type));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), flags_type),
                 type::Typed<ir::Value>(
                     ir::Value(*flags_type->EmitLiteral(node->member_name())),
                     flags_type));
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      EmitMoveInit(
          type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
          type::Typed<ir::Value>(
              ir::Value(builder().Load<type::Slice::length_t>(
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice =
                          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                      .result = builder().CurrentGroup()->Reserve(),
                  }))),
              type::U64));
    } else if (node->member_name() == "data") {
      EmitMoveInit(
          type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
          type::Typed<ir::Value>(
              ir::Value(builder().Load<ir::Addr>(
                  current_block()->Append(type::SliceDataInstruction{
                      .slice =
                          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                      .result = builder().CurrentGroup()->Reserve(),
                  }))),
              type::BufPtr(s->data_type())));
    } else {
      UNREACHABLE(node->member_name());
    }
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                   type::Typed<ir::Value>(ir::Value(builder().PtrFix(
                                              EmitRef(node), node_qt.type())),
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
  type::QualType operand_qt = context().qual_type(node->operand());
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1:
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                     type::Typed<ir::Value>(mod.ExportedValue(decl_ids[0]),
                                            operand_qt.type()));
        return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_type(node);
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                 type::Typed<ir::Value>(
                     ir::Value(*enum_type->EmitLiteral(node->member_name())),
                     enum_type));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                 type::Typed<ir::Value>(
                     ir::Value(*flags_type->EmitLiteral(node->member_name())),
                     flags_type));
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      EmitCopyInit(
          type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
          type::Typed<ir::Value>(
              ir::Value(builder().Load<type::Slice::length_t>(
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice =
                          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                      .result = builder().CurrentGroup()->Reserve(),
                  }))),
              type::U64));
    } else if (node->member_name() == "data") {
      EmitCopyInit(
          type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
          type::Typed<ir::Value>(
              ir::Value(builder().Load<ir::Addr>(
                  current_block()->Append(type::SliceDataInstruction{
                      .slice =
                          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
                      .result = builder().CurrentGroup()->Reserve(),
                  }))),
              type::BufPtr(s->data_type())));
    } else {
      UNREACHABLE(node->member_name());
    }
  } else {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      EmitCopyAssign(
          to[0], type::Typed<ir::Value>(
                     ir::Value(builder().PtrFix(EmitRef(node), node_qt.type())),
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
void Compiler::EmitMoveAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType operand_qt = context().qual_type(node->operand());
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1:
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                     type::Typed<ir::Value>(mod.ExportedValue(decl_ids[0]),
                                            operand_qt.type()));
        return;
      default: NOT_YET();
    }
  }

  if (not EmitAssignForAlwaysCopyTypes(*this, node, operand_qt.type(),
                                       *to[0])) {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      type::Type t = context().qual_type(node).type();
      EmitMoveAssign(to[0],
                     type::Typed<ir::Value>(
                         ir::Value(builder().PtrFix(EmitRef(node), t)), t));
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

void Compiler::EmitCopyAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  type::QualType operand_qt = context().qual_type(node->operand());
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = *ASSERT_NOT_NULL(
        EvaluateModuleWithCache(node->operand()).get<LibraryModule>());
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1:
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
                     type::Typed<ir::Value>(mod.ExportedValue(decl_ids[0]),
                                            operand_qt.type()));
        return;
      default: NOT_YET();
    }
  }

  if (not EmitAssignForAlwaysCopyTypes(*this, node, operand_qt.type(),
                                       *to[0])) {
    if (operand_qt.quals() >= type::Quals::Ref()) {
      type::Type t = context().qual_type(node).type();
      EmitMoveAssign(to[0],
                     type::Typed<ir::Value>(
                         ir::Value(builder().PtrFix(EmitRef(node), t)), t));
    } else {
      type::Typed<ir::RegOr<ir::Addr>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitCopyInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      EmitMoveAssign(
          to[0], builder().FieldValue(*temp, &struct_type,
                                      struct_type.index(node->member_name())));
    }
  }
}

}  // namespace compiler
