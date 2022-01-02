#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "type/block.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

bool EmitAssignForAlwaysCopyTypes(Compiler &c, ast::Access const *node,
                                  type::Type t, ir::RegOr<ir::addr_t> to) {
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
                  .slice  = c.EmitAs<ir::addr_t>(node->operand()),
                  .result = c.builder().CurrentGroup()->Reserve(),
              })),
          to);
    } else if (node->member_name() == "data") {
      c.builder().Store(
          c.builder().Load<ir::addr_t>(
              c.current_block()->Append(type::SliceDataInstruction{
                  .slice  = c.EmitAs<ir::addr_t>(node->operand()),
                  .result = c.builder().CurrentGroup()->Reserve(),
              }),
              type::BufPtr(s->data_type())),
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

void Compiler::EmitToBuffer(ast::Access const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Access", "Emitting value for %s", node->DebugString());
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = importer()
                          .get(EvaluateModuleWithCache(node->operand()))
                          .as<CompiledModule>();
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: mod.context().LoadConstant(decl_ids[0], out); return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_types(node)[0];
  if (node_qt.type().is<type::Block>() or
      node_qt.type().is<type::Generic<type::Block>>()) {
    auto scope_context =
        *EvaluateOrDiagnoseAs<ir::ScopeContext>(node->operand());
    out.append(scope_context.find(node->member_name()));
  } else if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    out.append(*enum_type->EmitLiteral(node->member_name()));
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    out.append(*flags_type->EmitLiteral(node->member_name()));
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      out.append(builder().Load<type::Slice::length_t>(
          current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = builder().CurrentGroup()->Reserve(),
          })));
    } else if (node->member_name() == "data") {
      out.append(builder().Load<ir::addr_t>(
          current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = builder().CurrentGroup()->Reserve(),
          }),
          type::BufPtr(s->data_type())));
    } else {
      UNREACHABLE(node->member_name());
    }
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const *a = t->if_as<type::Array>()) {
        if (node->member_name() == "length") {
          out.append(a->length());
        } else if (node->member_name() == "element_type") {
          out.append(a->data_type());
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
      out.append(builder().PtrFix(EmitRef(node), node_qt.type()));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      builder().FieldValue(*temp, &struct_type,
                           struct_type.index(node->member_name()), out);
    }
  }
}

ir::Reg Compiler::EmitRef(ast::Access const *node) {
  auto op_qt = context().qual_types(node->operand())[0];
  // TODO: This trick is good except that parameters look like references when
  // really they're by value for small types.
  size_t deref_count = (op_qt.quals() >= type::Quals::Ref())
                           ? size_t{0}
                           : static_cast<size_t>(-1);
  auto t = op_qt.type();

  auto ref = EmitRef(node->operand());
  if (ref.is_arg() and not t.get()->is_big()) { --deref_count; }

  // TODO: Do not iterate through this twice.
  auto const *tp = t.if_as<type::Pointer>();
  while (tp) {
    t  = tp->pointee();
    tp = t.if_as<type::Pointer>();
    ++deref_count;
  }

  ir::RegOr<ir::addr_t> reg = (op_qt.quals() >= type::Quals::Ref())
                                  ? ref
                                  : EmitAs<ir::addr_t>(node->operand());
  {
    auto t  = op_qt.type();
    auto tp = t.if_as<type::Pointer>();
    for (size_t i = 0; i < deref_count; ++i) {
      reg = builder().Load<ir::addr_t>(reg, tp);
      t   = tp->pointee();
      tp  = t.if_as<type::Pointer>();
    }
  }

  auto const &struct_type = t.as<type::Struct>();
  return *builder().FieldRef(reg, &struct_type,
                             struct_type.index(node->member_name()));
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = importer()
                          .get(EvaluateModuleWithCache(node->operand()))
                          .as<CompiledModule>();
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: {
        type::QualType node_qt = context().qual_types(node)[0];
        ir::PartialResultBuffer buffer;
        mod.context().LoadConstant(decl_ids[0], buffer);
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
      }
        return;
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_types(node)[0];
  type::Type to_type;
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*enum_type->EmitLiteral(node->member_name()));
    EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), enum_type), buffer);
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*flags_type->EmitLiteral(node->member_name()));
    EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), flags_type), buffer);
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
     ir::PartialResultBuffer buffer;
     buffer.append(builder().Load<type::Slice::length_t>(
         current_block()->Append(type::SliceLengthInstruction{
             .slice  = EmitAs<ir::addr_t>(node->operand()),
             .result = builder().CurrentGroup()->Reserve(),
         })));
     EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
    } else if (node->member_name() == "data") {
     ir::PartialResultBuffer buffer;
     buffer.append(builder().Load<ir::addr_t>(
         current_block()->Append(type::SliceDataInstruction{
             .slice  = EmitAs<ir::addr_t>(node->operand()),
             .result = builder().CurrentGroup()->Reserve(),
         })));
     EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
    } else {
      UNREACHABLE(node->member_name());
    }
  } else {
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      buffer.append(builder().PtrFix(EmitRef(node), node_qt.type()));
      EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      ir::PartialResultBuffer buffer;
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      auto t                  = builder().FieldValue(
          *temp, &struct_type, struct_type.index(node->member_name()), buffer);
      EmitMoveAssign(to[0], type::Typed(buffer[0], t));
    }
  }
}

// TODO: Unit tests
void Compiler::EmitCopyInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = importer()
                          .get(EvaluateModuleWithCache(node->operand()))
                          .as<CompiledModule>();
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: {
        ir::PartialResultBuffer buffer;
        mod.context().LoadConstant(decl_ids[0], buffer);
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
        return;
      }
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_types(node)[0];
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*enum_type->EmitLiteral(node->member_name()));
    EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*flags_type->EmitLiteral(node->member_name()));
    EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      ir::PartialResultBuffer buffer;
      buffer.append(builder().Load<type::Slice::length_t>(
          current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = builder().CurrentGroup()->Reserve(),
          })));
      EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
    } else if (node->member_name() == "data") {
      ir::PartialResultBuffer buffer;
      buffer.append(builder().Load<ir::addr_t>(
          current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = builder().CurrentGroup()->Reserve(),
          })));
      EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
    } else {
      UNREACHABLE(node->member_name());
    }
  } else {
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      buffer.append(builder().PtrFix(EmitRef(node), node_qt.type()));
      EmitCopyAssign(to[0], type::Typed(buffer[0], node_qt.type()));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      auto t                  = builder().FieldValue(
          *temp, &struct_type, struct_type.index(node->member_name()), buffer);
      EmitMoveAssign(to[0], type::Typed(buffer[0], t));
    }
  }
}

// TODO: Unit tests
void Compiler::EmitMoveAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = importer()
                          .get(EvaluateModuleWithCache(node->operand()))
                          .as<CompiledModule>();
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: {
        ir::PartialResultBuffer buffer;
        mod.context().LoadConstant(decl_ids[0], buffer);
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
        return;
      }
      default: NOT_YET();
    }
  }

  if (not EmitAssignForAlwaysCopyTypes(*this, node, operand_qt.type(),
                                       *to[0])) {
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      type::Type t = context().qual_types(node)[0].type();
      buffer.append(builder().PtrFix(EmitRef(node), t));
      EmitMoveAssign(to[0], type::Typed(buffer[0], t));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      auto t                  = builder().FieldValue(
          *temp, &struct_type, struct_type.index(node->member_name()), buffer);
      EmitMoveAssign(to[0], type::Typed(buffer[0], t));
    }
  }
}

void Compiler::EmitCopyAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    auto const &mod = importer()
                          .get(EvaluateModuleWithCache(node->operand()))
                          .as<CompiledModule>();
    auto decl_ids = mod.scope().ExportedDeclarationIds(node->member_name());
    switch (decl_ids.size()) {
      case 0: NOT_YET();
      case 1: {
        ir::PartialResultBuffer buffer;
        mod.context().LoadConstant(decl_ids[0], buffer);
        EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
        return;
      }
      default: NOT_YET();
    }
  }

  if (not EmitAssignForAlwaysCopyTypes(*this, node, operand_qt.type(),
                                       *to[0])) {
    type::Type t = context().qual_types(node)[0].type();
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      buffer.append(builder().PtrFix(EmitRef(node), t));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          builder().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitCopyInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      auto t                  = builder().FieldValue(
          *temp, &struct_type, struct_type.index(node->member_name()), buffer);
    }
    EmitMoveAssign(to[0], type::Typed(buffer[0], t));
  }
}

bool Compiler::PatternMatch(
    ast::Access const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  UNREACHABLE(node->DebugString());
}

}  // namespace compiler
