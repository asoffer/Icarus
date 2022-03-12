#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
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
    c.current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
        .value    = *enum_type->EmitLiteral(node->member_name()),
        .location = to,
    });
    return true;
  } else if (auto const *flags_type = t.if_as<type::Flags>()) {
    c.current_block()->Append(
        ir::StoreInstruction<type::Flags::underlying_type>{
            .value    = *flags_type->EmitLiteral(node->member_name()),
            .location = to,
        });
    return true;
  } else if (auto const *s = t.if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      c.current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
          .value    = c.current_block()->Append(ir::LoadInstruction{
              .type   = type::Slice::LengthType(),
              .addr   = c.current_block()->Append(type::SliceLengthInstruction{
                  .slice  = c.EmitAs<ir::addr_t>(node->operand()),
                  .result = c.current().subroutine->Reserve(),
              }),
              .result = c.current().subroutine->Reserve()}),
          .location = to,
      });
    } else if (node->member_name() == "data") {
      c.current_block()->Append(ir::StoreInstruction<ir::addr_t>{
          .value    = c.current_block()->Append(ir::LoadInstruction{
              .type   = type::BufPtr(s->data_type()),
              .addr   = c.current_block()->Append(type::SliceDataInstruction{
                  .slice  = c.EmitAs<ir::addr_t>(node->operand()),
                  .result = c.current().subroutine->Reserve(),
              }),
              .result = c.current().subroutine->Reserve()}),
          .location = to,
      });
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
    absl::Span symbol_infos =
        importer()
            .get(*EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand()))
            .Exported(node->member_name());
    switch (symbol_infos.size()) {
      case 0: NOT_YET();
      case 1: out.append(symbol_infos[0].value); return;
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
      out.append(current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve()}));
    } else if (node->member_name() == "data") {
      out.append(current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(s->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve()}));
    } else {
      UNREACHABLE(node->member_name());
    }
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const *a = t->if_as<type::Array>()) {
        if (node->member_name() == "length") {
          type::Typed<ir::RegOr<ir::addr_t>> temp(
              state().TmpAlloca(type::Integer), type::Integer);
          EmitCopyInit(node, absl::MakeConstSpan(&temp, 1));
          out.append(*temp);
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
      out.append(PtrFix(current(), EmitRef(node), node_qt.type()));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          state().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      type::Type t =
          struct_type.fields()[struct_type.index(node->member_name())].type;
      out.append(PtrFix(current(),
                        current_block()->Append(ir::StructIndexInstruction{
                            .addr  = *temp,
                            .index = struct_type.index(node->member_name()),
                            .struct_type = &struct_type,
                            .result      = current().subroutine->Reserve()}),
                        t));
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
      reg = current_block()->Append(ir::LoadInstruction{
          .type   = t,
          .addr   = reg,
          .result = current().subroutine->Reserve(),
      });
      t   = tp->pointee();
      tp  = t.if_as<type::Pointer>();
    }
  }

  auto const &struct_type = t.as<type::Struct>();
  return current_block()->Append(ir::StructIndexInstruction{
      .addr        = reg,
      .index       = struct_type.index(node->member_name()),
      .struct_type = &struct_type,
      .result      = current().subroutine->Reserve()});
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    absl::Span symbol_infos =
        importer()
            .get(*EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand()))
            .Exported(node->member_name());
    switch (symbol_infos.size()) {
      case 0: NOT_YET();
      case 1: {
        MoveInitializationEmitter emitter(*this);
        emitter(to[0], ir::PartialResultBuffer(symbol_infos[0].value));
        return;
      }
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_types(node)[0];
  type::Type to_type;
  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*enum_type->EmitLiteral(node->member_name()));
    MoveInitializationEmitter emitter(*this);
    emitter(enum_type, *to[0], buffer);
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*flags_type->EmitLiteral(node->member_name()));
    MoveInitializationEmitter emitter(*this);
    emitter(flags_type, *to[0], buffer);
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      ir::PartialResultBuffer buffer;
      buffer.append(current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }));
      MoveInitializationEmitter emitter(*this);
      emitter(to[0], buffer);
    } else if (node->member_name() == "data") {
      ir::PartialResultBuffer buffer;
      buffer.append(current_block()->Append(ir::LoadInstruction{
          .type   = to[0].type(),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }));
      MoveInitializationEmitter emitter(*this);
      emitter(to[0], buffer);
    } else {
      UNREACHABLE(node->member_name());
    }
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const *a = t->if_as<type::Array>()) {
        if (node->member_name() == "length") {
          auto *addr = &const_cast<ir::Integer &>(a->length());
          current_block()->Append(
              ir::CompileTime<ir::Action::CopyInit, ir::Integer>{
                  .from = reinterpret_cast<ir::addr_t>(addr), .to = *to[0]});
        } else if (node->member_name() == "element_type") {
          ir::PartialResultBuffer buffer;
          buffer.append(a->data_type());
          MoveInitializationEmitter emitter(*this);
          emitter(to[0], buffer);
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
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      buffer.append(PtrFix(current(), EmitRef(node), node_qt.type()));
      MoveInitializationEmitter emitter(*this);
      emitter(to[0], buffer);
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          state().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      ir::PartialResultBuffer buffer;
      auto const &struct_type = operand_qt.type().as<type::Struct>();

      type::Type t =
          struct_type.fields()[struct_type.index(node->member_name())].type;
      buffer.append(PtrFix(current(),
                           current_block()->Append(ir::StructIndexInstruction{
                               .addr  = *temp,
                               .index = struct_type.index(node->member_name()),
                               .struct_type = &struct_type,
                               .result      = current().subroutine->Reserve()}),
                           t));
      MoveAssignmentEmitter emitter(*this);
      emitter(to[0], type::Typed(buffer[0], t));
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
    absl::Span symbol_infos =
        importer()
            .get(*EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand()))
            .Exported(node->member_name());
    switch (symbol_infos.size()) {
      case 0: NOT_YET();
      case 1: {
        CopyInitializationEmitter emitter(*this);
        emitter(to[0], ir::PartialResultBuffer(symbol_infos[0].value));
        return;
      }
      default: NOT_YET();
    }
  }

  type::QualType node_qt = context().qual_types(node)[0];

  if (auto const *enum_type = node_qt.type().if_as<type::Enum>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*enum_type->EmitLiteral(node->member_name()));
    CopyInitializationEmitter emitter(*this);
    emitter(to[0], buffer);
  } else if (auto const *flags_type = node_qt.type().if_as<type::Flags>()) {
    ir::PartialResultBuffer buffer;
    buffer.append(*flags_type->EmitLiteral(node->member_name()));
    CopyInitializationEmitter emitter(*this);
    emitter(to[0], buffer);
  } else if (auto const *s = operand_qt.type().if_as<type::Slice>()) {
    if (node->member_name() == "length") {
      ir::PartialResultBuffer buffer;
      buffer.append(current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }));
      CopyInitializationEmitter emitter(*this);
      emitter(to[0], buffer);
    } else if (node->member_name() == "data") {
      ir::PartialResultBuffer buffer;
      buffer.append(current_block()->Append(ir::LoadInstruction{
          .type   = to[0].type(),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->operand()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }));
      CopyInitializationEmitter emitter(*this);
      emitter(to[0], buffer);
    } else {
      UNREACHABLE(node->member_name());
    }
  } else if (operand_qt == type::QualType::Constant(type::Type_)) {
    if (auto t = EvaluateOrDiagnoseAs<type::Type>(node->operand())) {
      if (type::Array const *a = t->if_as<type::Array>()) {
        if (node->member_name() == "length") {
          auto *addr = &const_cast<ir::Integer &>(a->length());
          current_block()->Append(
              ir::CompileTime<ir::Action::CopyInit, ir::Integer>{
                  .from = reinterpret_cast<ir::addr_t>(addr), .to = *to[0]});
        } else if (node->member_name() == "element_type") {
          ir::PartialResultBuffer buffer;
          buffer.append(a->data_type());
          MoveInitializationEmitter emitter(*this);
          emitter(to[0], buffer);
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
    ir::PartialResultBuffer buffer;
    if (operand_qt.quals() >= type::Quals::Ref()) {
      buffer.append(PtrFix(current(), EmitRef(node), node_qt.type()));
      CopyAssignmentEmitter emitter(*this);
      emitter(to[0], type::Typed(buffer[0], node_qt.type()));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          state().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      type::Type t =
          struct_type.fields()[struct_type.index(node->member_name())].type;
      buffer.append(PtrFix(current(),
                           current_block()->Append(ir::StructIndexInstruction{
                               .addr  = *temp,
                               .index = struct_type.index(node->member_name()),
                               .struct_type = &struct_type,
                               .result      = current().subroutine->Reserve()}),
                           t));
      MoveAssignmentEmitter emitter(*this);
      emitter(to[0], type::Typed(buffer[0], node_qt.type()));
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
    absl::Span symbol_infos =
        importer()
            .get(*EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand()))
            .Exported(node->member_name());
    switch (symbol_infos.size()) {
      case 0: NOT_YET();
      case 1: {
        MoveAssignmentEmitter emitter(*this);
        emitter(to[0], type::Typed<ir::PartialResultRef>(
                           symbol_infos[0].value[0],
                           symbol_infos[0].qualified_type.type()));
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
      buffer.append(PtrFix(current(), EmitRef(node), t));
      MoveAssignmentEmitter emitter(*this);
      emitter(to[0], type::Typed(buffer[0], t));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          state().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitMoveInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      type::Type t =
          struct_type.fields()[struct_type.index(node->member_name())].type;
      buffer.append(PtrFix(current(),
                           current_block()->Append(ir::StructIndexInstruction{
                               .addr  = *temp,
                               .index = struct_type.index(node->member_name()),
                               .struct_type = &struct_type,
                               .result      = current().subroutine->Reserve()}),
                           t));
      MoveAssignmentEmitter emitter(*this);
      emitter(to[0], type::Typed(buffer[0], t));
    }
  }
}

void Compiler::EmitCopyAssign(
    ast::Access const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  type::QualType operand_qt = context().qual_types(node->operand())[0];
  ASSERT(operand_qt.ok() == true);
  if (operand_qt.type() == type::Module) {
    absl::Span symbol_infos =
        importer()
            .get(*EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand()))
            .Exported(node->member_name());

    switch (symbol_infos.size()) {
      case 0: NOT_YET();
      case 1: {
        CopyAssignmentEmitter emitter(*this);
        emitter(to[0], type::Typed<ir::PartialResultRef>(
                           symbol_infos[0].value[0],
                           symbol_infos[0].qualified_type.type()));
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
      buffer.append(PtrFix(current(), EmitRef(node), t));
    } else {
      type::Typed<ir::RegOr<ir::addr_t>> temp(
          state().TmpAlloca(operand_qt.type()), operand_qt.type());
      EmitCopyInit(node->operand(), absl::MakeConstSpan(&temp, 1));
      auto const &struct_type = operand_qt.type().as<type::Struct>();
      type::Type t =
          struct_type.fields()[struct_type.index(node->member_name())].type;
      buffer.append(PtrFix(current(),
                           current_block()->Append(ir::StructIndexInstruction{
                               .addr  = *temp,
                               .index = struct_type.index(node->member_name()),
                               .struct_type = &struct_type,
                               .result      = current().subroutine->Reserve()}),
                           t));
    }
    MoveAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(buffer[0], t));
  }
}

bool Compiler::PatternMatch(
    ast::Access const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  UNREACHABLE(node->DebugString());
}

}  // namespace compiler
