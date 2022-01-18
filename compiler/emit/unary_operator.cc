#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/destroy.h"
#include "frontend/lex/operators.h"
#include "ir/instruction/arithmetic.h"
#include "type/interface/ir.h"
#include "type/pointer.h"
#include "type/struct.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::UnaryOperator const *node,
                            ir::PartialResultBuffer &out) {
  // TODO: user-defined-types
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Copy: {
      auto operand_type = context().qual_types(node->operand())[0].type();
      auto reg          = state().TmpAlloca(operand_type);
      EmitToBuffer(node->operand(), out);
      EmitCopyInit(type::Typed<ir::Reg>(reg, operand_type), out);
      out.pop_back();
      out.append(PtrFix(current(), reg, operand_type));
      return;
    } break;
    case ast::UnaryOperator::Kind::Destroy: {
      DestructionEmitter de(*this);
      de(context().typed(node->operand()).type(),
         EmitAs<ir::addr_t>(node->operand()));
      return;
    } break;
    case ast::UnaryOperator::Kind::Init:
      // TODO: Not entirely sure this is what the semantics ought to be.
    case ast::UnaryOperator::Kind::Move: {
      auto operand_type = context().qual_types(node->operand())[0].type();
      auto reg          = state().TmpAlloca(operand_type);
      EmitToBuffer(node->operand(), out);
      EmitMoveInit(type::Typed<ir::Reg>(reg, operand_type), out);
      out.pop_back();
      out.append(PtrFix(current(), reg, operand_type));
      return;
    } break;
    case ast::UnaryOperator::Kind::BufferPointer: {
      EmitToBuffer(node->operand(), out);
      auto value = out.back().get<type::Type>();
      out.pop_back();
      out.append(current_block()->Append(type::BufPtrInstruction{
          .operand = value,
          .result  = current().group->Reserve(),
      }));
      return;
    } break;
    case ast::UnaryOperator::Kind::Not: {
      auto operand_qt = context().qual_types(node->operand())[0];
      if (operand_qt.type() == type::Bool) {
        EmitToBuffer(node->operand(), out);
        auto value = out.back().get<bool>();
        out.pop_back();
        out.append(current_block()->Append(ir::NotInstruction{
            .operand = value, .result = current().group->Reserve()}));
        return;
      } else if (auto const *t = operand_qt.type().if_as<type::Flags>()) {
        out.append(current_block()->Append(type::XorFlagsInstruction{
            .lhs    = EmitAs<type::Flags::underlying_type>(node->operand()),
            .rhs    = t->All,
            .result = current().group->Reserve()}));
        return;
      } else {
        // TODO: Operator overloading
        NOT_YET();
      }
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      EmitToBuffer(node->operand(), out);
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, float, double>(
          context().qual_types(node->operand())[0].type(), [&]<typename T>() {
            auto value = out.back().get<T>();
            out.pop_back();
            out.append(current_block()->Append(ir::NegInstruction<T>{
                .operand = value,
                .result  = current().group->Reserve(),
            }));
          });
      return;
    } break;
    case ast::UnaryOperator::Kind::TypeOf:
      out.append(context().qual_types(node->operand())[0].type());
      return;
    case ast::UnaryOperator::Kind::Address:
      out.append(EmitRef(node->operand()));
      return;
    case ast::UnaryOperator::Kind::Pointer: {
      out.append(current_block()->Append(type::PtrInstruction{
          .operand = EmitAs<type::Type>(node->operand()),
          .result  = current().group->Reserve(),
      }));
      return;
    } break;
    case ast::UnaryOperator::Kind::At: {
      type::Type t = context().qual_types(node)[0].type();
      ir::PartialResultBuffer buffer;
      EmitToBuffer(node->operand(), buffer);
      out.append(current_block()->Append(ir::LoadInstruction{
          .type   = t,
          .addr   = buffer[0].get<ir::addr_t>(),
          .result = current().group->Reserve(),
      }));
      return;
    } break;
    case ast::UnaryOperator::Kind::BlockJump: {
      ir::PartialResultBuffer buffer;
      auto block = *EvaluateOrDiagnoseAs<ir::Block>(node->operand());
      auto *exit = current().group->AppendBlock();
      current_block()->set_jump(ir::JumpCmd::ToBlock(block, exit));
      current_block() = exit;
    } break;
    default: UNREACHABLE("Operator is ", static_cast<int>(node->kind()));
  }
}

void Compiler::EmitCopyInit(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init:
      EmitCopyInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Copy:
      EmitCopyInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Destroy: {
      DestructionEmitter de(*this);
      de(context().typed(node->operand()).type(),
         EmitAs<ir::addr_t>(node->operand()));
    } break;
    default: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(node, buffer);
      if (to.size() == 1) {
        EmitCopyAssign(
            to[0],
            type::Typed(buffer[0], context().qual_types(node)[0].type()));
      } else {
        NOT_YET();
      }
    } break;
  }
}

void Compiler::EmitMoveInit(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Copy:
      EmitCopyInit(node->operand(), to);
      break;
    default: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(node, buffer);
      if (to.size() == 1) {
        EmitMoveAssign(
            to[0],
            type::Typed(buffer[0], context().qual_types(node)[0].type()));
      } else {
        NOT_YET();
      }
    } break;
  }
}

ir::Reg Compiler::EmitRef(ast::UnaryOperator const *node) {
  ASSERT(node->kind() == ast::UnaryOperator::Kind::At);
  return EmitAs<ir::addr_t>(node->operand()).reg();
}

// TODO: Unit tests
void Compiler::EmitCopyAssign(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init: {
      EmitCopyInit(node->operand(), to);
    } break;
    case ast::UnaryOperator::Kind::Copy: {
      EmitCopyAssign(node->operand(), to);
    } break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveAssign(node->operand(), to);
      break;
    default: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(node, buffer);
      if (to.size() == 1) {
        EmitMoveAssign(
            to[0],
            type::Typed(buffer[0], context().qual_types(node)[0].type()));
      } else {
        NOT_YET();
      }
    } break;
  }
}

// TODO: Unit tests
void Compiler::EmitMoveAssign(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init: {
      EmitMoveInit(node->operand(), to);
    } break;
    case ast::UnaryOperator::Kind::Copy: {
      EmitCopyAssign(node->operand(), to);
    } break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveAssign(node->operand(), to);
      break;
    default: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(node, buffer);
      if (to.size() == 1) {
        EmitMoveAssign(
            to[0],
            type::Typed(buffer[0], context().qual_types(node)[0].type()));
      } else {
        NOT_YET();
      }
    } break;
  }
}

bool Compiler::PatternMatch(
    ast::UnaryOperator const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Pointer: {
      auto t = pmc.value.get<type::Type>(0);
      if (not t.is<type::Pointer>() or t.is<type::BufferPointer>()) {
        return false;
      }
      pmc.value.append(type::Type(t.as<type::Pointer>().pointee()));
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::BufferPointer: {
      auto t        = pmc.value.get<type::Type>(0);
      auto const *p = t.if_as<type::BufferPointer>();
      if (not p) { return false; }
      pmc.value.append(type::Type(p->pointee()));
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::Not: {
      bool b = pmc.value.get<bool>(0);
      pmc.value.append(not b);
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto t        = context().qual_types(node->operand())[0].type();
      auto const *p = t.if_as<type::Primitive>();
      if (not p) { return false; }
      p->Apply([&]<typename T>() {
        if constexpr (std::is_arithmetic_v<T>) {
          T x = pmc.value.template get<T>(0);
          pmc.value.append(-x);
        }
      });
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::Address: NOT_YET();
    case ast::UnaryOperator::Kind::Copy:
    case ast::UnaryOperator::Kind::Init:
    case ast::UnaryOperator::Kind::Move:
      return PatternMatch(node->operand(), pmc, bindings);
    default: UNREACHABLE();
  }
}

}  // namespace compiler
