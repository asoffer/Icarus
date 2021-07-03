#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "frontend/lex/operators.h"
#include "ir/value/value.h"
#include "type/interface/ir.h"
#include "type/pointer.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::UnaryOperator const *node,
                            base::untyped_buffer &out) {
  // TODO: user-defined-types
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Copy: {
      auto operand_type = context().qual_types(node->operand())[0].type();
      auto reg          = builder().TmpAlloca(operand_type);
      EmitToBuffer(node->operand(), out);
      EmitCopyInit(type::Typed<ir::Reg>(reg, operand_type), out);
      out.clear();
      FromValue(ir::Value(builder().PtrFix(reg, operand_type)), operand_type,
                out);
      return;
    } break;
    case ast::UnaryOperator::Kind::Destroy: {
      EmitDestroy(type::Typed<ir::Reg>(
          EmitAs<ir::addr_t>(node->operand()).reg(),
          context().qual_types(node->operand())[0].type()));
      return;
    } break;
    case ast::UnaryOperator::Kind::Init:
      // TODO: Not entirely sure this is what the semantics ought to be.
    case ast::UnaryOperator::Kind::Move: {
      auto operand_type = context().qual_types(node->operand())[0].type();
      auto reg = builder().TmpAlloca(operand_type);
      EmitToBuffer(node->operand(), out);
      EmitMoveInit(type::Typed<ir::Reg>(reg, operand_type), out);
      out.clear();
      FromValue(ir::Value(builder().PtrFix(reg, operand_type)), operand_type,
                out);
      return;
    } break;
    case ast::UnaryOperator::Kind::BufferPointer: {
      absl::Cleanup c = [b = state_.must_complete, this] {
        state_.must_complete = b;
      };
      state_.must_complete = false;

      EmitToBuffer(node->operand(), out);
      auto value = out.get<ir::RegOr<type::Type>>(0);
      out.clear();
      out.append(
          ir::RegOr<type::Type>(current_block()->Append(type::BufPtrInstruction{
              .operand = value,
              .result  = builder().CurrentGroup()->Reserve(),
          })));
      return;
    } break;
    case ast::UnaryOperator::Kind::Not: {
      auto operand_qt = context().qual_types(node->operand())[0];
      if (operand_qt.type() == type::Bool) {
        EmitToBuffer(node->operand(), out);
        auto value = out.get<ir::RegOr<bool>>(0);
        out.clear();
        out.append(ir::RegOr<bool>(builder().Not(value)));
        return;
      } else if (auto const *t = operand_qt.type().if_as<type::Flags>()) {
        out.append(ir::RegOr<type::Flags::underlying_type>(
            current_block()->Append(type::XorFlagsInstruction{
                .lhs    = EmitAs<type::Flags::underlying_type>(node->operand()),
                .rhs    = t->All,
                .result = builder().CurrentGroup()->Reserve()})));
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
            auto value = out.get<ir::RegOr<T>>(0);
            out.clear();
            out.append(ir::RegOr<T>(builder().Neg(value)));
          });
      return;
    } break;
    case ast::UnaryOperator::Kind::TypeOf:
      out.append(ir::RegOr<type::Type>(
          context().qual_types(node->operand())[0].type()));
      return;
    case ast::UnaryOperator::Kind::Address:
      out.append(ir::RegOr<ir::addr_t>(EmitRef(node->operand())));
      return;
    case ast::UnaryOperator::Kind::Pointer: {
      absl::Cleanup c = [b = state_.must_complete, this] {
        state_.must_complete = b;
      };
      state_.must_complete = false;
      out.append(
          ir::RegOr<type::Type>(current_block()->Append(type::PtrInstruction{
              .operand =
                  EmitAs<type::Type>(node->operand()),
              .result = builder().CurrentGroup()->Reserve(),
          })));
      return;
    } break;
    case ast::UnaryOperator::Kind::At: {
      type::Type t = context().qual_types(node)[0].type();
      EmitToBuffer(node->operand(), out);
      auto result = builder().Load(out.get<ir::RegOr<ir::addr_t>>(0), t);
      out.clear();
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          t, [&]<typename T>() { out.append(result.get<ir::RegOr<T>>()); });
      return;
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
    case ast::UnaryOperator::Kind::Destroy:
      EmitDestroy(type::Typed<ir::Reg>(
          EmitAs<ir::addr_t>(node->operand()).reg(),
          context().qual_types(node->operand())[0].type()));
      break;
    default: {
      auto from_val = EmitValue(node);
      auto from_qt  = *context().qual_types(node)[0];
      if (to.size() == 1) {
        EmitCopyAssign(to[0], type::Typed<ir::Value>(from_val, from_qt.type()));
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
      auto from_val = EmitValue(node);
      auto from_qt  = *context().qual_types(node)[0];
      if (to.size() == 1) {
        EmitMoveAssign(to[0], type::Typed<ir::Value>(from_val, from_qt.type()));
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
      auto from_val = EmitValue(node);
      auto from_qt  = *context().qual_types(node)[0];
      if (to.size() == 1) {
        EmitMoveAssign(to[0], type::Typed<ir::Value>(from_val, from_qt.type()));
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
      auto from_val = EmitValue(node);
      auto from_qt  = *context().qual_types(node)[0];
      if (to.size() == 1) {
        EmitMoveAssign(to[0], type::Typed<ir::Value>(from_val, from_qt.type()));
      } else {
        NOT_YET();
      }
    } break;
  }
}

bool Compiler::PatternMatch(
    ast::UnaryOperator const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Pointer: {
      auto t = pmc.value.get<type::Type>(0);
      if (not t.is<type::Pointer>() or t.is<type::BufferPointer>()) {
        return false;
      }
      pmc.value.set(0, type::Type(t.as<type::Pointer>().pointee()));
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::BufferPointer: {
      auto t        = pmc.value.get<type::Type>(0);
      auto const *p = t.if_as<type::BufferPointer>();
      if (not p) { return false; }
      pmc.value.set(0, type::Type(p->pointee()));
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::Not: {
      bool b = pmc.value.get<bool>(0);
      pmc.value.set(0, not b);
      return PatternMatch(node->operand(), pmc, bindings);
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto t        = context().qual_types(node->operand())[0].type();
      auto const *p = t.if_as<type::Primitive>();
      if (not p) { return false; }
      p->Apply([&]<typename T>() {
        if constexpr (std::is_arithmetic_v<T>) {
          T x = pmc.value.template get<T>(0);
          pmc.value.set(0, -x);
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
