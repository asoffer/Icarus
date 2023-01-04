#include "semantic_analysis/byte_code/emitter.h"
#include "semantic_analysis/type_verification/casting.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::BinaryOperator const *node,
                                      FunctionData data) {
  auto lhs_type          = context().qualified_type(&node->lhs()).type();
  auto rhs_type          = context().qualified_type(&node->rhs()).type();
  core::Type common_type = CommonType(lhs_type, rhs_type, type_system());
  auto& f = data.function();
  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Add: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      WithPrimitiveType(common_type,
                        [&]<jasmin::Addable T> { f.append<jasmin::Add<T>>(); });
    } break;
    case ast::BinaryOperator::Kind::Sub: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      WithPrimitiveType(common_type, [&]<jasmin::Subtractable T> {
        f.append<jasmin::Subtract<T>>();
      });
    } break;
    case ast::BinaryOperator::Kind::Mul: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      WithPrimitiveType(common_type, [&]<jasmin::Multiplicable T> {
        f.append<jasmin::Multiply<T>>();
      });
    } break;
    case ast::BinaryOperator::Kind::Div: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      WithPrimitiveType(common_type, [&]<jasmin::Divisible T> {
        f.append<jasmin::Divide<T>>();
      });
    } break;
    case ast::BinaryOperator::Kind::Mod: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      WithPrimitiveType(common_type,
                        [&]<jasmin::Modable T> { f.append<jasmin::Mod<T>>(); });
    } break;
    case ast::BinaryOperator::Kind::And: {
      Emit(&node->lhs(), data);
      f.append<jasmin::Duplicate>();
      f.append<jasmin::Not>();
      auto branch = f.append_with_placeholders<jasmin::JumpIf>();
      f.append<jasmin::Drop>(1);
      Emit(&node->rhs(), data);
      jasmin::OpCodeRange landing(f.raw_instructions().size(), 0);
      f.set_value(branch, 0, jasmin::OpCodeRange::Distance(landing, branch));
    } break;
    case ast::BinaryOperator::Kind::Or: {
      Emit(&node->lhs(), data);
      f.append<jasmin::Duplicate>();
      auto branch = f.append_with_placeholders<jasmin::JumpIf>();
      f.append<jasmin::Drop>(1);
      Emit(&node->rhs(), data);
      jasmin::OpCodeRange landing(f.raw_instructions().size(), 0);
      f.set_value(branch, 0, jasmin::OpCodeRange::Distance(landing, branch));
    } break;
    case ast::BinaryOperator::Kind::Xor: {
      Emit(&node->lhs(), data);
      Emit(&node->rhs(), data);
      f.append<jasmin::Xor>();
    } break;
    default: NOT_YET();
  }
}

void ByteCodeStatementEmitter::operator()(ast::BinaryOperator const *node,
                                          FunctionData data) {
  as<ByteCodeValueEmitter>().Emit(node, data);
  // TODO: Drop any unnecessary return values. Counting is more subtle than
  // this:
  data.function().append<jasmin::Drop>(context().qualified_types(node).size());
}

}  // namespace semantic_analysis

