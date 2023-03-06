#include "semantic_analysis/byte_code/emitter.h"
#include "semantic_analysis/type_verification/casting.h"
#include "serialization/module_index.h"

namespace semantic_analysis {

template <typename T>
concept Numeric = std::is_arithmetic_v<T> and not std::same_as<T, bool>;

void ByteCodeValueEmitter::operator()(ast::ComparisonOperator const* node,
                                      FunctionData data) {
  base::PtrSpan exprs = node->exprs();
  std::span ops       = node->ops();
  auto op_iter        = ops.begin();
  Emit(exprs.front(), data);
  auto& f = data.function();
  f.AppendDuplicate();

  std::vector<jasmin::OpCodeRange> branches;
  branches.reserve(ops.size());

  core::Type prev_type = context().qualified_type(exprs.front()).type();
  for (auto expr_iter = exprs.begin() + 1; expr_iter != exprs.end();
       ++expr_iter, ++op_iter) {
    f.AppendSwap();
    f.AppendDrop(1);

    core::Type curr_type   = context().qualified_type(*expr_iter).type();
    core::Type common_type = CommonType(prev_type, curr_type, type_system());

    Emit(*expr_iter, data);
    // We compute the logical negation of what we're interested in because if
    // that's true (i.e., the condition we actually care about is false) we want
    // to jump via `JumpIf`.
    WithPrimitiveType(common_type, [&]<Numeric T> {
      switch (*op_iter) {
        case frontend::Operator::Gt:
          f.AppendDuplicateAt(0);
          f.AppendDuplicateAt(2);
          f.AppendLessThan<vm::Function::Consume, T>();
          f.AppendNot();
          break;
        case frontend::Operator::Lt:
          f.AppendLessThan<vm::Function::Append, T>();
          f.AppendNot();
          break;
        case frontend::Operator::Le:
          f.AppendDuplicateAt(0);
          f.AppendDuplicateAt(2);
          f.AppendLessThan<vm::Function::Consume, T>();
          break;
        case frontend::Operator::Ge:
          f.AppendLessThan<vm::Function::Append, T>();
          break;
        case frontend::Operator::Eq:
          f.AppendEqual<vm::Function::Append, T>();
          f.AppendNot();
          break;
        case frontend::Operator::Ne:
          f.AppendEqual<vm::Function::Append, T>();
          break;
        default: UNREACHABLE();
      }
      branches.push_back(f.AppendJumpIfWithPlaceholders());
    });
  }
  auto true_branch = f.AppendJumpWithPlaceholders();

  // Failure branch
  jasmin::OpCodeRange false_land(f.raw_instructions().size(), 0);
  f.AppendDrop(2);
  f.AppendPush(false);
  auto final_branch = f.AppendJumpWithPlaceholders();

  // Success branch
  jasmin::OpCodeRange true_land(f.raw_instructions().size(), 0);
  f.AppendDrop(2);
  f.AppendPush(true);
  jasmin::OpCodeRange final_land(f.raw_instructions().size(), 0);


  for (auto& branch : branches) {
    f.set_value(branch, 0, jasmin::OpCodeRange::Distance(false_land, branch));
  }
  f.set_value(true_branch, 0,
              jasmin::OpCodeRange::Distance(true_land, true_branch));
  f.set_value(final_branch, 0,
              jasmin::OpCodeRange::Distance(final_land, final_branch));
}

void ByteCodeStatementEmitter::operator()(ast::ComparisonOperator const* node,
                                          FunctionData data) {
}

}  // namespace semantic_analysis

