#include "ir/value/module_id.h"
#include "semantic_analysis/byte_code/emitter.h"
#include "semantic_analysis/type_verification/casting.h"

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
  f.append<jasmin::Duplicate>();

  std::vector<jasmin::OpCodeRange> branches;
  branches.reserve(ops.size());

  core::Type prev_type = context().qualified_type(exprs.front()).type();
  for (auto expr_iter = exprs.begin() + 1; expr_iter != exprs.end();
       ++expr_iter, ++op_iter) {
    f.append<jasmin::Swap>();
    f.append<jasmin::Drop>(1);

    core::Type curr_type   = context().qualified_type(*expr_iter).type();
    core::Type common_type = CommonType(prev_type, curr_type, type_system());

    Emit(*expr_iter, data);
    // We compute the logical negation of what we're interested in because if
    // that's true (i.e., the condition we actually care about is false) we want
    // to jump via `JumpIf`.
    WithPrimitiveType(common_type, [&]<Numeric T> {
      switch (*op_iter) {
        case frontend::Operator::Gt:
          f.append<jasmin::DuplicateAt>(0);
          f.append<jasmin::DuplicateAt>(2);
          f.append<jasmin::LessThan<T>>();
          f.append<jasmin::Not>();
          break;
        case frontend::Operator::Lt:
          f.append<jasmin::AppendLessThan<T>>();
          f.append<jasmin::Not>();
          break;
        case frontend::Operator::Le:
          f.append<jasmin::DuplicateAt>(0);
          f.append<jasmin::DuplicateAt>(2);
          f.append<jasmin::LessThan<T>>();
          break;
        case frontend::Operator::Ge:
          f.append<jasmin::AppendLessThan<T>>();
          break;
        case frontend::Operator::Eq:
          f.append<jasmin::AppendEqual<T>>();
          f.append<jasmin::Not>();
          break;
        case frontend::Operator::Ne:
          f.append<jasmin::AppendEqual<T>>();
          break;
        default: UNREACHABLE();
      }
      branches.push_back(f.append_with_placeholders<jasmin::JumpIf>());
    });
  }
  auto true_branch = f.append_with_placeholders<jasmin::Jump>();

  // Failure branch
  jasmin::OpCodeRange false_land(f.raw_instructions().size(), 0);
  f.append<jasmin::Drop>(2);
  f.append<jasmin::Push>(false);
  auto final_branch = f.append_with_placeholders<jasmin::Jump>();

  // Success branch
  jasmin::OpCodeRange true_land(f.raw_instructions().size(), 0);
  f.append<jasmin::Drop>(2);
  f.append<jasmin::Push>(true);
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

