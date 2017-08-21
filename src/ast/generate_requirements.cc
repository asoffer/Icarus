#include "ast.h"

extern IR::Val Evaluate(AST::Expression *expr);

namespace AST {
void ChainOp::GenerateRequirements() const {
  // TODO something generic
  // This is just a test. Probably needs a full rewrite
  ASSERT_EQ(2, exprs.size());
  if (exprs[0]->is<Identifier>()) {
    auto *id_addr = &ptr_cast<Identifier>(exprs[0].get())->decl->addr;
    if (id_addr->value.is<IR::Argument>()) {
      switch (ops[0]) {
      case Language::Operator::Lt: {
        // TODO actually, bound this by the upper bound for the evaluated
        // argument.
        auto upper_bound = Evaluate(exprs[1].get());
        if (upper_bound.value.is<i64>()) {
          id_addr->value.as<IR::Argument>().prop_ =
              std::make_unique<IR::UpperBound<i64>>(upper_bound.value.as<i64>());
        }
      } break;
      case Language::Operator::Le: NOT_YET();
      case Language::Operator::Eq: NOT_YET();
      case Language::Operator::Ne: NOT_YET();
      case Language::Operator::Ge: NOT_YET();
      case Language::Operator::Gt: NOT_YET();
      default: UNREACHABLE();
      }
    }
  }
  if (exprs[1]->is<Identifier>()) {
    LOG << ptr_cast<Identifier>(exprs[1].get())->decl->addr.to_string();
  }
}
} // namespace AST
