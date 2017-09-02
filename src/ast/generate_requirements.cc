#include "ast.h"

#include "../ir/property.h"

extern IR::Val Evaluate(AST::Expression *expr);

namespace AST {
void ChainOp::GenerateRequirements() const {
  // TODO should there be a more direct way to compute this?
  auto *containing_function = scope_->ContainingFnScope()->fn_lit->ir_func;

  // TODO something generic
  // This is just a test. Probably needs a full rewrite
  ASSERT_EQ(2, exprs.size());
  if (exprs[0]->is<Identifier>()) {
    auto *id_addr = &exprs[0]->as<Identifier>().decl->addr;
    if (id_addr->value.is<IR::Register>()) {
      switch (ops[0]) {
      case Language::Operator::Lt: {
        auto upper_bound = Evaluate(exprs[1].get());
        if (upper_bound.value.is<i64>()) {
          containing_function->properties_[id_addr->value.as<IR::Register>()] =
              std::make_unique<IR::IntProperty>(
                  loc, std::numeric_limits<i64>::min(),
                  upper_bound.value.as<i64>() - 1);
        }
      } break;
      case Language::Operator::Le: {
        auto upper_bound = Evaluate(exprs[1].get());
        if (upper_bound.value.is<i64>()) {
          containing_function->properties_[id_addr->value.as<IR::Register>()] =
              std::make_unique<IR::IntProperty>(loc,
                                                std::numeric_limits<i64>::min(),
                                                upper_bound.value.as<i64>());
        }
      } break;
      case Language::Operator::Eq: NOT_YET();
      case Language::Operator::Ne: NOT_YET();
      case Language::Operator::Ge: {
        auto lower_bound = Evaluate(exprs[1].get());
        if (lower_bound.value.is<i64>()) {
          containing_function->properties_[id_addr->value.as<IR::Register>()] =
              std::make_unique<IR::IntProperty>(
                  loc, lower_bound.value.as<i64>(),
                  std::numeric_limits<i64>::max());
        }
      } break;
      case Language::Operator::Gt: {
        auto lower_bound = Evaluate(exprs[1].get());
        if (lower_bound.value.is<i64>()) {
          containing_function->properties_[id_addr->value.as<IR::Register>()] =
              std::make_unique<IR::IntProperty>(
                  loc, lower_bound.value.as<i64>() + 1,
                  std::numeric_limits<i64>::max());
        }
      } break;
      default: UNREACHABLE();
      }
    }
  }
  if (exprs[1]->is<Identifier>()) {
    LOG << exprs[1]->as<Identifier>().decl->addr.to_string();
  }
}
} // namespace AST
