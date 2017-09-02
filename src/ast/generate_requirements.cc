#include "ast.h"

#include "../ir/property.h"

extern IR::Val Evaluate(AST::Expression *expr);

static void AddBound(const Cursor &loc, std::unique_ptr<IR::Property> *prop,
                     i32 low, i32 hi) {
  if (*prop == nullptr) {
    *prop = std::make_unique<IR::IntProperty>(loc, low, hi);
  } else if ((*prop)->is<IR::IntProperty>()) {
    auto *int_prop = &(*prop)->as<IR::IntProperty>();
    int_prop->min_ = std::max(int_prop->min_, low);
    int_prop->max_ = std::min(int_prop->max_, hi);
  }
}

namespace AST {
void ChainOp::GenerateRequirements() const {
  // TODO should there be a more direct way to compute this?
  auto &props = scope_->ContainingFnScope()->fn_lit->ir_func->properties_;

  for (size_t i = 0; i < exprs.size() - 1; ++i) {
    auto &lhs = exprs[i];
    auto &rhs = exprs[i + 1];
    if (lhs->is<Identifier>()) {
      if (rhs->is<Identifier>()) {
        NOT_YET();
      } else {
        auto *id_addr = &lhs->as<Identifier>().decl->addr;
        auto val      = Evaluate(rhs.get());
        if (!id_addr->value.is<IR::Register>()) { NOT_YET(); }
        auto *entry = &props[id_addr->value.as<IR::Register>()];

        switch (ops[i]) {
        case Language::Operator::Lt: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, std::numeric_limits<i32>::min(),
                     val.value.as<i32>() - 1);
          }
        } break;
        case Language::Operator::Le: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, std::numeric_limits<i32>::min(),
                     val.value.as<i32>());
          }
        } break;
        case Language::Operator::Eq: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>(), val.value.as<i32>());
          }
        } break;
        case Language::Operator::Ne: NOT_YET();
        case Language::Operator::Ge: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>(),
                     std::numeric_limits<i32>::max());
          }
        } break;
        case Language::Operator::Gt: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>() + 1,
                     std::numeric_limits<i32>::max());
          }
        } break;
        default: UNREACHABLE();
        }
      }
    } else {
      if (rhs->is<Identifier>()) {
        auto *id_addr = &rhs->as<Identifier>().decl->addr;
        auto val      = Evaluate(lhs.get());
        if (!id_addr->value.is<IR::Register>()) { NOT_YET(); }
        auto *entry = &props[id_addr->value.as<IR::Register>()];

        switch (ops[i]) {
        case Language::Operator::Lt: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>() + 1,
                     std::numeric_limits<i32>::max());
          }
        } break;
        case Language::Operator::Le: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>(),
                     std::numeric_limits<i32>::max());
          }
        } break;
        case Language::Operator::Eq: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, val.value.as<i32>(), val.value.as<i32>());
          }
        } break;
        case Language::Operator::Ne: NOT_YET();
        case Language::Operator::Ge: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, std::numeric_limits<i32>::min(),
                     val.value.as<i32>());
          }
        } break;
        case Language::Operator::Gt: {
          if (val.value.is<i32>()) {
            AddBound(loc, entry, std::numeric_limits<i32>::min(),
                     val.value.as<i32>() - 1);
          }
        } break;
        default: UNREACHABLE();
        }
      } else {
        NOT_YET();
      }
    }
  }
}
} // namespace AST
