#include "ast.h"

#include "../ir/property.h"

extern IR::Val Evaluate(AST::Expression *expr);

static void AddBound(std::unique_ptr<IR::Property> *prop, double low, double hi) {
  if (*prop == nullptr) {
    *prop = std::make_unique<IR::RealProperty>(low, hi);
  } else if ((*prop)->is<IR::RealProperty>()) {
    auto *real_prop = &(*prop)->as<IR::RealProperty>();
    real_prop->min_ = std::max(real_prop->min_, low);
    real_prop->max_ = std::min(real_prop->max_, hi);
  }
}

static void AddBound(std::unique_ptr<IR::Property> *prop, i32 low, i32 hi) {
  if (*prop == nullptr) {
    *prop = std::make_unique<IR::IntProperty>(low, hi);
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
            AddBound(entry, std::numeric_limits<i32>::lowest(),
                     val.value.as<i32>() - 1);
          } else if (val.value.is<double>()) {
            // Previous double "- 1"
            AddBound(entry, std::numeric_limits<double>::lowest(),
                     val.value.as<double>());
          }
        } break;
        case Language::Operator::Le: {
          if (val.value.is<i32>()) {
            AddBound(entry, std::numeric_limits<i32>::lowest(),
                     val.value.as<i32>());
           } else if (val.value.is<double>()) {
             AddBound(entry, std::numeric_limits<double>::lowest(),
                      val.value.as<double>());
          }
        } break;
        case Language::Operator::Eq: {
          if (val.value.is<i32>()) {
            AddBound(entry, val.value.as<i32>(), val.value.as<i32>());
          } else if (val.value.is<double>()) {
            AddBound(entry, val.value.as<double>(), val.value.as<double>());
          }
        } break;
        case Language::Operator::Ne: NOT_YET();
        case Language::Operator::Ge: {
          if (val.value.is<i32>()) {
            AddBound(entry, val.value.as<i32>(),
                     std::numeric_limits<i32>::max());
          } else if (val.value.is<double>()) {
            AddBound(entry, val.value.as<double>(),
                     std::numeric_limits<double>::max());
          }
        } break;
        case Language::Operator::Gt: {
          if (val.value.is<i32>()) {
            AddBound(entry, val.value.as<i32>() + 1,
                     std::numeric_limits<i32>::max());
          } else if (val.value.is<double>()) {
            // Next double "+ 1"
            AddBound(entry, val.value.as<double>(),
                     std::numeric_limits<double>::max());
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
            AddBound(entry, val.value.as<i32>() + 1,
                     std::numeric_limits<i32>::max());
          } else if (val.value.is<double>()) {
            // Next double "+ 1"
            AddBound(entry, val.value.as<double>(),
                     std::numeric_limits<double>::max());
          }
        } break;
        case Language::Operator::Le: {
          if (val.value.is<i32>()) {
            AddBound(entry, val.value.as<i32>(),
                     std::numeric_limits<i32>::max());
          } else if (val.value.is<double>()) {
            AddBound(entry, val.value.as<double>(),
                     std::numeric_limits<double>::max());
          }
        } break;
        case Language::Operator::Eq: {
          if (val.value.is<i32>()) {
            AddBound(entry, val.value.as<i32>(), val.value.as<i32>());
          } else if (val.value.is<double>()) {
            AddBound(entry, val.value.as<double>(), val.value.as<double>());
          }
        } break;
        case Language::Operator::Ne: NOT_YET();
        case Language::Operator::Ge: {
          if (val.value.is<i32>()) {
            AddBound(entry, std::numeric_limits<i32>::lowest(),
                     val.value.as<i32>());
          } else if (val.value.is<double>()) {
            AddBound(entry, std::numeric_limits<double>::lowest(),
                     val.value.as<double>());
          }
        } break;
        case Language::Operator::Gt: {
          if (val.value.is<i32>()) {
            AddBound(entry, std::numeric_limits<i32>::lowest(),
                     val.value.as<i32>() - 1);
          } else if (val.value.is<double>()) {
            // Previous double "- 1"
            AddBound(entry, std::numeric_limits<double>::lowest(),
                     val.value.as<double>());
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
