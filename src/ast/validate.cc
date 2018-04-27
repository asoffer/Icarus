#include "ast.h"
#include "stages.h"
#include "../type/all.h"
#include "context.h"

#define STARTING_CHECK                                                         \
  do {                                                                         \
    if (stage_range_.high < ValidatedStage) { return; }                        \
    if (stage_range_.low >= ValidatedStage) { return; }                        \
    stage_range_.low = ValidatedStage;                                         \
  } while (false)

// TODO macro duplicated in verifytypes
#define HANDLE_CYCLIC_DEPENDENCIES                                             \
  do {                                                                         \
    if (ctx->cyc_dep_vec_ == nullptr) { break; }                               \
    if constexpr (std::is_same_v<decltype(this), Identifier *>) {              \
      auto *this_as_id = reinterpret_cast<Identifier *>(this);                 \
      if (!ctx->cyc_dep_vec_->empty() &&                                       \
          this_as_id == ctx->cyc_dep_vec_->front()) {                          \
        ctx->cyc_dep_vec_ = nullptr;                                           \
      } else {                                                                 \
        ctx->cyc_dep_vec_->push_back(this_as_id);                              \
      }                                                                        \
    } else if constexpr (std::is_same_v<decltype(this), Declaration *>) {      \
      auto *this_as_id =                                                       \
          reinterpret_cast<Declaration *>(this)->identifier.get();             \
      if (!ctx->cyc_dep_vec_->empty() &&                                       \
          this_as_id == ctx->cyc_dep_vec_->front()) {                          \
        ctx->cyc_dep_vec_ = nullptr;                                           \
      } else {                                                                 \
        ctx->cyc_dep_vec_->push_back(this_as_id);                              \
      }                                                                        \
    }                                                                          \
    type = type::Err;                                                          \
    limit_to(StageRange::Nothing());                                           \
    return;                                                                    \
  } while (false)

namespace AST {
void GenericFunctionLiteral::Validate(Context *ctx) {}
void Terminal::Validate(Context *ctx) {}
void Hole::Validate(Context *ctx) {}
void CodeBlock::Validate(Context *ctx) {}
void Identifier::Validate(Context *ctx) {}
void Import::Validate(Context *ctx) {}
void Jump::Validate(Context *ctx) {}

void Binop::Validate(Context *ctx) {
  STARTING_CHECK;
  lhs->Validate(ctx);
  rhs->Validate(ctx);
}

void Call::Validate(Context *ctx) {
  STARTING_CHECK;
  fn_->Validate(ctx);
  args_.Apply([ctx](auto &arg) { arg->Validate(ctx); });
}

void Declaration::Validate(Context *ctx) {
  STARTING_CHECK;
  if (type_expr) { type_expr->Validate(ctx); }
  if (init_val) { init_val->Validate(ctx); }
}

void InDecl::Validate(Context *ctx) {
  STARTING_CHECK;
  container->Validate(ctx);
}

void Statements::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &stmt : content_) { stmt->Validate(ctx); }
}

void Unop::Validate(Context *ctx) {
  STARTING_CHECK;
  operand->Validate(ctx);
}

void Access::Validate(Context *ctx) {
  STARTING_CHECK;
  operand->Validate(ctx);
}

void ChainOp::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void CommaList::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void ArrayLiteral::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &elem : elems) { elem->Validate(ctx); }
}

void ArrayType::Validate(Context *ctx) {
  STARTING_CHECK;
  length->Validate(ctx);
  data_type->Validate(ctx);
}

void FunctionLiteral::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &in : inputs) { in->Validate(ctx); }
  for (auto &out : outputs) { out->Validate(ctx); }

  // NOTE! Type verifcation on statements first!
  statements->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;

  std::vector<const Expression *> rets;
  statements->ExtractReturns(&rets);
  statements->Validate(ctx);
  std::set<const type::Type*> types;
  for (auto* expr : rets) { types.insert(expr->type); }

  std::vector<const type::Type *> input_type_vec, output_type_vec;
  input_type_vec.reserve(inputs.size());
  for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

  if (return_type_inferred_) {
    switch (types.size()) {
      case 0: type = type::Func(std::move(input_type_vec), {}); break;
      case 1: {
        auto *one_type = *types.begin();
        if (one_type->is<type::Tuple>()) {
          const auto &entries = one_type->as<type::Tuple>().entries_;
          for (auto *entry : entries) {
            outputs.push_back(
                std::make_unique<Terminal>(TextSpan(), IR::Val::Type(entry)));
          }
          type = type::Func(std::move(input_type_vec), entries);
        } else {
          outputs.push_back(
              std::make_unique<Terminal>(TextSpan(), IR::Val::Type(one_type)));
          type = type::Func(std::move(input_type_vec), {one_type});
        }
      } break;
      default: {
        // Note: this feels impossible, but it is possible if we allow scopes to
        // both evaluate to values and return.
        NOT_YET();
      } break;
    }
  } else {
    const auto &outs = type->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        for (auto *expr : rets) {
          ctx->error_log_.NoReturnTypes(expr);
          limit_to(StageRange::NoEmitIR());
        }
      } break;
      case 1: {
        for (auto *expr : rets) {
          if (expr->type == outs[0]) { continue; }
          limit_to(StageRange::NoEmitIR());
          ctx->error_log_.ReturnTypeMismatch(outs[0], expr);
        }
      } break;
      default: {
        for (auto *expr : rets) {
          if (expr->type->is<type::Tuple>()) {
            const auto &tup_entries = expr->type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log_.ReturningWrongNumber(expr, outs.size());
              limit_to(StageRange::NoEmitIR());
            } else {
              for(size_t i = 0; i < tup_entries.size(); ++i) {
                // TODO compare with Join rather than direct comparison
                if (tup_entries AT(i) != outs AT(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  ctx->error_log_.IndexedReturnTypeMismatch(outs AT(i), expr,
                                                            i);
                  limit_to(StageRange::NoEmitIR());
                }
              }
            }
          } else {
            ctx->error_log_.ReturningWrongNumber(expr, outs.size());
            limit_to(StageRange::NoEmitIR());
          }
        }
      } break;
    }
  }
}

void For::Validate(Context *ctx) {
  STARTING_CHECK;
  // TODO
}

void ScopeNode::Validate(Context *ctx) {
  STARTING_CHECK;
  // TODO
}

void ScopeLiteral::Validate(Context *ctx) {
  STARTING_CHECK;
  // TODO
}

void StructLiteral::Validate(Context *ctx) {
  STARTING_CHECK;
  for (auto &f : fields_) { f->Validate(ctx); }
}
}  // namespace AST
#undef STARTING_CHECK
#undef HANDLE_CYCLIC_DEPENDENCIES
