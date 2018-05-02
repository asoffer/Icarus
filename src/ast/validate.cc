#include "../type/all.h"
#include "ast.h"
#include "ast/terminal.h"
#include "context.h"
#include "ir/val.h"

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
void Binop::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  lhs->Validate(ctx);
  rhs->Validate(ctx);
}

void Access::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  operand->Validate(ctx);
}

void ChainOp::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void CommaList::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &expr : exprs) { expr->Validate(ctx); }
}


}  // namespace AST
#undef HANDLE_CYCLIC_DEPENDENCIES
