#ifndef ICARUS_AST_VERIFY_MACROS_H
#define ICARUS_AST_VERIFY_MACROS_H

#include "ast/declaration.h"
#include "ast/stages.h"
#include "context.h"

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
      auto *this_as_decl =                                                     \
          reinterpret_cast<Declaration *>(this)->identifier.get();             \
      if (!ctx->cyc_dep_vec_->empty() &&                                       \
          this_as_decl == ctx->cyc_dep_vec_->front()) {                        \
        ctx->cyc_dep_vec_ = nullptr;                                           \
      } else {                                                                 \
        ctx->cyc_dep_vec_->push_back(this_as_decl);                            \
      }                                                                        \
    }                                                                          \
    limit_to(StageRange::Nothing());                                           \
    return nullptr;                                                            \
  } while (false)

// TODO this is probably not necessary if we treat this as a relatively pure
// tree? We could likely just store the type of a declaration and nowhere else.
#define VERIFY_STARTING_CHECK_EXPR                                             \
  base::defer defer_##__LINE__(                                                \
      [this]() { this->stage_range_.low = DoneTypeVerificationStage; });       \
  if (stage_range_.high < StartTypeVerificationStage) { return nullptr; }      \
  if (stage_range_.low >= DoneTypeVerificationStage) {                         \
    return ASSERT_NOT_NULL(ctx->type_of(this));                                \
  }                                                                            \
  if (stage_range_.low == StartTypeVerificationStage) {                        \
    ctx->cyc_dep_vec_ = ctx->error_log_.CyclicDependency();                    \
    HANDLE_CYCLIC_DEPENDENCIES;                                                \
  }                                                                            \
  stage_range_.low = StartTypeVerificationStage

#define VERIFY_OR_RETURN(expr_type, expr)                                      \
  type::Type const *expr_type = nullptr;                                       \
  do {                                                                         \
    expr_type = expr->VerifyType(ctx);                                         \
    HANDLE_CYCLIC_DEPENDENCIES;                                                \
    if (expr_type == nullptr) {                                                \
      /* TODO Maybe this should be Nothing() */                                \
      limit_to(expr->stage_range_.high);                                       \
      return nullptr;                                                          \
    }                                                                          \
  } while (false)

#define RETURN_IF_NULL(expr)                                                   \
  do {                                                                         \
    if (expr == nullptr) { return nullptr; }                                   \
  } while (false)

#endif  // ICARUS_AST_VERIFY_MACROS_H
