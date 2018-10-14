#ifndef ICARUS_AST_VERIFY_MACROS_H
#define ICARUS_AST_VERIFY_MACROS_H

#include "ast/declaration.h"
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

#define VERIFY_OR_RETURN(expr_type, expr)                                      \
  type::Type const *expr_type = nullptr;                                       \
  do {                                                                         \
    expr_type = expr->VerifyType(ctx);                                         \
    HANDLE_CYCLIC_DEPENDENCIES;                                                \
    if (expr_type == nullptr) { return nullptr; }                              \
  } while (false)

#define RETURN_IF_NULL(expr)                                                   \
  do {                                                                         \
    if (expr == nullptr) { return nullptr; }                                   \
  } while (false)

#endif  // ICARUS_AST_VERIFY_MACROS_H
