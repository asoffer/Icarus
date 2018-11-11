#include "ast/scope_literal.h"

#include "ast/declaration.h"
#include "ast/verify_macros.h"
#include "error/log.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/pointer.h"

namespace ast {
std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "scope " << (stateful_ ? "!" : "") << "{\n";
  for (const auto &decl : decls_) {
    ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
  }
  ss << "}";
  return ss.str();
}

void ScopeLiteral::assign_scope(Scope *scope) {
  scope_      = scope;
  body_scope_ = scope->add_child<DeclScope>();
  for (auto &decl : decls_) { decl.assign_scope(body_scope_.get()); }
}

type::Pointer const *StatePtrTypeOrLogError(type::Type const *t) {
  if (!t->is<type::Function>()) {
    NOT_YET("log an error");
    return nullptr;
  }
  auto &input_types = t->as<type::Function>().input;
  if (input_types.empty()) { return nullptr; }

  if (!input_types.at(0)->is<type::Pointer>()) {
    NOT_YET("log an error");
    return nullptr;
  }
  return &input_types.at(0)->as<type::Pointer>();
}

type::Type const *ScopeLiteral::VerifyType(Context *ctx) {
  ctx->set_type(this, type::Scope);

  if (stateful_) {
    std::unordered_map<type::Pointer const *, std::vector<Declaration const *>>
        state_types;
    for (auto &decl : decls_) {
      // TODO handle errors.
      auto *t = decl.VerifyType(ctx);
      if (decl.id_ == "done") {
        auto *state_type = StatePtrTypeOrLogError(t);
        if (state_type == nullptr) { continue; }
        state_types[state_type].push_back(&decl);
      } else if (t == type::Block || t == type::OptBlock ||
                 t == type::RepBlock) {
        // TODO add these types to the state_types map.
      }
    }
    switch (state_types.size()) {
      case 0: {
        TextSpan block_title_span = span;
        block_title_span.finish   = block_title_span.start;
        block_title_span.finish.offset += sizeof("scope!") - 1;
        ctx->error_log_.StatefulScopeWithoutStateArg(block_title_span);
      } break;
      case 1: break;
      default: NOT_YET("Inconsistent"); break;
    }
  } else {
    for (auto &decl : decls_) { decl.VerifyType(ctx); }
  }

  return type::Scope;
}

void ScopeLiteral::Validate(Context *ctx) {
  for (auto &decl : decls_) { decl.Validate(ctx); }
}

void ScopeLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &decl : decls_) { decl.ExtractJumps(rets); }
}

base::vector<ir::Val> ast::ScopeLiteral::EmitIR(Context *ctx) {
  for (auto &decl : decls_) { decl.EmitIR(ctx); }
  return {ir::Val(this)};
}
base::vector<ir::Register> ScopeLiteral::EmitLVal(Context *) {
  UNREACHABLE(this);
}
}  // namespace ast
