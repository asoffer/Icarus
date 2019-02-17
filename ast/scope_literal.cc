#include "ast/scope_literal.h"

#include "error/log.h"
#include "ir/val.h"
#include "misc/context.h"
#include "misc/scope.h"
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

void ScopeLiteral::DependentDecls(base::Graph<Declaration *> *g,
                                  Declaration *d) const {
  for (auto &decl : decls_) { decl.DependentDecls(g, d); }
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

VerifyResult ScopeLiteral::VerifyType(Context *ctx) {
  if (stateful_) {
    std::unordered_map<type::Pointer const *, std::vector<Declaration const *>>
        state_types;
    bool error = false;
    for (auto &decl : decls_) {
      // TODO handle errors.
      auto result = decl.VerifyType(ctx);
      if (decl.id_ == "done") {
        ASSIGN_OR(continue, auto &state_type,
                  StatePtrTypeOrLogError(result.type_));
        state_types[&state_type].push_back(&decl);
      } else if (result.type_ == type::Block ||
                 result.type_ == type::OptBlock ||
                 result.type_ == type::RepBlock) {
        // TODO add these types to the state_types map.
      }

      if (!result.const_) {
        error = true;
        NOT_YET("log an error");
      }
    }

    if (error) { return VerifyResult::Error(); }

    switch (state_types.size()) {
      case 0: {
        TextSpan block_title_span = span;
        block_title_span.finish   = block_title_span.start;
        block_title_span.finish.offset += sizeof("scope!") - 1;
        ctx->error_log()->StatefulScopeWithoutStateArg(block_title_span);
      } break;
      case 1: break;
      default: NOT_YET("Inconsistent"); break;
    }
  } else {
    bool error = false;
    for (auto &decl : decls_) {
      auto result = decl.VerifyType(ctx);
      if (!result.const_) {
        error = true;
        NOT_YET("log an error");
      }
    }
    if (error) { return VerifyResult::Error(); }
  }
  return VerifyResult::Constant(ctx->set_type(this, type::Scope));
}

void ScopeLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &decl : decls_) { decl.ExtractJumps(rets); }
}

std::vector<ir::Val> ast::ScopeLiteral::EmitIR(Context *ctx) {
  for (auto &decl : decls_) { decl.EmitIR(ctx); }
  return {ir::Val(this)};
}

}  // namespace ast
