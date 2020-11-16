#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/verify/common.h"

namespace compiler {
namespace {

std::optional<type::Quals> VerifyAndGetQuals(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto r = v->VerifyType(expr);
    err |= not r.ok();
    if (not err) { quals &= r.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

}  // namespace

type::QualType Compiler::VerifyType(ast::ArgumentType const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(ast::BuiltinFn const *node) {
  return context().set_qual_type(
      node, type::QualType::Constant(node->value().type()));
}

type::QualType Compiler::VerifyType(ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::YieldStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Verifying ScopeNode named `%s` at %d:%d",
      node->name()->DebugString(), node->name()->range().begin().line_num.value,
      node->name()->range().begin().offset.value);
  // TODO: The type of the arguments and the scope name are independent and
  // should not have early-exists.

  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyArguments(node->args()));

  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyType(node->name()));

  context().TrackJumps(node);

  for (auto const &block : node->blocks()) { VerifyType(&block); }

  ASSIGN_OR(type::QualType::Error(),  //
            ir::Scope scope, EvaluateOrDiagnoseAs<ir::Scope>(node->name()));
  auto *compiled_scope               = ir::CompiledScope::From(scope);
  ir::OverloadSet &exit_overload_set = compiled_scope->exit();

  std::vector<absl::Span<type::Type const>> return_types;
  std::optional<size_t> num_rets;
  for (core::Arguments<type::QualType> const &args :
       YieldArgumentTypes(context(), node)) {
    if (auto maybe_fn = exit_overload_set.Lookup(args)) {
      auto rets = maybe_fn->type()->return_types();
      if (num_rets) {
        if (*num_rets != rets.size()) { NOT_YET(); }
      } else {
        num_rets = rets.size();
      }
      return_types.push_back(rets);
    } else {
      NOT_YET();
    }
  }

  switch (return_types.size()) {
    case 0:
      return context().set_qual_type(node,
                                     type::QualType::NonConstant(type::Void()));
    case 1:
      return context().set_qual_type(
          node,
          type::QualType(return_types.front(), type::Quals::Unqualified()));
    default: {
      std::vector<type::Type> merged_rets(return_types.front().begin(),
                                          return_types.front().end());
      absl::Span<absl::Span<type::Type const> const> return_types_span =
          return_types;
      return_types_span.remove_prefix(1);
      for (absl::Span<type::Type const> rets : return_types_span) {
        for (size_t i = 0; i < *num_rets; ++i) {
          // TODO: Error checking.
          merged_rets[i] = type::Meet(merged_rets[i], rets[i]);
        }
      }
      return context().set_qual_type(
          node, type::QualType(merged_rets, type::Quals::Unqualified()));
    }
  }
}

type::QualType Compiler::VerifyType(ast::Label const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Label));
}

}  // namespace compiler
