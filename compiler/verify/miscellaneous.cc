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
    auto qt = v->VerifyType(expr)[0];
    err |= not qt.ok();
    if (not err) { quals &= qt.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::ArgumentType const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::BuiltinFn const *node) {
  return context().set_qual_type(
      node, type::QualType::Constant(ir::Fn(node->value()).type()));
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::ErrorSpan(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return {};
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::YieldStmt const *node) {
  ir::CompleteResultBuffer buffer;
  VerifyArguments(node->arguments(), buffer);
  return {};
}

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ScopeNode const *node) {
  LOG("ScopeNode", "Verifying ScopeNode named `%s`",
      node->name()->DebugString());
  // TODO: The type of the arguments and the scope name are independent and
  // should not have early-exists.

  ir::CompleteResultBuffer buffer;
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   std::ignore, VerifyArguments(node->arguments(), buffer));

  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   std::ignore, VerifyType(node->name())[0]);

  context().TrackJumps(node);

  std::vector<ir::ScopeContext::block_type> blocks;
  blocks.reserve(node->blocks().size());
  for (auto const &block : node->blocks()) {
    // TODO: When we start to allow block parameters to be deduced from values
    // injected from the scope, doing this work here won't make sense.
    VerifyType(&block);
    auto param_types = block.params().Transform(
        [&](auto const &p) { return context().qual_types(p.get())[0]; });
    blocks.emplace_back(std::string(block.name()), std::move(param_types));
  }
  // TODO: Validation that this scope context is valid for use with the unbound
  // scope.
  context().set_scope_context(node, std::move(blocks));

  // TODO: Allow for types to be yielded to this position.
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::Label const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Label));
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::IfStmt const *node) {
  // TODO: Emit errors if the type's fail to check.
  VerifyType(&node->condition());
  for (auto const *stmt : node->true_block()) { VerifyType(stmt); }
  if (node->has_false_block()) {
    for (auto const *stmt : node->false_block()) { VerifyType(stmt); }
  }

  // TODO: Allow for types to be yielded
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::WhileStmt const *node) {
  // TODO: Emit errors if the type's fail to check.
  VerifyType(&node->condition());
  for (auto const *stmt : node->body()) { VerifyType(stmt); }

  // TODO: Allow for types to be yielded
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

}  // namespace compiler
