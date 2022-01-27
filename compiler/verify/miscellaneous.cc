#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/context.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "ir/value/fn.h"

namespace compiler {
namespace {

struct NonBooleanCondition {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-boolean-condition";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("If statements require the condition to be of type "
                         "`bool`, but you provided a value of type `%s`.",
                         type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  std::string type;
};

std::optional<type::Quals> VerifyAndGetQuals(
    TypeVerifier &tv, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto qt = tv.VerifyType(expr)[0];
    err |= not qt.ok();
    if (not err) { quals &= qt.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ArgumentType const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::BuiltinFn const *node) {
  return context().set_qual_type(
      node, type::QualType::Constant(ir::Fn(node->value()).type()));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::ErrorSpan(),  //
                   auto quals, VerifyAndGetQuals(*this, node->exprs()));
  return {};
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::YieldStmt const *node) {
  ir::CompleteResultBuffer buffer;
  VerifyArguments(*this, node->arguments(), buffer);
  return {};
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ScopeNode const *node) {
  LOG("ScopeNode", "Verifying ScopeNode named `%s`",
      node->name()->DebugString());
  // TODO: The type of the arguments and the scope name are independent and
  // should not have early-exists.

  ir::CompleteResultBuffer buffer;
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   auto argument_values,
                   VerifyArguments(*this, node->arguments(), buffer));
  LOG("ScopeNode", "Arguments = %s", argument_values);

  // TODO: Determine to what extent we want to support ADL
  if (auto const *id = node->name()->if_as<ast::Identifier>()) {
    context().SetCallMetadata(node, CallMetadata(id->name(), node->scope()));
    context().set_qual_type(node->name(),
                            type::QualType::NonConstant(type::Void));
  } else {
    auto callee_qt = VerifyType(node->name())[0];

    LOG("ScopeNode", "Callee's qual-type is %s", callee_qt);
    if (not callee_qt.ok()) {
      return context().set_qual_type(node, type::QualType::Error());
    }

    if (auto const *access = node->name()->if_as<ast::Access>()) {
      std::optional mod_id =
          EvaluateOrDiagnoseAs<ir::ModuleId>(access->operand());
      if (not mod_id) {
        return context().set_qual_type(node, type::QualType::Error());
      }

      absl::flat_hash_set<CallMetadata::callee_locator_t> locs;
      for (auto &loc :
           importer().get(*mod_id).Exported(access->member_name())) {
        locs.insert(&loc);
      }

      context().SetCallMetadata(node, CallMetadata(std::move(locs)));
    } else {
      context().SetCallMetadata(node, CallMetadata(node->name()));
    }
  }

  auto qts_or_errors = VerifyCall(
      *this,
      {.call = node, .callee = node->name(), .arguments = argument_values});
  if (auto *errors = std::get_if<
          absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>(
          &qts_or_errors)) {
    diag().Consume(UncallableError(context(), node->name(), node->arguments(),
                                   std::move(*errors)));
    return context().set_qual_type(node, type::QualType::Error());
  }

  std::vector<ir::ScopeContext::block_type> blocks;
  blocks.reserve(node->blocks().size());
  for (auto const &block : node->blocks()) {
    type::Type t   = VerifyType(&block)[0].type();
    auto *as_block = t.if_as<type::Block>();
    using ptr_union_type =
        base::PtrUnion<type::Block const, type::Generic<type::Block> const>;
    auto ptr = as_block ? ptr_union_type(as_block)
                        : ptr_union_type(&t.as<type::Generic<type::Block>>());
    blocks.push_back({.name = block.name(), .type = ptr, .node = &block});
  }
  // TODO: Validation that this scope context is valid for use with the unbound
  // scope.
  context().set_scope_context(node, std::move(blocks));

  // TODO: Allow for types to be yielded to this position.
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Label const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Label));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::IfStmt const *node) {
  auto qt = VerifyType(&node->condition())[0];
  if (qt.type() != type::Bool) {
    diag().Consume(NonBooleanCondition{
        .view = SourceViewFor(node),
        .type = TypeForDiagnostic(&node->condition(), context()),
    });
  }
  if (node->hashtags.contains(ir::Hashtag::Const)) {
    if (not qt.constant()) { NOT_YET(); }
    if (*EvaluateOrDiagnoseAs<bool>(&node->condition())) {
      for (auto const *stmt : node->true_block()) { VerifyType(stmt); }
    } else if (node->has_false_block()) {
      for (auto const *stmt : node->false_block()) { VerifyType(stmt); }
    }

  } else {
    // TODO: Emit errors if the type's fail to check.
    for (auto const *stmt : node->true_block()) { VerifyType(stmt); }
    for (auto const *stmt : node->false_block()) { VerifyType(stmt); }
  }

  // TODO: Allow for types to be yielded
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::WhileStmt const *node) {
  // TODO: Emit errors if the type's fail to check.
  VerifyType(&node->condition());
  for (auto const *stmt : node->body()) { VerifyType(stmt); }

  // TODO: Allow for types to be yielded
  return context().set_qual_type(node, type::QualType::NonConstant(type::Void));
}

}  // namespace compiler
