#include "ast/ast.h"
#include "compiler/context.h"
#include "compiler/instantiate.h"
#include "compiler/verify/verify.h"

namespace compiler {

type::QualType VerifyConcrete(CompilationDataReference data,
                              ast::BlockNode const *node) {
  bool has_error   = false;
  core::Parameters<type::QualType> param_types;
  for (auto const &[name, value, flags] : node->params()) {
    auto qt = VerifyType(data, value.get())[0];
    has_error |= qt.HasErrorMark();
    param_types.append(name, qt, flags);
  }

  if (not has_error) {
    LOG("BlockNode", "Verifying %s %s", node->DebugString(),
        data.context().DebugString());
    for (auto *stmt : node->stmts()) {
      absl::Span<type::QualType const> qts = VerifyType(data, stmt);
      if (qts.size() == 1 and not qts[0].ok()) { has_error = true; }
    }
  }

  auto qt = type::QualType::Constant(type::Blk(std::move(param_types)));
  if (has_error) { qt.MarkError(); }
  return qt;
}

type::QualType VerifyGeneric(CompilationDataReference comp_data,
                             ast::BlockNode const *node) {
  auto gen = [node, comp_data = comp_data.data()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Block const * {
    ASSIGN_OR(
        return nullptr,  //
               auto result,
               Instantiate(CompilationDataReference(&comp_data), node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("BlockNode", "inserted! %s into %s", node->DebugString(),
          context.DebugString());
      CompilationData data{.context        = &context,
                           .work_resources = wr,
                           .resources      = comp_data.resources};
      auto qt = VerifyConcrete(CompilationDataReference(&data), node);
      // TODO: Provide a mechanism by which this can fail.
      ASSERT(qt.ok() == true);
      // TODO: We shouldn't have a queue per compiler. We may not be able to
      // verify these yet.
      return &qt.type().as<type::Block>();
    } else {
      LOG("BlockNode", "cached! %s", node->DebugString());
      return type::Blk(params);
    }
  };

  return type::QualType::Constant(
      type::Allocate<type::Generic<type::Block>>(std::move(gen)));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::BlockNode const *node) {
  LOG("BlockNode", "Verifying %s", node->DebugString());

  // TODO: Verify that the block's name makes sense.
  return context().set_qual_type(node, node->is_generic()
                                           ? VerifyGeneric(*this, node)
                                           : VerifyConcrete(*this, node));
}

}  // namespace compiler
