#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/instantiate.h"
#include "compiler/verify/verify.h"

namespace compiler {

type::QualType VerifyConcrete(CompilationDataReference data,
                              ast::BlockNode const *node) {
  bool has_error   = false;
  auto param_types = node->params().Transform([&](auto const &p) {
    auto qt = VerifyType(data, p.get())[0];
    has_error |= qt.HasErrorMark();
    return qt;
  });

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

type::QualType VerifyGeneric(CompilationDataReference data,
                             ast::BlockNode const *node) {
  auto gen = [node, data = data.data()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Block const * {
    Compiler c(&data);
    c.set_work_resources(wr);
    ASSIGN_OR(return nullptr,  //
                     auto result, Instantiate(c, node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("BlockNode", "inserted! %s into %s", node->DebugString(),
          context.DebugString());
      CompilationData data{.context        = &context,
                           .work_resources = wr,
                           .resources      = c.resources()};
      Compiler compiler(&data);
      auto qt = VerifyConcrete(compiler, node);
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
