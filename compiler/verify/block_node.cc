#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/instantiate.h"

namespace compiler {

type::QualType VerifyConcrete(Compiler &c, ast::BlockNode const *node) {
  bool has_error   = false;
  auto param_types = node->params().Transform([&](auto const &p) {
    auto qt = c.VerifyType(p.get())[0];
    has_error |= qt.HasErrorMark();
    return qt;
  });

  if (not has_error) {
    LOG("BlockNode", "Verifying %s %s", node->DebugString(),
        c.context().DebugString());
    for (auto *stmt : node->stmts()) {
      absl::Span<type::QualType const> qts = c.VerifyType(stmt);
      if (qts.size() == 1 and not qts[0].ok()) { has_error = true; }
    }
  }

  auto qt = type::QualType::Constant(type::Blk(std::move(param_types)));
  if (has_error) { qt.MarkError(); }
  return qt;
}

type::QualType VerifyGeneric(Compiler &c, ast::BlockNode const *node) {
  auto gen = [node,
              instantiation_compiler = Compiler(&c.context(), c.resources()),
              cg                     = c.builder().CurrentGroup()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Block const * {
    instantiation_compiler.set_work_resources(wr);
    ASSIGN_OR(return nullptr,  //
                     auto result,
                     Instantiate(instantiation_compiler, node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("BlockNode", "inserted! %s into %s", node->DebugString(),
          context.DebugString());
      PersistentResources resources = instantiation_compiler.resources();
      auto compiler = instantiation_compiler.MakeChild(&context, resources);
      compiler.set_work_resources(wr);
      compiler.builder().CurrentGroup() = cg;
      auto qt                           = VerifyConcrete(compiler, node);
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

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::BlockNode const *node) {
  LOG("BlockNode", "Verifying %s", node->DebugString());

  // TODO: Verify that the block's name makes sense.
  return context().set_qual_type(node, node->is_generic()
                                           ? VerifyGeneric(*this, node)
                                           : VerifyConcrete(*this, node));
}

}  // namespace compiler
