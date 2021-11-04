#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/instantiate.h"
#include "compiler/resources.h"
#include "compiler/verify/common.h"

namespace compiler {

type::QualType VerifyConcrete(Compiler &c,
                              ast::ShortFunctionLiteral const *node) {
  LOG("ShortFunctionLiteral", "VerifyConcrete %s", node->DebugString());
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, c.VerifyParams(node->params()));
  ASSIGN_OR(return _, auto body_qt, c.VerifyType(node->body())[0]);
  return c.context().set_qual_type(
      node, type::QualType::Constant(
                type::Func(std::move(params), {body_qt.type()})))[0];
}

type::QualType VerifyGeneric(Compiler &c,
                             ast::ShortFunctionLiteral const *node) {
  auto gen = [node,
              instantiation_compiler = Compiler(&c.context(), c.resources()),
              cg                     = c.builder().CurrentGroup()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Function const * {
    instantiation_compiler.set_work_resources(wr);
    auto [params, rets_ref, context, inserted] =
        Instantiate(instantiation_compiler, node, args);

    if (inserted) {
      LOG("FunctionLiteral", "inserted! %s", node->DebugString());
      PersistentResources resources = instantiation_compiler.resources();
      auto compiler = instantiation_compiler.MakeChild(&context, resources);
      compiler.set_work_resources(wr);
      compiler.builder().CurrentGroup() = cg;
      auto qt                           = VerifyConcrete(compiler, node);
      auto outs = qt.type().as<type::Function>().output();
      rets_ref.assign(outs.begin(), outs.end());

      // TODO: Provide a mechanism by which this can fail.
      ASSERT(qt.ok() == true);
      context.set_qual_type(node, qt);
      // TODO: We shouldn't have a queue per compiler. We may not be able to
      // verify these yet.
      return &qt.type().as<type::Function>();
    }

    type::Function const *ft = type::Func(params.types(), rets_ref);
    context.set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return c.context().set_qual_type(
      node, type::QualType::Constant(new type::GenericFunction(
                node->params().Transform([](auto const &p) {
                  return type::GenericFunction::EmptyStruct{};
                }),
                std::move(gen))))[0];
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  ast::OverloadSet os;
  os.insert(node);
  context().SetAllOverloads(node, std::move(os));
  auto qt = node->is_generic() ? VerifyGeneric(*this, node)
                               : VerifyConcrete(*this, node);
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
