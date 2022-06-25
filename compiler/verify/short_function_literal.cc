#include "ast/ast.h"
#include "compiler/instantiate.h"
#include "compiler/resources.h"
#include "compiler/verify/common.h"

namespace compiler {

type::QualType VerifyConcrete(CompilationDataReference data,
                              ast::ShortFunctionLiteral const *node) {
  LOG("ShortFunctionLiteral", "VerifyConcrete %s", node->DebugString());
  TypeVerifier tv(data);
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, VerifyParameters(tv, node->parameters()));
  ASSIGN_OR(return _, auto body_qt, VerifyType(data, node->body())[0]);
  return data.context().set_qual_type(
      node, type::QualType::Constant(
                type::Func(std::move(params), {body_qt.type()})))[0];
}

type::QualType VerifyGeneric(CompilationDataReference data,
                             ast::ShortFunctionLiteral const *node) {
  auto gen = [node, comp_data = data.data()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Function const * {
    comp_data.work_resources = wr;
    ASSIGN_OR(
        return nullptr,  //
               auto result,
               Instantiate(CompilationDataReference(&comp_data), node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("ShortFunctionLiteral", "inserted! %s", node->DebugString());
      CompilationData data{.context        = &context,
                           .work_resources = wr,
                           .resources      = comp_data.resources};
      auto qt   = VerifyConcrete(CompilationDataReference(&data), node);
      auto outs = qt.type().as<type::Function>().return_types();
      rets_ref.assign(outs.begin(), outs.end());

      // TODO: Provide a mechanism by which this can fail.
      ASSERT(qt.ok() == true);
      context.set_qual_type(node, qt);
      // TODO: We shouldn't have a queue per compiler. We may not be able to
      // verify these yet.
      return &qt.type().as<type::Function>();
    }

    type::Function const *ft = type::Func(params, rets_ref);
    context.set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return data.context().set_qual_type(
      node, type::QualType::Constant(
                new type::LegacyGeneric<type::Function>(std::move(gen))))[0];
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ShortFunctionLiteral const *node) {
  auto qt = node->is_generic() ? VerifyGeneric(*this, node)
                               : VerifyConcrete(*this, node);
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
