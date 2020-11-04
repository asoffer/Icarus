#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/common.h"

namespace compiler {

type::QualType VerifyConcrete(Compiler &c,
                              ast::ShortFunctionLiteral const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, c.VerifyParams(node->params()));
  ASSIGN_OR(return _, auto body_qt, c.VerifyType(node->body()));
  return c.context().set_qual_type(
      node, type::QualType::Constant(
                type::Func(std::move(params), {body_qt.type()})));
}

type::QualType VerifyGeneric(Compiler &c,
                             ast::ShortFunctionLiteral const *node) {
  auto *diag_consumer = &c.diag();
  auto gen            = [node, c = Compiler(c.resources())](
                 core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    // TODO handle compilation failures.
    auto [params, rets, context, inserted] = c.Instantiate(node, args);
    Compiler body_compiler({
        .data                = context,
        .diagnostic_consumer = c.diag(),
        .importer            = c.importer(),
    });
    auto body_qt = body_compiler.VerifyType(node->body());
    ASSERT(body_qt.ok() == true);
    rets = {body_qt.type()};
    return type::Func(params.Transform([](auto const &p) { return p.second; }),
                      rets);
  };

  return c.context().set_qual_type(
      node, type::QualType::Constant(new type::GenericFunction(
                node->params().Transform([](auto const &p) {
                  return type::GenericFunction::EmptyStruct{};
                }),
                std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  ast::OverloadSet os;
  os.insert(node);
  context().SetAllOverloads(node, std::move(os));
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto qt,
                   node->is_generic() ? VerifyGeneric(*this, node)
                                      : VerifyConcrete(*this, node));
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
