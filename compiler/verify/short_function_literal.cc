#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "compiler/verify/common.h"

namespace compiler {

type::QualType VerifyConcrete(Compiler &c,
                              ast::ShortFunctionLiteral const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, c.VerifyParams(node->params()));
  ASSIGN_OR(return _, auto body_qt, c.VerifyType(node->body()));
  return c.data().set_qual_type(
      node, type::QualType::Constant(
                type::Func(std::move(params), {body_qt.type()})));
}

type::QualType VerifyGeneric(Compiler &c,
                             ast::ShortFunctionLiteral const *node) {
  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &c.diag();
  auto gen            = [node, compiler_data = &c.data(), diag_consumer,
              ordered_nodes = std::move(ordered_nodes)](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    // TODO handle compilation failures.
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);

    module::FileImporter<LibraryModule> importer;
    auto body_qt = Compiler({.builder             = ir::GetBuilder(),
                             .data                = data,
                             .diagnostic_consumer = *diag_consumer,
                             .importer            = importer})
                       .VerifyType(node->body());
    rets = {body_qt.type()};
    return type::Func(params.Transform([](auto const &p) { return p.second; }),
                      rets);
  };

  return c.data().set_qual_type(
      node, type::QualType::Constant(new type::GenericFunction(
                node->params().Transform([](auto const &p) {
                  return type::GenericFunction::EmptyStruct{};
                }),
                std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  ast::OverloadSet os;
  os.insert(node);
  data().SetAllOverloads(node, std::move(os));
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto qt,
                   node->is_generic() ? VerifyGeneric(*this, node)
                                      : VerifyConcrete(*this, node));
  return data().set_qual_type(node, qt);
}

}  // namespace compiler
