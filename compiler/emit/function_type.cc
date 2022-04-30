#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/reg_or.h"
#include "type/function.h"
#include "type/function_instructions.h"
#include "type/type.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::FunctionType const *node,
                           ir::PartialResultBuffer &out) {
  std::vector<std::pair<std::string, ir::RegOr<type::Type>>> param_vals;
  std::vector<ir::RegOr<type::Type>> out_vals;
  param_vals.reserve(node->parameters().size());
  out_vals.reserve(node->outputs().size());
  for (auto const *p : node->parameters()) {
    if (auto const *decl = p->if_as<ast::Declaration>()) {
      ASSERT(decl->ids().size() == 1u);
      if (auto const *te = decl->type_expr()) {
        param_vals.emplace_back(decl->ids()[0].name(), EmitAs<type::Type>(te));
      } else {
        NOT_YET();
      }
    } else {
      param_vals.emplace_back("", EmitAs<type::Type>(p));
    }
  }
  for (auto const *o : node->outputs()) {
    out_vals.push_back(EmitAs<type::Type>(o));
  }

  out.append(current_block()->Append(type::FunctionTypeInstruction{
      .inputs  = std::move(param_vals),
      .outputs = std::move(out_vals),
      .result  = current().subroutine->Reserve()}));
}

}  // namespace compiler
