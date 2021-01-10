#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/function.h"
#include "type/type.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::FunctionType const *node) {
  std::vector<std::pair<std::string, ir::RegOr<type::Type>>> param_vals;
  std::vector<ir::RegOr<type::Type>> out_vals;
  param_vals.reserve(node->params().size());
  out_vals.reserve(node->outputs().size());
  for (auto const *p : node->params()) {
    if (auto const *decl = p->if_as<ast::Declaration>()) {
      ASSERT(decl->ids().size() == 1u);
      if (auto const *te = decl->type_expr()) {
        param_vals.emplace_back(decl->ids()[0].name(),
                                EmitValue(te).get<ir::RegOr<type::Type>>());
      } else {
        NOT_YET();
      }
    } else {
      param_vals.emplace_back("", EmitValue(p).get<ir::RegOr<type::Type>>());
    }
  }
  for (auto const *o : node->outputs()) {
    out_vals.push_back(EmitValue(o).get<ir::RegOr<type::Type>>());
  }

  return ir::Value(current_block()->Append(type::FunctionTypeInstruction{
      .inputs  = std::move(param_vals),
      .outputs = std::move(out_vals),
      .result  = builder().CurrentGroup()->Reserve()}));
}

}  // namespace compiler
