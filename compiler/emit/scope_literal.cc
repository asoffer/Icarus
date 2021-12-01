#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/reg_or.h"
#include "type/type.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ScopeLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeLiteral", "State type = %p", node->state_type());
  type::Type state_type = nullptr;
  if (node->state_type()) {
    ASSIGN_OR(return,  //
                    state_type,
                    EvaluateOrDiagnoseAs<type::Type>(node->state_type()));
  }

  absl::flat_hash_map<std::string_view, ir::Block> blocks;
  std::vector<ir::RegOr<ir::Jump>> enters;
  std::vector<ir::RegOr<ir::Fn>> exits;
  for (auto const &decl : node->decls()) {
    for (auto const &id : decl.ids()) {
      if (id.name() == "enter") {
        enters.push_back(EmitAs<ir::Jump>(&id));
      } else if (id.name() == "exit") {
        exits.push_back(EmitAs<ir::Fn>(&id));
      } else {
        blocks.emplace(id.name(), EmitAs<ir::Block>(&id).value());
      }
    }
  }

  ir::Scope s = context().add_scope(state_type);
  builder().MakeScope(s, std::move(enters), std::move(exits),
                      std::move(blocks));
  out.append(s);
}

}  // namespace compiler
