#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::FunctionType const* node, IrFunction& f) {
  f.append<core::ParameterType::Begin>();

  for (auto const* parameter : node->parameters()) {
    if (auto const* p = parameter->if_as<ast::Declaration>()) {
      if (auto const* type_expression = p->type_expr()) {
        if (p->ids().size() != 1) { NOT_YET(); }
        std::string_view name = p->ids()[0].name();
        f.append<jasmin::Push>(name.data());
        f.append<jasmin::Push>(name.size());
        EmitByteCode(type_expression, f);
        f.append<core::ParameterType::AppendNamed>();
      } else {
        NOT_YET();
      }
    } else {
      EmitByteCode(parameter, f);
      f.append<core::ParameterType::Append>();
    }
  }

  f.append<core::ParameterType::End<TypeSystem>>(&type_system());
  for (auto const* output : node->outputs()) { EmitByteCode(output, f); }
  f.append<core::FunctionType::End<TypeSystem>>(&type_system(),
                                                node->outputs().size());
}

}  // namespace semantic_analysis
