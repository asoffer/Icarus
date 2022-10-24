#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::FunctionType const* node,
                                      FunctionData data) {
  data.function().append<core::ParameterType::Begin>();

  for (auto const* parameter : node->parameters()) {
    if (auto const* p = parameter->if_as<ast::Declaration>()) {
      if (auto const* type_expression = p->type_expr()) {
        if (p->ids().size() != 1) { NOT_YET(); }
        std::string_view name = p->ids()[0].name();
        data.function().append<jasmin::Push>(name.data());
        data.function().append<jasmin::Push>(name.size());
        Emit(type_expression, data);
        data.function().append<core::ParameterType::AppendNamed>();
      } else {
        NOT_YET();
      }
    } else {
      Emit(parameter, data);
      data.function().append<core::ParameterType::Append>();
    }
  }

  data.function().append<core::ParameterType::End<TypeSystem>>(&type_system());
  for (auto const* output : node->outputs()) { Emit(output, data); }
  data.function().append<core::FunctionType::End<TypeSystem>>(
      &type_system(), node->outputs().size());
}

void ByteCodeStatementEmitter::operator()(ast::FunctionType const* node,
                                          FunctionData data) {
  for (auto const* parameter : node->parameters()) {
    if (auto const* p = parameter->if_as<ast::Declaration>()) {
      if (auto const* type_expression = p->type_expr()) {
        Emit(type_expression, data);
      }
    } else {
      Emit(parameter, data);
    }
  }

  for (auto const* output : node->outputs()) { Emit(output, data); }
}

}  // namespace semantic_analysis
