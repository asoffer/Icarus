#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::FunctionType const* node,
                                      FunctionData data) {
  data.function().AppendBeginParameterType();

  for (auto const* parameter : node->parameters()) {
    if (auto const* p = parameter->if_as<ast::Declaration>()) {
      if (auto const* type_expression = p->type_expr()) {
        if (p->ids().size() != 1) { NTH_UNIMPLEMENTED(); }
        std::string_view name = p->ids()[0].name();
        data.function().AppendPush(name.data());
        data.function().AppendPush(name.size());
        Emit(type_expression, data);
        data.function().AppendNamedParameter();
      } else {
        NTH_UNIMPLEMENTED();
      }
    } else {
      Emit(parameter, data);
      data.function().AppendAnonymousParameter();
    }
  }

  data.function().AppendEndParameterType(&GlobalTypeSystem);
  for (auto const* output : node->outputs()) { Emit(output, data); }
  data.function().AppendEndFunctionType(&GlobalTypeSystem,
                                        node->outputs().size());
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
