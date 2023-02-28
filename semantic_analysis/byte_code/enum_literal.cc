#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::EnumLiteral const* node,
                                      FunctionData data) {
  if (node->specified_values().size() != node->enumerators().size()) {
    NOT_YET("Don't yet handle automatically assigned enumerator values.");
  }

    std::vector<std::pair<std::string, uint64_t>> enumerators;
  for (auto const& [name, value_expr] : node->specified_values()) {
    core::Type t = context().qualified_type(value_expr.get()).type();
    if (t == I(64)) {
      enumerators.emplace_back(name, EvaluateAs<int64_t>(value_expr.get()));
    } else if (t == U(64)) {
      enumerators.emplace_back(name, EvaluateAs<uint64_t>(value_expr.get()));
    } else if (t == Integer) {
      // TODO: Use a real cast.
      enumerators.emplace_back(
          name, EvaluateAs<data_types::IntegerHandle>(value_expr.get())
                    .value()
                    .span()[0]);
    }
  }
  core::Type type = EnumType(type_system(), std::move(enumerators));
  data.function().append<jasmin::Push>(type);
}

void ByteCodeStatementEmitter::operator()(ast::EnumLiteral const*,
                                          FunctionData) {}

}  // namespace semantic_analysis
