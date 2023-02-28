#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::EnumLiteral const* node,
                                      FunctionData data) {
  absl::flat_hash_set<uint64_t> used_values;
  std::vector<std::pair<std::string, uint64_t>> enumerators;
  for (auto const& [name, value_expr] : node->specified_values()) {
    core::Type t = context().qualified_type(value_expr.get()).type();

    uint64_t value;
    // TODO: Handle casts correctly.
    if (t == Integer) {
      value = EvaluateAs<data_types::IntegerHandle>(value_expr.get())
                  .value()
                  .span()[0];
    } else {
      [[maybe_unused]] bool found =
          WithPrimitiveType(t, [&, v = value_expr.get()]<std::integral T> {
            value = EvaluateAs<T>(v);
          });
      ASSERT(found);
    }
    enumerators.emplace_back(name, value);
    used_values.insert(value);
    uint64_t i = 0;
    for (std::string_view name : node->enumerators()) {
      if (node->specified_values().contains(name)) { continue; }
      while (used_values.contains(i)) { ++i; }
      // Because we're counting up there's no need to insert `i`. We can just
      // move past it.
      enumerators.emplace_back(name, i++);
    }
  }
  core::Type type = EnumType(type_system(), std::move(enumerators));
  data.function().append<jasmin::Push>(type);
}

void ByteCodeStatementEmitter::operator()(ast::EnumLiteral const*,
                                          FunctionData) {}

}  // namespace semantic_analysis
