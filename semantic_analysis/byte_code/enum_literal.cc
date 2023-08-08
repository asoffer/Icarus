#include "core/integer.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::EnumLiteral const* node,
                                      FunctionData data) {
  absl::flat_hash_set<uint64_t> used_values;
  serialization::TypeSystem::EnumType enum_type;
  auto& enumerators = *enum_type.mutable_enumerator();
  for (auto const& [name, value_expr] : node->specified_values()) {
    core::Type t = context().qualified_type(value_expr.get()).type();

    uint64_t value;
    // TODO: Handle casts correctly.
    if (t == Integer) {
      value = absl::Int128Low64(
          core::AsInt128(EvaluateAs<core::Integer>(value_expr.get())));
    } else {
      [[maybe_unused]] bool found =
          WithPrimitiveType(t, [&, v = value_expr.get()]<std::integral T> {
            value = EvaluateAs<T>(v);
          });
      NTH_ASSERT(found);
    }
    enumerators[name] = value;
    used_values.insert(value);
  }

  for (uint64_t i = 0; std::string_view name : node->enumerators()) {
    if (node->specified_values().contains(name)) { continue; }
    while (used_values.contains(i)) { ++i; }
    // Because we're counting up there's no need to insert `i`. We can just
    // move past it.
    enumerators[name] = i++;
  }

  auto [index, ptr] =
      resources().unique_type_table().insert_enum(std::move(enum_type));
  core::Type type =
      EnumType(type_system(), module::UniqueId::Self(), index, ptr);
  data.function().AppendPush(type);
}

void ByteCodeStatementEmitter::operator()(ast::EnumLiteral const*,
                                          FunctionData) {}

}  // namespace semantic_analysis
