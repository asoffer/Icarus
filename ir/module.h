#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <cstdint>
#include <span>

#include "absl/container/flat_hash_map.h"
#include "absl/container/inlined_vector.h"
#include "ir/function.h"
#include "ir/global_function_registry.h"
#include "jasmin/value.h"
#include "type/type.h"

namespace ic {

struct Module {
  explicit Module(GlobalFunctionRegistry& registry) : registry_(&registry) {}

  struct Entry {
    type::QualifiedType qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error);
    absl::InlinedVector<jasmin::Value, 2> value;
  };
  Entry const& Lookup(uint32_t index) const;
  void Insert(uint32_t index, Entry e);

  constexpr IrFunction& initializer() { return initializer_; }
  constexpr IrFunction const& initializer() const { return initializer_; }

  constexpr std::span<IrFunction> functions() { return functions_; }
  constexpr std::span<IrFunction const> functions() const { return functions_; }

  IrFunction& add_function(size_t parameters, size_t returns) {
    auto& f = functions_.emplace_back(parameters, returns);
    registry_->Register(
        FunctionId(ModuleId::Current(), LocalFunctionId(functions_.size() - 1)),
        &f);
    return f;
  }

 private:
  static Entry const DefaultEntry;

  absl::flat_hash_map<uint32_t, Entry> entries_;
  IrFunction initializer_{0, 0};
  std::vector<IrFunction> functions_;
  GlobalFunctionRegistry* registry_;
};

}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
