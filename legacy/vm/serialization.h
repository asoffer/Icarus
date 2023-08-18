#ifndef ICARUS_VM_SERIALIZATION_H
#define ICARUS_VM_SERIALIZATION_H

#include "module/global_function_map.h"
#include "module/unique_id.h"
#include "serialization/foreign_symbol_map.h"
#include "serialization/proto/function.pb.h"
#include "serialization/read_only_data.h"
#include "vm/function.h"
#include "vm/function_table.h"

namespace vm {

struct SerializationState {
  SerializationState(serialization::ReadOnlyData& read_only_data,
                     serialization::ForeignSymbolMap& foreign_symbol_map,
                     module::UniqueId module_id,
                     module::GlobalFunctionMap& fn_map)
      : read_only_data_(read_only_data),
        foreign_symbol_map_(foreign_symbol_map),
        push_fn_state_(module_id, fn_map) {}
  template <typename T>
  T& get() {
    constexpr auto t = nth::type<T>;
    if constexpr (t == nth::type<serialization::ReadOnlyData>) {
      return read_only_data_;
    } else if constexpr (t == nth::type<serialization::ForeignSymbolMap>) {
      return foreign_symbol_map_;
    } else {
      return push_fn_state_;
    }
  }

 private:
  serialization::ReadOnlyData& read_only_data_;
  serialization::ForeignSymbolMap& foreign_symbol_map_;
  std::tuple<module::UniqueId, module::GlobalFunctionMap&> push_fn_state_;
};

void Serialize(Function const& f, serialization::proto::Function& proto,
               SerializationState& state);
bool Deserialize(serialization::proto::Function const& proto, Function& f,
                 SerializationState& state);

void Serialize(FunctionTable const& from,
               serialization::proto::FunctionTable& to,
               SerializationState& state);

bool Deserialize(serialization::proto::FunctionTable const& from,
                 FunctionTable& to, module::UniqueId module_id,
                 SerializationState& state);

}  // namespace vm

#endif  // ICARUS_VM_SERIALIZATION_H
