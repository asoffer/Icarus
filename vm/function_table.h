#ifndef ICARUS_VM_FUNCTION_TABLE_H
#define ICARUS_VM_FUNCTION_TABLE_H

#include <deque>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "module/global_function_map.h"
#include "module/unique_id.h"
#include "serialization/function_index.h"
#include "serialization/proto/function_table.pb.h"
#include "vm/function.h"

namespace vm {
// TODO
struct SerializationState;

struct FunctionTable {
  explicit FunctionTable(module::GlobalFunctionMap& map) : function_map_(map) {}

  std::pair<serialization::FunctionIndex, Function*> emplace(
      size_t parameters, size_t returns, module::UniqueId module_id);

  Function const& function(serialization::FunctionIndex index) const;

  serialization::FunctionIndex find(Function const* f);

 private:
  friend void Serialize(Function const& f,
                        serialization::proto::Function& proto,
                        SerializationState& state);
  friend bool Deserialize(serialization::proto::Function const& proto,
                          Function& f, SerializationState& state);
  friend void Serialize(FunctionTable const& from,
                        serialization::proto::FunctionTable& to,
                        SerializationState& state);

  friend bool Deserialize(serialization::proto::FunctionTable const& from,
                          FunctionTable& to, module::UniqueId module_id,
                          SerializationState& state);

  // NOTE: This looks a lot like the implementation of `nth::flyweight_set`, but
  // it cannot be replaced with that type. This is because the `Function`s
  // held need not be hashable or equality-comparable. Moreover they need to be
  // mutated after insertion.
  std::deque<Function> functions_;
  absl::flat_hash_map<Function const*, serialization::FunctionIndex>
      function_indices_;

  module::GlobalFunctionMap& function_map_;
};

}  // namespace vm

#endif  // ICARUS_VM_FUNCTION_TABLE_H
