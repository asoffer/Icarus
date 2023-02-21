#ifndef ICARUS_SERIALIZATION_FUNCTION_TABLE_H
#define ICARUS_SERIALIZATION_FUNCTION_TABLE_H

#include <deque>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "jasmin/serialization.h"
#include "module/function_map.h"
#include "serialization/function_index.h"
#include "serialization/module_index.h"
#include "serialization/proto/function_table.pb.h"

namespace serialization {

template <typename FunctionType>
struct FunctionTable {
  explicit FunctionTable(module::FunctionMap& map) : function_map_(map) {}

  std::pair<FunctionIndex, FunctionType*> emplace(size_t parameters,
                                                  size_t returns) {
    FunctionIndex index(functions_.size());
    auto& f = functions_.emplace_back(parameters, returns);
    function_indices_.emplace(&f, FunctionIndex(functions_.size() - 1));

    // Note: This is correct because we only call `emplace` on the
    // currently-being compiled function.
    function_map_.insert_function(&f, serialization::ModuleIndex::Self(),
                                  index);

    return std::pair(index, &f);
  }

  FunctionType const& function(FunctionIndex index) const {
    ASSERT(index.value() < functions_.size());
    return functions_[index.value()];
  }

  FunctionIndex find(FunctionType const* f) {
    if (auto iter = function_indices_.find(f);
        iter != function_indices_.end()) {
      return iter->second;
    } else {
      return FunctionIndex::Invalid();
    }
  }

  template <typename State>
  static void Serialize(FunctionTable const& from, proto::FunctionTable& to,
                        State& state);
  template <typename State>
  static bool Deserialize(proto::FunctionTable const& from, FunctionTable& to,
                          ModuleIndex module_index, State& state);

 private:
  template <typename State>
  static void SerializeFunction(FunctionType const& f, proto::Function& proto,
                                State& state) {
    proto.set_parameters(f.parameter_count());
    proto.set_returns(f.return_count());
    jasmin::Serialize(f, *proto.mutable_content(), state);
  }

  // NOTE: This looks a lot like the implementation of `nth::flyweight_set`, but
  // it cannot be replaced with that type. This is because the `FunctionType`s
  // held need not be hashable or equality-comparable. Moreover they need to be
  // mutated after insertion.
  std::deque<FunctionType> functions_;
  absl::flat_hash_map<FunctionType const*, FunctionIndex> function_indices_;

  module::FunctionMap& function_map_;
};

template <typename FunctionType>
template <typename State>
void FunctionTable<FunctionType>::Serialize(
    FunctionTable<FunctionType> const& from, proto::FunctionTable& to,
    State& state) {
  to.mutable_functions()->Reserve(from.functions_.size());
  for (auto const& function : from.functions_) {
    SerializeFunction(function, *to.add_functions(), state);
  }
}

template <typename FunctionType>
template <typename State>
bool FunctionTable<FunctionType>::Deserialize(proto::FunctionTable const& from,
                                              FunctionTable<FunctionType>& to,
                                              ModuleIndex module_index,
                                              State& state) {
  for (auto const& function : from.functions()) {
    auto [index, f] = to.emplace(function.parameters(), function.returns());
    auto& fn_map = state.function_state().template get<module::FunctionMap>();
    fn_map.insert_function(&f, module_index, index);
  }

  for (size_t i = 0; i < to.functions_.size(); ++i) {
    if (not jasmin::Deserialize(from.functions(i).content(), to.functions_[i],
                                state.function_state())) {
      return false;
    }
  }
  return true;
}

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_FUNCTION_TABLE_H
