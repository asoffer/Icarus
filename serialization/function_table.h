#ifndef ICARUS_SERIALIZATION_FUNCTION_TABLE_H
#define ICARUS_SERIALIZATION_FUNCTION_TABLE_H

#include <deque>

#include "absl/container/flat_hash_map.h"
#include "jasmin/serialization.h"
#include "serialization/function_index.h"
#include "serialization/proto/function_table.pb.h"

namespace serialization {

template <typename FunctionType>
struct FunctionTable {
  std::pair<FunctionIndex, FunctionType*> emplace(size_t parameters,
                                                  size_t returns) {
    FunctionIndex index(functions_.size());
    auto& f = functions_.emplace_back(parameters, returns);
    function_indices_.emplace(&f, FunctionIndex(functions_.size() - 1));
    return std::pair(index, &f);
  }

  FunctionType const& function(FunctionIndex index) {
    ASSERT(index.value() < functions_.size());
    return functions_[index.value()];
  }

  FunctionIndex find(FunctionType const* f) {
    auto iter = function_indices_.find(f);
    if (iter == function_indices_.end()) { return FunctionIndex::Invalid(); }
    return iter->second;
  }

  template <typename State>
  static void Serialize(FunctionTable const& from, proto::FunctionTable& to,
                        State& state);
  template <typename State>
  static bool Deserialize(proto::FunctionTable const& from, FunctionTable& to,
                          State& state);

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
                                              State& state) {
  for (auto const& function : from.functions()) {
    auto [index, f] = to.emplace(function.parameters(), function.returns());
  }

  for (size_t i = 0; i < to.functions_.size(); ++i) {
    if (not jasmin::Deserialize(from.functions(i).content(), to.functions_[i],
                                state)) {
      return false;
    }
  }
  return true;
}

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_FUNCTION_TABLE_H
