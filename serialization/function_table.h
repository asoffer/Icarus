#ifndef ICARUS_SERIALIZATION_FUNCTION_TABLE_H
#define ICARUS_SERIALIZATION_FUNCTION_TABLE_H

#include <deque>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "jasmin/serialization.h"
#include "serialization/function_index.h"
#include "serialization/module_index.h"
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

  FunctionType const& function(FunctionIndex index) const {
    ASSERT(index.value() < functions_.size());
    return functions_[index.value()];
  }

  FunctionType const& function(ModuleIndex module_index,
                               FunctionIndex function_index) const {
    if (module_index == ModuleIndex::Self()) {
      return function(function_index);
    } else {
      auto iter = reverse_wrapper_.find({module_index, function_index});
      ASSERT(iter != reverse_wrapper_.end());
      return function(iter->second);
    }
  }

  FunctionIndex find(FunctionType const* f) {
    if (auto iter = function_indices_.find(f);
        iter != function_indices_.end()) {
      return iter->second;
    } else {
      return FunctionIndex::Invalid();
    }
  }

  std::pair<ModuleIndex, FunctionIndex> find_wrapper(FunctionType const* f) {
    if (auto iter = function_indices_.find(f);
        iter != function_indices_.end()) {
      return std::pair(ModuleIndex::Self(), iter->second);
    } else if (auto iter = wrapper_.find(f); iter != wrapper_.end()) {
      return std::pair(iter->second.module_index, iter->second.wrapped_index);
    } else {
      return std::pair(ModuleIndex::Invalid(), FunctionIndex::Invalid());
    }
  }

  template <std::invocable<FunctionType&> MakeWrapper>
  std::pair<FunctionIndex, FunctionType const*> emplace_wrapper(
      ModuleIndex module_index, FunctionIndex wrapped_index,
      FunctionType const* f, MakeWrapper&& make_wrapper) {
    auto [iter, inserted] = wrapper_.emplace(
        f, WrapperEntry{.wrapper_index = FunctionIndex::Invalid(),
                        .module_index  = module_index,
                        .wrapped_index = wrapped_index});
    FunctionIndex index;
    if (inserted) {
      FunctionType* wrapper;
      std::tie(index, wrapper) =
          emplace(f->parameter_count(), f->return_count());
      std::forward<MakeWrapper>(make_wrapper)(*wrapper);
      iter->second.wrapper_index = index;
      reverse_wrapper_.emplace(std::pair(module_index, wrapped_index), index);
    } else {
      index = iter->second.wrapper_index;
    }
    return std::pair(index, &function(index));
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

  struct WrapperEntry {
    // The index of the wrapper function in this module.
    FunctionIndex wrapper_index;
    // The index of the module in which we can find the wrapped function.
    ModuleIndex module_index;
    // The index of the wrapped function in `module_index`.
    FunctionIndex wrapped_index;
  };

  // Maps a pointer to a function in a different module to information about
  // where it can be found in that module and where its wrapper can be found in
  // this module.
  absl::flat_hash_map<FunctionType const*, WrapperEntry> wrapper_;
  // Maps information about where a function can be found in a different module
  // to the wrapper for that function in the current module.
  absl::btree_map<std::pair<ModuleIndex, FunctionIndex>, FunctionIndex>
      reverse_wrapper_;
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

  to.mutable_wrappers()->Reserve(from.reverse_wrapper_.size());
  for (auto [wrapped, fn_id] : from.reverse_wrapper_) {
    auto& wrapper = *to.add_wrappers();
    wrapper.set_wrapped_module(wrapped.first.value());
    wrapper.mutable_wrapped_function()->set_index(wrapped.second.value());
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

  for (auto const& wrapper : from.wrappers()) {
    ModuleIndex module_index(wrapper.wrapped_module());
    FunctionIndex function_index(wrapper.wrapped_function().index());
    auto const* f = state.lookup(module_index, function_index);
    to.emplace_wrapper(module_index, function_index, f, state.make_wrapper(f));
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
