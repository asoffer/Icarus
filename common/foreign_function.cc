#include "common/foreign_function.h"

#include <dlfcn.h>

#include "absl/synchronization/mutex.h"
#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"

namespace ic {
namespace {

absl::Mutex mutex;
int generation                               = 0;
nth::flyweight_map<std::pair<StringLiteral, type::FunctionType>,
                   std::pair<ForeignFunctionHandle, int>>
    foreign_functions = {};

uint32_t Insert(StringLiteral name, type::FunctionType t) {
  absl::MutexLock lock(&mutex);
  auto [iter, inserted] = foreign_functions.try_emplace(std::pair(name, t));
  iter->second.second   = generation;
  size_t index          = foreign_functions.index(iter);

  if (inserted) {
    dlerror();  // Clear existing errors.
    iter->second.first = ForeignFunctionHandle(
        dlsym(RTLD_DEFAULT, static_cast<std::string const &>(name).c_str()));
    char const *error = dlerror();
    if (error != nullptr) { NTH_UNIMPLEMENTED("{}") <<= {error}; }
    internal_foreign_function::ptr_index.emplace(iter->second.first, index);
  }
  return index;
}

}  // namespace

ForeignFunction::ForeignFunction()
    : StrongIdentifierType<ForeignFunction, uint32_t>(0) {
}

ForeignFunction::ForeignFunction(uint32_t n)
    : StrongIdentifierType<ForeignFunction, uint32_t>(n) {
  NTH_REQUIRE((v.debug), n < foreign_functions.size());
}

ForeignFunction::ForeignFunction(StringLiteral name, type::FunctionType t) {
  mutable_value() = Insert(name, t);
}

ForeignFunction ForeignFunction::FromIndex(uint32_t n) {
  return ForeignFunction(n);
}

StringLiteral ForeignFunction::name() const {
  return foreign_functions.from_index(index()).first.first;
}

type::FunctionType ForeignFunction::type() const {
  return foreign_functions.from_index(index()).first.second;
}

ForeignFunctionHandle ForeignFunction::function() const {
  return foreign_functions.from_index(index()).second.first;
}

std::vector<ForeignFunction> ForeignFunction::LatestGeneration() {
  absl::MutexLock lock(&mutex);
  std::vector<ForeignFunction> fs;
  size_t i = 0;
  for (auto [k, v] : foreign_functions) {
    if (v.second == generation) { fs.push_back(ForeignFunction(i)); }
    ++i;
  }
  return fs;
}

void ForeignFunction::CompleteGeneration() {
  absl::MutexLock lock(&mutex);
  ++generation;
}

}  // namespace ic
