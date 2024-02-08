#include "common/foreign_function.h"

#include <dlfcn.h>

#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"

namespace ic {
namespace {

nth::flyweight_map<std::pair<StringLiteral, type::FunctionType>,
                   ForeignFunctionHandle>
    foreign_functions = {};

}  // namespace

ForeignFunction::ForeignFunction()
    : StrongIdentifierType<ForeignFunction, uint32_t>(0) {}

ForeignFunction::ForeignFunction(uint32_t n)
    : StrongIdentifierType<ForeignFunction, uint32_t>(n) {
  NTH_REQUIRE((v.debug), n < foreign_functions.size());
}

ForeignFunction::ForeignFunction(StringLiteral name, type::FunctionType t) {
  auto [iter, inserted] = foreign_functions.try_emplace(std::pair(name, t));
  size_t index          = foreign_functions.index(iter);
  mutable_value()       = index;
  if (inserted) {
    dlerror();  // Clear existing errors.
    iter->second = ForeignFunctionHandle(
        dlsym(RTLD_DEFAULT, std::string(name.str()).c_str()));
    char const *error = dlerror();
    if (error != nullptr) { NTH_UNIMPLEMENTED("{}") <<= {error}; }
    internal_foreign_function::ptr_index.emplace(iter->second, index);
  }
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
  return foreign_functions.from_index(index()).second;
}

std::vector<ForeignFunction> ForeignFunction::All() {
  std::vector<ForeignFunction> fs;
  fs.reserve(foreign_functions.size());
  for (size_t i = 0; i < foreign_functions.size(); ++i) {
    fs.push_back(ForeignFunction(i));
  }
  return fs;
}

}  // namespace ic
