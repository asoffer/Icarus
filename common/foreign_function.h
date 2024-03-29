#ifndef ICARUS_COMMON_FOREIGN_FUNCTION_H
#define ICARUS_COMMON_FOREIGN_FUNCTION_H

#include <cstddef>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "common/result.h"
#include "common/string_literal.h"
#include "common/strong_identifier_type.h"
#include "nth/container/flyweight_map.h"
#include "nth/debug/debug.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/reader/reader.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/writer.h"
#include "type/function.h"

namespace ic {

struct ForeignFunctionHandle {
  ForeignFunctionHandle() = default;
  explicit ForeignFunctionHandle(void const *ptr) : ptr_(ptr) {}

  void const *ptr() const { return ptr_; }

  friend Result NthSerialize(auto &s, ForeignFunctionHandle f);
  friend Result NthDeserialize(auto &d, ForeignFunctionHandle &f);

  friend bool operator==(ForeignFunctionHandle lhs, ForeignFunctionHandle rhs) {
    return lhs.ptr_ == rhs.ptr_;
  }

  friend bool operator!=(ForeignFunctionHandle lhs, ForeignFunctionHandle rhs) {
    return lhs.ptr_ != rhs.ptr_;
  }

  template <typename H>
  friend H AbslHashValue(H h, ForeignFunctionHandle f) {
    return H::combine(std::move(h), f.ptr_);
  }

 private:
  friend struct ForeignFunction;

  void const *ptr_;
};

namespace internal_foreign_function {

inline absl::flat_hash_map<ForeignFunctionHandle, uint32_t> ptr_index;

}  // namespace internal_foreign_function

struct ForeignFunction
    : private StrongIdentifierType<ForeignFunction, uint32_t> {
  ForeignFunction();
  ForeignFunction(StringLiteral name, type::FunctionType t);

  static ForeignFunction FromIndex(uint32_t n);

  size_t index() const {
    return StrongIdentifierType<ForeignFunction, uint32_t>::value();
  }

  StringLiteral name() const;
  type::FunctionType type() const;
  ForeignFunctionHandle function() const;

  friend Result NthSerialize(auto &s, ForeignFunction f) {
    return nth::io::serialize(s, f.name(), f.type());
  }

  friend bool operator==(ForeignFunction lhs, ForeignFunction rhs) {
    return lhs.value() == rhs.value();
  }

  friend bool operator!=(ForeignFunction lhs, ForeignFunction rhs) {
    return lhs.value() != rhs.value();
  }

  template <typename H>
  friend H AbslHashValue(H h, ForeignFunction f) {
    return H::combine(std::move(h), f.value());
  }

  friend void NthPrint(auto &p, auto &f, ForeignFunction fn) {
    f(p, fn.name());
  }

  static void CompleteGeneration();
  // TODO: Use a better container/reference.
  static std::vector<ForeignFunction> LatestGeneration();

 private:
  ForeignFunction(uint32_t n);
};

Result NthSerialize(auto &s, ForeignFunctionHandle f) {
  auto iter = internal_foreign_function::ptr_index.find(f);
  NTH_REQUIRE((v.debug), iter != internal_foreign_function::ptr_index.end());
  return nth::io::write_fixed(s, iter->second);
}

Result NthDeserialize(auto &d, ForeignFunctionHandle &f) {
  uint32_t index;
  if (not nth::io::read_fixed(d, index)) { return Result(false); }
  f = ForeignFunction::FromIndex(index).function();
  return Result::success();
}

}  // namespace ic

#endif  // ICARUS_COMMON_FOREIGN_FUNCTION_H
