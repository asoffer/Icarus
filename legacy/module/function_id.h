#ifndef ICARUS_MODULE_FUNCTION_ID_H
#define ICARUS_MODULE_FUNCTION_ID_H

#include <cstdint>
#include <ostream>

#include "module/unique_id.h"
#include "nth/debug/debug.h"
#include "nth/strings/interpolate.h"
#include "serialization/proto/function_index.pb.h"

namespace module {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
struct LocalFnId {
  using icarus_serialization_type = serialization::proto::FunctionIndex;
  using underlying_type           = uint32_t;

  constexpr explicit LocalFnId() = default;

  constexpr explicit LocalFnId(underlying_type n) : value_(n) {}

  auto operator<=>(LocalFnId const &) const = default;

  template <typename H>
  friend H AbslHashValue(H h, LocalFnId f) {
    return H::combine(std::move(h), f.value());
  }

  friend void NthPrint(auto &p, LocalFnId f) {
    if (f == LocalFnId::Invalid()) {
      p.write("invalid");
    } else {
      auto fmt = nth::config::default_formatter();
      nth::Interpolate<"local.{}">(p, fmt, f.value());
    }
  }

  // No module has this identifier.
  static constexpr LocalFnId Invalid() { return LocalFnId(); }

  underlying_type value() const { return value_; }

  friend void IcarusSerialize(auto &serializer, LocalFnId const &from) {
    serializer.output().set_index(from.value());
  }
  friend bool IcarusDeserialize(auto &deserializer, LocalFnId &to) {
    to.value_ = deserializer.input().index();
    return true;
  }

 private:
  underlying_type value_ = std::numeric_limits<underlying_type>::max();
};

struct Fn {
  Fn() : Fn(UniqueId::Invalid(), LocalFnId::Invalid()) {}
  explicit Fn(UniqueId mod, LocalFnId fn) : module_id_(mod), function_id_(fn) {}

  UniqueId module() const { return module_id_; }
  constexpr LocalFnId local() const { return function_id_; }

  friend bool operator==(Fn, Fn) = default;
  friend bool operator!=(Fn, Fn) = default;

  friend void NthPrint(auto &p, Fn f) {
    auto fmt = nth::config::default_formatter();
    nth::Interpolate<"Fn({}.{})">(p, fmt, f.module_id_, f.function_id_);
  }

  template <typename H>
  friend H AbslHashValue(H h, Fn f) {
    return H::combine(std::move(h), f.module_id_, f.function_id_);
  }

 private:
  UniqueId module_id_;
  LocalFnId function_id_;
};

}  // namespace module

#endif  // ICARUS_MODULE_FUNCTION_ID_H
