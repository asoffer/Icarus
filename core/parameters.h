#ifndef ICARUS_CORE_PARAMS_H
#define ICARUS_CORE_PARAMS_H

#include <algorithm>
#include <iostream>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/extend/equality.h"
#include "base/extend/serialize.h"
#include "base/macros.h"
#include "base/serialize.h"
#include "base/universal_print.h"

namespace core {

// Flags indicating special properties of parameters.
struct ParameterFlags {
  constexpr ParameterFlags() = default;
  // Indicates that the parameter has a default so that arguments binding to it
  // can be elided.
  static constexpr auto HasDefault() { return ParameterFlags(0b00000001); }

  // Indicates that binding an argument to this parameter must be done
  // positionally.
  static constexpr auto MustNotName() { return ParameterFlags(0b00000010); }

  // Indicates that this parameter represents a formal variadic parameter.
  static constexpr auto Variadic() { return ParameterFlags(0b00000100); }

  friend constexpr ParameterFlags operator|(ParameterFlags lhs,
                                            ParameterFlags rhs) {
    return ParameterFlags(lhs.flags_ | rhs.flags_);
  }
  friend constexpr ParameterFlags operator&(ParameterFlags lhs,
                                            ParameterFlags rhs) {
    return ParameterFlags(lhs.flags_ & rhs.flags_);
  }

  friend constexpr bool operator==(ParameterFlags lhs,
                                   ParameterFlags rhs) = default;
  friend constexpr bool operator!=(ParameterFlags lhs,
                                   ParameterFlags rhs) = default;
  friend constexpr bool operator<=(ParameterFlags lhs, ParameterFlags rhs) {
    return (lhs & rhs) == lhs;
  }
  friend constexpr bool operator>=(ParameterFlags lhs, ParameterFlags rhs) {
    return rhs <= lhs;
  }
  friend constexpr bool operator<(ParameterFlags lhs, ParameterFlags rhs) {
    return lhs <= rhs and lhs != rhs;
  }
  friend constexpr bool operator>(ParameterFlags lhs, ParameterFlags rhs) {
    return rhs < lhs;
  }

  friend std::ostream& operator<<(std::ostream& os, ParameterFlags flags) {
    os << "flags(";
    std::string_view separator = "";
    if (HasDefault() <= flags) {
      os << std::exchange(separator, " | ") << "default";
    }

    if (MustNotName() <= flags) {
      os << std::exchange(separator, " | ") << "must-not-name";
    }

    if (Variadic() <= flags) {
      os << std::exchange(separator, " | ") << "variadic";
    }

    return os << ")";
  }

  template <typename H>
  friend H AbslHashValue(H h, ParameterFlags f) {
    return H::combine(std::move(h), f.flags_);
  }

  friend void BaseSerialize(base::Serializer auto& s, ParameterFlags f) {
    base::Serialize(s, f.flags_);
  }

  friend bool BaseDeserialize(base::Deserializer auto& d, ParameterFlags& f) {
    return base::Deserialize(d, f.flags_);
  }

  static constexpr ParameterFlags FromValue(uint8_t value) {
    return ParameterFlags(value);
  }
  uint8_t value() const { return flags_; }

 private:
  explicit constexpr ParameterFlags(uint8_t flags) : flags_(flags) {}

  uint8_t flags_ = 0;
};

// Represents a single parameter, consisting of a name, an associated value, and
// flags indicating specific properties. `Parameters` (see below) is a sequence
// of `Parameter` objects representing the sequence of parameters in a
// parameterized entity.
template <typename T>
struct Parameter
    : base::Extend<Parameter<T>>::template With<base::AbslHashExtension,
                                                base::BaseSerializeExtension> {
  using value_type = T;

  friend std::ostream& operator<<(std::ostream& os, Parameter const& param) {
    return os << param.name << ": " << base::UniversalPrintToString(param.value)
              << " " << param.flags;
  }

  std::string name = "";
  value_type value;
  ParameterFlags flags;
};

// Represents an entirely anonymous parameter which has no name and must not be
// named during argument binding.
template <typename T>
Parameter<std::decay_t<T>> AnonymousParameter(T&& val) {
  using type = std::decay_t<T>;
  return Parameter<type>{.name  = "",
                         .value = std::forward<T>(val),
                         .flags = ParameterFlags::MustNotName()};
}

// Represents a sequence of parameters for a parameterized entity which can be
// invoked by binding arguments to the parameters.
template <typename T>
struct Parameters
    : base::Extend<Parameters<T>, 1>::template With<base::AbslHashExtension> {
  using parameter_value_type = T;
  using value_type           = Parameter<parameter_value_type>;
  using parameter_type       = Parameter<parameter_value_type>;
  using const_iterator = typename std::vector<parameter_type>::const_iterator;

  // Constructs an empty `Parameters` object.
  Parameters() = default;

  // Constructs a `Parameters` object holding `n` elements, each of which are
  // default-constructed.
  template <std::default_initializable = parameter_type>
  explicit Parameters(size_t n) : params_(n) {}

  // Constructs a `Parameters object holding `n` elements, each of which is a
  // copy of `v`.
  explicit Parameters(size_t n, parameter_type const& v) : params_(n, v) {}

  // Constructs a `Parameters` object holding the parameters in `params` in the
  // same order.
  explicit Parameters(std::vector<parameter_type> params)
      : params_(std::move(params)) {}

  // Constructs a `Parameters` object holding the parameters in `params` in the
  // same order.
  Parameters(std::initializer_list<parameter_type> params) : params_(params) {}

  // Constructs a `Parameters` object holding the parameters in the range from
  // `b` to `e` (including `b`, but excluding `e`).
  template <std::input_iterator Iter>
  Parameters(Iter b, Iter e) requires(
      base::meta<std::decay_t<decltype(*std::declval<Iter>())>> ==
      base::meta<parameter_type>)
      : params_(b, e) {}

  // Returns the number of parameters in this object.
  constexpr size_t size() const { return params_.size(); }

  // Returns true if and only if the size is zero.
  constexpr bool empty() const { return params_.empty(); }

  // Removes all parameters elements.
  constexpr void clear() { params_.clear(); }

  // Reserves space for up to `n` parameters.
  void reserve(size_t n) { params_.reserve(n); }

  constexpr auto begin() const { return params_.begin(); }
  constexpr auto end() const { return params_.end(); }
  constexpr auto cbegin() const { return params_.begin(); }
  constexpr auto cend() const { return params_.end(); }

  constexpr auto begin() { return params_.begin(); }
  constexpr auto end() { return params_.end(); }

  // Returns a pair consisting of a pointer to the parameter names `s` and the
  // parameter's index if such a parameter exists. If no such parameter exists,
  // returns a null pointer for the first pair element. The value of the second
  // parameter is unspecified.
  std::pair<parameter_type*, size_t> try_get(std::string_view s) {
    size_t i = 0;
    for (auto& param : params_) {
      if (param.name == s) { return std::pair(&param, i); }
      ++i;
    }
    return std::pair(nullptr, 0);
  }

  // Returns a pair consisting of a pointer to the parameter names `s` and the
  // parameter's index if such a parameter exists. If no such parameter exists,
  // returns a null pointer for the first pair element. The value of the second
  // parameter is unspecified.
  std::pair<parameter_type const*, size_t> try_get(std::string_view s) const {
    size_t i = 0;
    for (auto const& param : params_) {
      if (param.name == s) { return std::pair(&param, i); }
      ++i;
    }
    return std::pair(nullptr, 0);
  }

  // Returns a reference to the element at index `i`. Behavior is undefined if
  // `i` is greater than `this->size()`.
  parameter_type const& operator[](size_t i) const& {
    ASSERT(i < params_.size());
    return params_[i];
  }

  // Returns a reference to the element at index `i`. Behavior is undefined if
  // `i` is greater than `this->size()`.
  parameter_type& operator[](size_t i) & {
    ASSERT(i < params_.size());
    return params_[i];
  }

  // Returns a reference to the element named `s`. Behavior is undefined if no
  // such element exists.
  parameter_type const& operator[](std::string_view s) const& {
    for (auto const& param : params_) {
      if (param.name == s) { return param; }
    }
    UNREACHABLE();
  }

  // Returns a reference to the element named `s`. Behavior is undefined if no
  // such element exists.
  parameter_type& operator[](std::string_view s) & {
    for (auto& param : params_) {
      if (param.name == s) { return param; }
    }
    UNREACHABLE();
  }

  void append(parameter_type const& p) { params_.push_back(p); }
  void append(parameter_type&& p) { params_.push_back(std::move(p)); }

  void append(std::string name, parameter_value_type val,
              ParameterFlags flags = ParameterFlags()) {
    params_.push_back(parameter_type{
        .name  = std::move(name),
        .value = std::move(val),
        .flags = flags,
    });
  }

  friend std::ostream& operator<<(std::ostream& os, Parameters const& p) {
    os << "Parameters[";
    std::string_view separator = "";
    for (auto const& parameter : p.params_) {
      os << std::exchange(separator, ", ") << parameter;
    }
    return os << "]";
  }

 private:
  friend base::EnableExtensions;
  std::vector<parameter_type> params_;
};

}  // namespace core

#endif  // ICARUS_CORE_PARAMS_H
