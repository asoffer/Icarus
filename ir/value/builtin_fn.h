#ifndef ICARUS_IR_VALUE_BUILTIN_FN_H
#define ICARUS_IR_VALUE_BUILTIN_FN_H

#include <array>
#include <cstdint>
#include <optional>

#include "base/debug.h"

namespace ir {

// Represents a function that comes built-in to the language
struct BuiltinFn {
  friend struct Fn;
  enum class Which : uint8_t { Bytes, Alignment, Opaque, Foreign, DebugIr };
  explicit constexpr BuiltinFn(Which w) : which_(w) {}

  static BuiltinFn Bytes() { return BuiltinFn(Which::Bytes); }
  static BuiltinFn Alignment() { return BuiltinFn(Which::Alignment); }
  static BuiltinFn Opaque() { return BuiltinFn(Which::Opaque); }
  static BuiltinFn Foreign() { return BuiltinFn(Which::Foreign); }
  static BuiltinFn DebugIr() { return BuiltinFn(Which::DebugIr); }

  static std::optional<BuiltinFn> ByName(std::string_view name) {
    size_t i = 0;
    for (auto s : kNames) {
      if (s == name) { return BuiltinFn(static_cast<Which>(i)); }
      ++i;
    }
    return std::nullopt;
  }

  Which which() const { return which_; }

  friend std::ostream &operator<<(std::ostream &os, BuiltinFn f) {
    return os << kNames[static_cast<int>(f.which())];
  }

  friend bool operator==(BuiltinFn lhs, BuiltinFn rhs) {
    return lhs.which_ == rhs.which_;
  }

  friend bool operator!=(BuiltinFn lhs, BuiltinFn rhs) {
    return not(lhs == rhs);
  }

 private:
  static constexpr std::array kNames{
      "bytes", "alignment", "opaque", "foreign", "debug_ir",
  };

  Which which_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_BUILTIN_FN_H
