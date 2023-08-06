#ifndef ICARUS_MODULE_UNIQUE_MODULE_ID_H
#define ICARUS_MODULE_UNIQUE_MODULE_ID_H

#include <string>
#include <string_view>
#include <utility>

#include "nth/base/attributes.h"
#include "nth/debug/debug.h"
#include "nth/debug/trace/trace.h"

namespace module {

// Represents an identifier for the module which is unique amongst all modules
// linked into the same binary.
struct UniqueId {
  // Constructs a `UniqueId` from a pointer to a constant `std::string`. The
  // unique identifier represents the pointed-to string value, in the sense that
  // two `UniqueId`s will compare equal if and only if they point to strings
  // with the same underlyring value (even if they point to different strings).
  explicit UniqueId(NTH_ATTRIBUTE(lifetimebound) std::string const *value)
      : value_(value) {
    NTH_ASSERT((v.harden), value_ != nullptr);
  }

  // Constructs a `UniqueId` from `value`, possibly constructing an eternal
  // string comparing equal to `value`. As such, it is not required that that
  // `value` live at least as long as the constructed `UniqueId`.
  explicit UniqueId(std::string &&value);
  explicit UniqueId(std::string_view value = "");
  template <size_t N>
  explicit UniqueId(char const (&value)[N])
      : UniqueId(std::string_view(value)) {}

  // Valid modules must only use printable characters.
  static UniqueId Invalid();

  std::string_view value() const { return *value_; }

  friend bool operator==(UniqueId lhs, UniqueId rhs) {
    return lhs.value_ == rhs.value_ or *lhs.value_ == *rhs.value_;
  }
  friend bool operator!=(UniqueId lhs, UniqueId rhs) { return not(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, UniqueId id) {
    return H::combine(std::move(h), *id.value_);
  }

  friend void NthPrint(auto &p, UniqueId id) {
    p.write("uid.");
    if (*id.value_ == "") {
      p.write("[invalid]");
    } else {
      p.write(*id.value_);
    }
  }

  friend bool NthCommandlineParse(std::string_view s, UniqueId &id, auto) {
    id = UniqueId(s);
    return true;
  }

 private:
  std::string const *value_;
};

}  // namespace module

NTH_TRACE_DECLARE_API(module::UniqueId, (value));

#endif  // ICARUS_MODULE_UNIQUE_MODULE_ID_H
