#ifndef ICARUS_MODULE_UNIQUE_MODULE_ID_H
#define ICARUS_MODULE_UNIQUE_MODULE_ID_H

#include <string>
#include <string_view>
#include <utility>

namespace module {

// Represents an identifier for the module which is unique amongst all modules
// linked into the same binary.
struct UniqueId {
  explicit UniqueId(std::string &&value) : value_(std::move(value)) {}
  explicit UniqueId(std::string_view value = "") : value_(value) {}
  template <size_t N>
  explicit UniqueId(char const (&value)[N]) : value_(value) {}

  // Valid modules must only use printable characters.
  static UniqueId Invalid() { return UniqueId(std::string("\0", 1)); }

  std::string_view value() const { return value_; }

  friend bool operator==(UniqueId const &, UniqueId const &) = default;
  friend bool operator!=(UniqueId const &, UniqueId const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, UniqueId const &id) {
    return H::combine(std::move(h), id.value_);
  }

  friend bool NthCommandlineParse(std::string_view s, UniqueId &id, auto) {
    id = UniqueId(s);
    return true;
  }

 private:
  std::string value_;
};

}  // namespace module

#endif  // ICARUS_MODULE_UNIQUE_MODULE_ID_H
