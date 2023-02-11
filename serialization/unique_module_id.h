#ifndef ICARUS_SERIALIZATION_UNIQUE_MODULE_ID_H
#define ICARUS_SERIALIZATION_UNIQUE_MODULE_ID_H

#include <string>
#include <string_view>
#include <utility>

namespace serialization {

// Represents an identifier for the module which is unique amongst all modules
// linked into the same binary.
struct UniqueModuleId {
  explicit UniqueModuleId(std::string &&value) : value_(std::move(value)) {}
  explicit UniqueModuleId(std::string_view value = "") : value_(value) {}
  explicit UniqueModuleId(char const *value) : value_(value) {}

  // Valid modules must only use printable characters.
  static UniqueModuleId Invalid() {
    return UniqueModuleId(std::string("\0", 1));
  }

  std::string_view value() const { return value_; }

  friend bool operator==(UniqueModuleId const &,
                         UniqueModuleId const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, UniqueModuleId const &id) {
    return H::combine(std::move(h), id.value_);
  }

 private:
  std::string value_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_UNIQUE_MODULE_ID_H
