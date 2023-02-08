#ifndef ICARUS_MODULE_MODULE_NAME_H
#define ICARUS_MODULE_MODULE_NAME_H

#include <string>
#include <string_view>
#include <utility>

namespace module {

// Represents the name of a module as specifiable in an `import` expression.
struct ModuleName {
  explicit ModuleName(std::string &&name) : name_(std::move(name)) {}
  explicit ModuleName(std::string_view name = "") : name_(name) {}
  explicit ModuleName(char const *name) : name_(name) {}

  std::string_view name() const { return name_; }

  friend bool operator==(ModuleName const &, ModuleName const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, ModuleName const &id) {
    return H::combine(std::move(h), id.name_);
  }

 private:
  std::string name_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_NAME_H
