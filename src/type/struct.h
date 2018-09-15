#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <atomic>
#include <mutex>
#include <string>
#include <string_view>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"
#include "ir/val.h"
#include "type/type.h"

namespace IR {
struct Func;
} // namespace IR

namespace type {
struct Struct : public Type {
  struct Field {
    Field(type::Type const *t) : type(t) {}
    // TODO make a string_view but deal with trickiness of moving
    std::string name;
    const Type *type = nullptr;
    IR::Val init_val;
  };

  virtual ~Struct() {}
  BASIC_METHODS;

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string const &name) const {
    auto iter = field_indices_.find(name);
    if (iter == field_indices_.end()) { return nullptr; }
    return &fields_[iter->second];
  }

  virtual bool needs_destroy() const {
    return std::any_of(fields_.begin(), fields_.end(),
                       [](Field const &f) { return f.type->needs_destroy(); });
  }

  static Type const *Make() { return new Struct; }

  void finalize() { complete_ = true; }

  base::vector<Field> fields_;
  base::unordered_map<std::string, size_t> field_indices_;

 private:
  Struct() = default;
  mutable std::mutex mtx_;
  mutable IR::Func *init_func_ = nullptr, *assign_func = nullptr,
                   *destroy_func_ = nullptr, *repr_func_ = nullptr;
  // TODO This probably doesn't need any thread safety guarantees, but I'm not
  // sure yet.
  mutable std::atomic<bool> complete_ = false;
};

} // namespace type
#endif // ICARUS_TYPE_STRUCT_H
