#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <string_view>
#include <string>
#include "base/container/vector.h"
#include "base/container/unordered_map.h"
#include <mutex>

#include "type.h"

namespace IR {
struct Func;
} // namespace IR

namespace type {
struct Struct : public Type {
  struct Field {
    // TODO make a string_view but deal with trickiness of moving
    std::string name;
    const Type *type = nullptr;
    IR::Val init_val;
  };

  Struct() = default;
  Struct(base::vector<Field> fields,
         base::unordered_map<std::string, size_t> field_indices)
      : fields_(std::move(fields)), field_indices_(std::move(field_indices)) {}
  virtual ~Struct() {}
  BASIC_METHODS;

  // Return the type of a field, or a nullptr if it doesn't exist
  const Field *field(const std::string &name) const;

  virtual bool needs_destroy() const {
    for (const auto &field : fields_) {
      if (field.type->needs_destroy()) { return true; }
    }
    return false;
  }

  const Type *finalize();

  base::vector<Field> fields_;
  base::unordered_map<std::string, size_t> field_indices_;

private:
  mutable std::mutex mtx_;
  mutable IR::Func *init_func_ = nullptr, *assign_func = nullptr,
                   *destroy_func_ = nullptr, *repr_func_ = nullptr;
};
} // namespace type
#endif // ICARUS_TYPE_STRUCT_H
