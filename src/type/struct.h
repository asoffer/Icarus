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

struct Architecture;

namespace ast {
struct StructLiteral;
}  // namespace ast

namespace ir {
struct Func;
}  // namespace ir

namespace type {
struct Struct : public Type {
  struct Field {
    Field(type::Type const *t) : type(t) {}
    // TODO make a string_view but deal with trickiness of moving
    std::string name;
    const Type *type = nullptr;
    ir::Val init_val;
  };

  virtual ~Struct() {}
  BASIC_METHODS;

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string const &name) const;

  virtual bool needs_destroy() const {
    ASSERT(to_be_completed_ == nullptr);
    return std::any_of(fields_.begin(), fields_.end(),
                       [](Field const &f) { return f.type->needs_destroy(); });
  }

  static Struct *Make(ast::StructLiteral *lit);
  void finalize();

  size_t offset(size_t n, Architecture const &arch) const;

  base::vector<Field> const &fields() const;
  void set_last_name(std::string_view s);

  size_t index(std::string const &name) const;

  void add_field(type::Type const *t) { fields_.emplace_back(t); }

 private:
  Struct() = default;

  base::vector<Field> fields_;
  base::unordered_map<std::string, size_t> field_indices_;

  mutable std::mutex mtx_;
  mutable ir::Func *init_func_ = nullptr, *assign_func = nullptr,
                   *destroy_func_ = nullptr, *repr_func_ = nullptr;
  // TODO This probably doesn't need any thread safety guarantees, but I'm not
  // sure yet.
  mutable std::atomic<ast::StructLiteral *> to_be_completed_ = nullptr;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
