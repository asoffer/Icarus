#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <mutex>
#include <string>
#include <string_view>
#include "ast/hashtag.h"
#include "base/container/vector.h"
#include "ir/val.h"
#include "scope.h"
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
    Type const *type = nullptr;
    ir::Val init_val;
    base::vector<ast::Hashtag> hashtags_;
  };

  Struct(::Scope const *scope, ::Module const *mod)
      : scope_(scope), mod_(mod) {}
  ~Struct() override {}
  BASIC_METHODS;

  void EmitDestroy(ir::Register reg, Context *ctx) const override;

  void set_last_name(std::string_view s);
  void add_hashtag(ast::Hashtag hashtag);
  void add_hashtag_to_last_field(ast::Hashtag hashtag);
  void add_field(type::Type const *t);

  bool IsCopyable() const override;
  bool IsDefaultInitializable() const override;

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string const &name) const;

  bool needs_destroy() const override;

  ::Module const *defining_module() const { return mod_; }

  size_t offset(size_t n, Architecture const &arch) const;

  base::vector<Field> const &fields() const { return fields_; }
  size_t index(std::string const &name) const;

 private:
  ::Scope const *scope_ = nullptr;
  ::Module const *mod_ = nullptr;
  base::vector<ast::Hashtag> hashtags_;
  base::vector<Field> fields_;
  base::unordered_map<std::string, size_t> field_indices_;

  mutable std::mutex mtx_;
  mutable ir::Func *init_func_ = nullptr, *assign_func_ = nullptr,
                   *repr_func_ = nullptr;
  mutable ir::AnyFunc destroy_func_{nullptr};
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
