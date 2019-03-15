#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <mutex>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/hashtag.h"
#include "base/lazy.h"
#include "core/scope.h"
#include "ir/any_func.h"
#include "type/type.h"

namespace ast {
struct StructLiteral;
}  // namespace ast

namespace type {

struct Struct : public Type {
  struct Field {
    Field(type::Type const *t) : type(t) {}
    // TODO make a string_view but deal with trickiness of moving
    std::string name;
    Type const *type = nullptr;
    std::vector<ast::Hashtag> hashtags_;
  };

  Struct(core::Scope const *scope, ::Module const *mod,
         ast::StructLiteral const *parent)
      : scope_(scope), mod_(const_cast<::Module *>(mod)), parent_(parent) {}
  ~Struct() override {}
  BASIC_METHODS;

  void EmitDestroy(ir::Register reg, Context *ctx) const override;

  void set_last_name(std::string_view s);
  void add_hashtag(ast::Hashtag hashtag);
  void add_hashtag_to_last_field(ast::Hashtag hashtag);
  void add_field(type::Type const *t);

  bool IsCopyable() const override;
  bool IsMovable() const override;
  bool IsDefaultInitializable() const override;

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string const &name) const;

  bool needs_destroy() const override;

  ::Module const *defining_module() const { return mod_; }

  layout::Bytes offset(size_t n, layout::Arch const &arch) const;

  std::vector<Field> const &fields() const { return fields_; }
  size_t index(std::string const &name) const;

  bool contains_hashtag(ast::Hashtag needle) const;

  core::Scope const *scope_         = nullptr;
  ::Module *mod_                    = nullptr;
  ast::StructLiteral const *parent_ = nullptr;

  base::lazy<ir::AnyFunc> init_func_;
  base::lazy<ir::AnyFunc> destroy_func_;
  base::lazy<ir::AnyFunc> copy_assign_func_;
  base::lazy<ir::AnyFunc> move_assign_func_;

  std::vector<ast::Hashtag> hashtags_;
  std::vector<Field> fields_;
  absl::flat_hash_map<std::string, size_t> field_indices_;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
