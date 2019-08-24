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
         absl::Span<std::tuple<std::string_view, type::Type const *> const>);

  ~Struct() override {}
  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;
  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

#include "visitor/type_visitors.xmacro.h"

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string_view name) const;

  ::Module const *defining_module() const { return mod_; }

  core::Bytes offset(size_t n, core::Arch const &arch) const;

  std::vector<Field> const &fields() const { return fields_; }
  size_t index(std::string_view name) const;

  bool contains_hashtag(ast::Hashtag needle) const;

  core::Scope const *scope_         = nullptr;
  ::Module *mod_                    = nullptr;

  // `init_func_` is generated in FinalizeStruct.
  //
  // TODO I'm not sure if this needs locking. If the threading model turns out
  // to be 1 per module, then probably not. By generating this in init_func_, we
  // guarantee that no one really had access to this struct, but maybe they got
  // a pointer to it before it was completely initialized and tried to
  // dereference ad use it? We can definitely hand out pointers before
  // FinalizeStruct is called because that's how we do recursive types.
  ir::AnyFunc init_func_;

  base::lazy<ir::AnyFunc> destroy_func_;
  base::lazy<ir::AnyFunc> copy_assign_func_;
  base::lazy<ir::AnyFunc> move_assign_func_;

  std::vector<ast::Hashtag> hashtags_;
  std::vector<Field> fields_;
  absl::flat_hash_map<std::string, size_t> field_indices_;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
