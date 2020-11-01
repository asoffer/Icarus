#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <mutex>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/hashtag.h"
#include "ast/scope/scope.h"
#include "base/lazy.h"
#include "ir/value/fn.h"
#include "ir/value/native_fn.h"
#include "ir/value/value.h"
#include "type/type.h"

namespace ast {
struct StructLiteral;
}  // namespace ast

namespace type {

struct Struct : public LegacyType {
  struct Field {
    // TODO make a string_view but deal with trickiness of moving

    bool contains_hashtag(ast::Hashtag const &h) const {
      for (auto const &tag : hashtags_) {
        if (h == tag) { return true; }
      }
      return false;
    }

    std::string name;
    Type type = nullptr;
    ir::Value initial_value;
    std::vector<ast::Hashtag> hashtags_;
  };

  struct Options {
    uint8_t is_copyable : 1;
    uint8_t is_movable : 1;
  };
  Struct(module::BasicModule const *mod, Options options);
  void AppendFields(std::vector<Field> fields);

  void SetDestructor(ir::Fn dtor);
  ir::Fn Destructor() const;

  void SetMoveAssignment(ir::Fn move_assign);
  ir::Fn MoveAssignment() const;

  void SetCopyAssignment(ir::Fn copy_assign);
  ir::Fn CopyAssignment() const;


  bool is_big() const override { return true; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string_view name) const;

  module::BasicModule const *defining_module() const { return mod_; }

  core::Bytes offset(size_t n, core::Arch const &arch) const;

  absl::Span<Field const> fields() const { return fields_; }
  size_t index(std::string_view name) const;

  bool contains_hashtag(ast::Hashtag needle) const;

  module::BasicModule const *mod_ = nullptr;
  Completeness completeness_      = Completeness::Incomplete;

  std::vector<ast::Hashtag> hashtags_;
  std::vector<Field> fields_;
  std::optional<ir::Fn> init_, user_dtor_, dtor_, copy_assign_, move_assign_;
  absl::flat_hash_map<std::string_view, size_t> field_indices_;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
