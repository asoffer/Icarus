#include "type/struct.h"

#include "core/arch.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

core::Bytes Struct::offset(size_t field_num, core::Arch const &a) const {
  auto offset = core::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += fields_.at(i).type->bytes(a);
    offset = core::FwdAlign(offset, fields_.at(i + 1).type->alignment(a));
  }
  return offset;
}

size_t Struct::index(std::string const &name) const {
  return field_indices_.at(name);
}

Struct::Field const *Struct::field(std::string const &name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

void Struct::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

void Struct::set_last_name(std::string_view s) {
  fields_.back().name = std::string(s);
  auto[iter, success] =
      field_indices_.emplace(fields_.back().name, fields_.size() - 1);
  static_cast<void>(iter);
  ASSERT(success == true);
}

void Struct::add_hashtag(ast::Hashtag hashtag) { hashtags_.push_back(hashtag); }

void Struct::add_hashtag_to_last_field(ast::Hashtag hashtag) {
  fields_.back().hashtags_.push_back(hashtag);
}

void Struct::add_field(Type const *t) { fields_.emplace_back(t); }

bool Struct::IsDefaultInitializable() const {
  return std::all_of(fields_.begin(), fields_.end(),
                     [](Field const &field) {
                       return field.type->IsDefaultInitializable();
                     }) &&
         std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::NoDefault;
         });
}

bool Struct::IsCopyable() const {
  return std::all_of(
             fields_.begin(), fields_.end(),
             [](Field const &field) { return field.type->IsCopyable(); }) &&
         std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::Uncopyable;
         });
}

bool Struct::IsMovable() const {
  return std::all_of(
             fields_.begin(), fields_.end(),
             [](Field const &field) { return field.type->IsMovable(); }) &&
         std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::Immovable;
         });
}

bool Struct::needs_destroy() const {
  /*
  // TODO this depends on whether or not a destructor has been defined, so it
  // requires IR and therefore shouldn't be on the type explicitly. Move it to a
  // visitor.
  //
  // TODO is this okay? Does it work for generics? Does it need to?
  Context ctx(mod_);
  for (auto const *decl : scope_->AllDeclsWithId("~")) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = ctx.type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != Ptr(this)) { continue; }
  }
  */
  return absl::c_any_of(fields_,
                        [](Field const &f) { return f.type->needs_destroy(); });
}

void Struct::WriteTo(std::string *result) const {
  result->append("struct.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

bool Struct::contains_hashtag(ast::Hashtag needle) const {
  for (auto const &tag : hashtags_) {
    if (tag.kind_ == needle.kind_) { return true; }
  }
  return false;
}

core::Bytes Struct::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  for (auto const &field : fields_) {
    num_bytes += field.type->bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = core::FwdAlign(num_bytes, field.type->alignment(a));
  }

  return num_bytes;
}

core::Alignment Struct::alignment(core::Arch const &a) const {
  auto align = core::Alignment{1};
  for (auto const &field : fields_) {
    align = std::max(align, field.type->alignment(a));
  }
  return align;
}

Cmp Struct::Comparator() const { return Cmp::None; }

bool Struct::ReinterpretAs(Type const *t) const { return t == this; }
}  // namespace type
