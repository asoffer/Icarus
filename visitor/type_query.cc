#include "visitor/type_query.h"

#include "absl/algorithm/container.h"
#include "ast/declaration.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace visitor {

bool TypeQuery::IsDefaultInitializable(type::Struct const *t) {
  return absl::c_all_of(t->fields_,
                        [](type::Struct::Field const &field) {
                          return field.type->IsDefaultInitializable();
                        }) &&
         absl::c_none_of(t->hashtags_, [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::NoDefault;
         });
}

bool TypeQuery::IsDefaultInitializable(type::Enum const *t) { return false; }
bool TypeQuery::IsDefaultInitializable(type::Opaque const *t) { return false; }
bool TypeQuery::IsDefaultInitializable(type::Variant const *t) { return false; }

bool TypeQuery::IsCopyable(type::Array const *t) {
  return t->data_type->IsCopyable();
}

bool TypeQuery::IsCopyable(type::Struct const *t) {
  return absl::c_all_of(t->fields_,
                        [](type::Struct::Field const &field) {
                          return field.type->IsCopyable();
                        }) &&
         absl::c_none_of(t->hashtags_, [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::Uncopyable;
         });
}

bool TypeQuery::IsCopyable(type::Tuple const *t) {
  return absl::c_all_of(t->entries_,
                        [](type::Type const *t) { return t->IsCopyable(); });
}

bool TypeQuery::IsCopyable(type::Variant const *t) {
  return absl::c_all_of(t->variants_,
                        [](type::Type const *t) { return t->IsCopyable(); });
}

bool TypeQuery::IsMovable(type::Array const *t) {
  return t->data_type->IsMovable();
}

bool TypeQuery::IsMovable(type::Struct const *t) {
  return absl::c_all_of(t->fields_,
                        [](type::Struct::Field const &field) {
                          return field.type->IsMovable();
                        }) &&
         absl::c_none_of(t->hashtags_, [](ast::Hashtag tag) {
           return tag.kind_ == ast::Hashtag::Builtin::Immovable;
         });
}

bool TypeQuery::IsMovable(type::Tuple const *t) {
  return absl::c_all_of(t->entries_,
                        [](type::Type const *t) { return t->IsMovable(); });
}

bool TypeQuery::IsMovable(type::Variant const *t) {
  return absl::c_all_of(t->variants_,
                        [](type::Type const *t) { return t->IsMovable(); });
}

bool TypeQuery::HasDestructor(type::Array const *t) {
  return t->data_type->HasDestructor();
}

bool TypeQuery::HasDestructor(type::Struct const *t) {
  // TODO is this okay? Does it work for generics? Does it need to?
  Context ctx(t->mod_);
  for (auto const *decl : t->scope_->AllDeclsWithId("~")) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = ctx.type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<type::Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != type::Ptr(t)) { continue; }
  }

  return absl::c_any_of(t->fields_, [](type::Struct::Field const &field) {
    return field.type->HasDestructor();
  });
  // TODO Consider adding a #{no-destroy} hashtag?
}

bool TypeQuery::HasDestructor(type::Tuple const *t) {
  return absl::c_any_of(t->entries_,
                        [](type::Type const *t) { return t->HasDestructor(); });
}

bool TypeQuery::HasDestructor(type::Variant const *t) {
  return absl::c_any_of(t->variants_,
                        [](type::Type const *t) { return t->HasDestructor(); });
}

}  // namespace visitor
