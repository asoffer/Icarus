#include "visitor/type_query.h"

#include "absl/algorithm/container.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/opaque.h"
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

}  // namespace visitor
