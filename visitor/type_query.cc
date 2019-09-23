#include "visitor/type_query.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
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
  NOT_YET();
  // TODO Hard because we need to find a corresponding function named "~" which
  // means we need some amount of type-checking.
  //
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

bool TypeQuery::ReinterpretableAs(type::Variant const *from,
                                  type::Type const *to) {
  auto *v = to->if_as<type::Variant>();
  if (!v) { return false; }
  // Every type in this variant needs to be reinterprettable as a type in v
  // exactly once. The problem is this isn't quite enough because the to-type
  // could have another member that's much larger. This violates the
  // size-doesnt-change invariant.
  for (auto *from_v : from->variants_) {
    if (absl::c_count_if(v->variants_, [from_v](type::Type const *to) {
          return from_v->ReinterpretableAs(to);
        }) == 1) {
      continue;
    } else {
      return false;
    }
  }
  return true;
}

bool TypeQuery::ReinterpretableAs(type::Pointer const *from,
                                  type::Type const *to) {
  auto *to_ptr = to->if_as<type::Pointer>();
  if (!to_ptr) { return false; }
  if (to_ptr->is<type::BufferPointer>()) { return false; }
  return from->pointee->ReinterpretableAs(to_ptr->pointee);
}

bool TypeQuery::ReinterpretableAs(type::BufferPointer const *from,
                                  type::Type const *to) {
  if (auto *to_ptr = to->if_as<type::Pointer>()) {
    return from->pointee->ReinterpretableAs(to_ptr->pointee);
  } else {
    return false;
  }
}
bool TypeQuery::ReinterpretableAs(type::Primitive const *from,
                                  type::Type const *to) {
  return to == from || (from == type::NullPtr && to->is<type::Pointer>()) ||
         (from == type::EmptyArray && to->is<type::Array>() &&
          to->as<type::Array>().len == 0);
}

bool TypeQuery::ReinterpretableAs(type::Tuple const *from,
                                  type::Type const *to) {
  auto *tup = to->if_as<type::Tuple>();
  if (!tup || tup->size() != from->size()) { return false; }
  for (size_t i = 0; i < from->size(); ++i) {
    if (!from->entries_.at(i)->ReinterpretableAs(tup->entries_.at(i))) {
      return false;
    }
  }
  return true;
}

bool TypeQuery::ReinterpretableAs(type::Array const *from,
                                  type::Type const *to) {
  if (auto *a = to->if_as<type::Array>()) {
    return from->len == a->len &&
           from->data_type->ReinterpretableAs(a->data_type);
  }
  return false;
}

}  // namespace visitor
