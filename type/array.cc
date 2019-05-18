#include "type/array.h"

#include "base/guarded.h"
#include "base/tuple.h"
#include "core/fn_params.h"
#include "core/arch.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

static base::guarded<
    absl::flat_hash_map<Type const *, absl::flat_hash_map<size_t, Array *>>>
    fixed_arrays_;
Array const *Arr(size_t len, Type const *t) {
  auto handle = fixed_arrays_.lock();
  return (*handle)[t].emplace(len, new Array(len, t)).first->second;
}

void Array::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  data_type->defining_modules(modules);
}

void Array::WriteTo(std::string *result) const {
  result->append("[");
  result->append(std::to_string(len));
  Type const *t = data_type;
  while (auto *array_ptr = t->if_as<Array>()) {
    result->append(", ");
    result->append(std::to_string(array_ptr->len));
    t = array_ptr->data_type;
  }
  result->append("; ");
  t->WriteTo(result);
  result->append("]");
}

bool Array::IsCopyable() const { return data_type->IsCopyable(); }
bool Array::IsMovable() const { return data_type->IsMovable(); }

core::Bytes Array::bytes(core::Arch const &a) const {
  return core::FwdAlign(data_type->bytes(a), data_type->alignment(a)) * len;
}

core::Alignment Array::alignment(core::Arch const &a) const {
  return data_type->alignment(a);
}

bool Array::ReinterpretAs(Type const *t) const {
  if (auto *a = t->if_as<Array>()) {
    return len == a->len && data_type->ReinterpretAs(a->data_type);
  }
  return false;
}

// TODO arrays are tricky because they may contain structs and so just using the
// result of this function is... maybe not what you intended.
Cmp Array::Comparator() const { return data_type->Comparator(); }

}  // namespace type
