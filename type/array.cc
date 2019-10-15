#include "type/array.h"

#include "base/guarded.h"
#include "base/tuple.h"
#include "core/arch.h"
#include "core/fn_params.h"
#include "module/module.h"
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

core::Bytes Array::bytes(core::Arch const &a) const {
  return core::FwdAlign(data_type->bytes(a), data_type->alignment(a)) * len;
}

core::Alignment Array::alignment(core::Arch const &a) const {
  return data_type->alignment(a);
}

}  // namespace type
