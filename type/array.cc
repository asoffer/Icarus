#include "type/array.h"

#include "absl/strings/str_format.h"
#include "base/guarded.h"
#include "base/no_destructor.h"

namespace type {

static base::NoDestructor<base::guarded<absl::node_hash_set<Array>>> cache;
Array const *Arr(size_t len, Type const *t) {
  return &*cache->lock()->insert(Array(len, t)).first;
}

void Array::WriteTo(std::string *result) const {
  absl::StrAppendFormat(result, "[%u", length());
  Type const *t = data_type();
  while (auto *array_ptr = t->if_as<Array>()) {
    absl::StrAppendFormat(result, ", %d", array_ptr->length());
    t = array_ptr->data_type();
  }
  result->append("; ");
  t->WriteTo(result);
  result->append("]");
}

core::Bytes Array::bytes(core::Arch const &a) const {
  return core::FwdAlign(data_type()->bytes(a), data_type()->alignment(a)) *
         length();
}

core::Alignment Array::alignment(core::Arch const &a) const {
  return data_type()->alignment(a);
}

}  // namespace type
