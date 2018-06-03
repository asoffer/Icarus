#include "type/struct.h"

#include <set>
#include "base/guarded.h"

namespace std {
template <> struct less<type::Struct> {
  size_t operator()(const type::Struct &lhs, const type::Struct &rhs) {
    return lhs.fields_ < rhs.fields_;
  }
};
} // namespace std

namespace type {
static bool operator<(const Struct::Field &lhs, const Struct::Field &rhs) {
  if (lhs.name < rhs.name) { return true; }
  if (lhs.name > rhs.name) { return false; }
  if (lhs.type < rhs.type) { return true; }
  if (lhs.type > rhs.type) { return false; }
  return false;

  // TODO compare initial values
  // return lhs.init_val < rhs.init_val;
}

static base::guarded<std::set<Struct>> structs_;
const Struct::Field *Struct::field(const std::string &name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

const Type *Struct::finalize() {
  const Struct *interned =
      &*structs_.lock()
            ->emplace(std::move(fields_), std::move(field_indices_))
            .first;

  delete this;
  return interned;
}
} // namespace type
