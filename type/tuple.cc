#include "type/tuple.h"

#include <utility>

#include "base/global.h"
#include "core/arch.h"
#include "type/function.h"
#include "type/pointer.h"

// TODO Currently order of init/move/copy/destroy is entirely randomized. This
// may not be a good design.

namespace type {
Type Void() { return Tup({}); }

static base::Global<absl::node_hash_map<std::vector<Type>, Tuple const>> tups_;
Type Tup(std::vector<Type> entries) {
  if (entries.size() == 1) { return entries[0]; }
  auto handle = tups_.lock();
  auto [iter, success] =
      handle->emplace(std::piecewise_construct, std::forward_as_tuple(entries),
                      std::forward_as_tuple(entries));
  return &iter->second;
}

core::Bytes Tuple::offset(size_t field_num, core::Arch const &a) const {
  auto offset = core::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += entries_[i].bytes(a);
    offset = core::FwdAlign(offset, entries_[i + 1].alignment(a));
  }
  return offset;
}

void Tuple::WriteTo(std::string *result) const {
  std::string_view sep = "";

  result->append("(");
  for (Type t : entries_) {
    result->append(std::exchange(sep, ", "));
    t.get()->WriteTo(result);
  }
  result->append(")");
}

Tuple::Tuple(std::vector<Type> entries)
    : LegacyType(LegacyType::Flags{.is_default_initializable = 1,
                                   .is_copyable              = 1,
                                   .is_movable               = 1,
                                   .has_destructor           = 0}),
      entries_(std::move(entries)) {
  for (Type entry : entries_) {
    flags_.is_default_initializable &= entry.get()->IsDefaultInitializable();
    flags_.is_copyable &= entry.get()->IsCopyable();
    flags_.is_movable &= entry.get()->IsMovable();
    flags_.has_destructor |= entry.get()->HasDestructor();
  }
}

core::Bytes Tuple::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  for (Type t : entries_) {
    num_bytes += t.bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = core::FwdAlign(num_bytes, t.alignment(a));
  }

  return num_bytes;
}

core::Alignment Tuple::alignment(core::Arch const &a) const {
  auto align = core::Alignment{1};
  for (Type t : entries_) { align = std::max(align, t.alignment(a)); }
  return align;
}

}  // namespace type
