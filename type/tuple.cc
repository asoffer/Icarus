#include "type/tuple.h"

#include <mutex>
#include <utility>

#include "base/guarded.h"
#include "base/permutation.h"
#include "core/arch.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

// TODO Currently order of init/move/copy/destroy is entirely randomized. This
// may not be a good design.

namespace type {
Type const *Void() { return Tup({}); }

bool Tuple::needs_destroy() const {
  return std::any_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->needs_destroy(); });
}

static base::guarded<std::map<std::vector<Type const *>, Tuple const>> tups_;
Type const *Tup(std::vector<Type const *> entries) {
  if (entries.size() == 1) { return entries[0]; }
  auto[iter, success] = tups_.lock()->emplace(std::piecewise_construct,
                                              std::forward_as_tuple(entries),
                                              std::forward_as_tuple(entries));
  return &iter->second;
}

Type const *Tuple::finalize() {
  auto *result = Tup(std::move(entries_));
  ASSERT(this != result);
  delete this;
  return result;
}

core::Bytes Tuple::offset(size_t field_num, core::Arch const &a) const {
  auto offset = core::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += entries_.at(i)->bytes(a);
    offset = core::FwdAlign(offset, entries_.at(i + 1)->alignment(a));
  }
  return offset;
}

void Tuple::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  for (auto *entry : entries_) { entry->defining_modules(modules); }
}

void Tuple::WriteTo(std::string *result) const {
  if (entries_.empty()) {
    result->append("()");
    return;
  }
  result->append("(");
  auto iter = entries_.begin();
  (*iter)->WriteTo(result);
  ++iter;
  for (; iter != entries_.end(); ++iter) {
    result->append(", ");
    (*iter)->WriteTo(result);
  }
  result->append(")");
}

bool Tuple::IsCopyable() const {
  return std::all_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->IsCopyable(); });
}

bool Tuple::IsMovable() const {
  return std::all_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->IsMovable(); });
}

core::Bytes Tuple::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  for (auto const *t : entries_) {
    num_bytes += t->bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = core::FwdAlign(num_bytes, t->alignment(a));
  }

  return num_bytes;
}

core::Alignment Tuple::alignment(core::Arch const &a) const {
  auto align = core::Alignment{1};
  for (auto const *t : entries_) { align = std::max(align, t->alignment(a)); }
  return align;
}

bool Tuple::ReinterpretAs(Type const *t) const {
  auto *tup = t->if_as<Tuple>();
  if (!tup || tup->size() != size()) { return false; }
  for (size_t i = 0; i < size(); ++i) {
    if (!entries_.at(i)->ReinterpretAs(tup->entries_.at(i))) { return false; }
  }
  return true;
}
}  // namespace type
