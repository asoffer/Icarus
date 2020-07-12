#include "type/variant.h"

#include <algorithm>

#include "absl/algorithm/container.h"
#include "base/global.h"
#include "type/primitive.h"
#include "type/tuple.h"

namespace type {

bool Variant::contains(Type const *t) const {
  // TODO can do a binary search.
  for (auto *v : variants_) {
    if (v == t) { return true; }
  }
  return false;
}

Variant::Variant(std::vector<Type const *> variants)
    : Type(Type::Flags{.is_default_initializable = 0,
                       .is_copyable              = 1,
                       .is_movable               = 1,
                       .has_destructor           = 0}),
      variants_(std::move(variants)) {
  for (auto const *variant : variants_) {
    flags_.is_default_initializable &= variant->IsDefaultInitializable();
    flags_.is_copyable &= variant->IsCopyable();
    flags_.is_movable &= variant->IsMovable();
    flags_.has_destructor |= variant->HasDestructor();
  }
}

static base::Global<std::map<std::vector<Type const *>, Variant>> all_variants_;
Type const *Var(std::vector<Type const *> variants) {
  if (variants.empty()) { return Void(); }
  if (variants.size() == 1) { return variants[0]; }

  size_t end = variants.size();
  size_t i   = 0;
  while (i < end) {
    if (variants[i]->is<Variant>()) {
      Variant const *var = &variants[i]->as<Variant>();
      variants[i]        = variants.back();
      variants.pop_back();
      variants.insert(variants.end(), var->variants_.begin(),
                      var->variants_.end());
    } else {
      ++i;
    }
  }

  // TODO This sort order should be deterministic to allow interoperability
  // between multiple runs of the compiler.

  std::sort(variants.begin(), variants.end());
  variants.erase(std::unique(variants.begin(), variants.end()), variants.end());

  if (variants.size() == 1) { return variants.front(); }

  return &all_variants_.lock()
              ->emplace(std::piecewise_construct,
                        std::forward_as_tuple(variants),
                        std::forward_as_tuple(variants))
              .first->second;
}

void Variant::WriteTo(std::string *result) const {
  auto iter = variants_.begin();

  // TODO Will you ever need parentheses?
  (*iter)->WriteTo(result);

  ++iter;
  for (; iter != variants_.end(); ++iter) {
    result->append(" | ");
    (*iter)->WriteTo(result);
  }
}

core::Bytes Variant::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  auto align     = core::Alignment{1};
  for (auto const *t : variants_) {
    align     = std::max(align, t->alignment(a));
    num_bytes = std::max(num_bytes, t->bytes(a));
  }
  return core::FwdAlign(Type_->bytes(a), align) + num_bytes;
}

core::Alignment Variant::alignment(core::Arch const &a) const {
  auto align = Type_->alignment(a);
  for (auto const *t : variants_) { align = std::max(align, t->alignment(a)); }
  return align;
}

core::Alignment Variant::alternative_alignment(core::Arch const &a) const {
  core::Alignment align;
  for (auto const *t : variants_) { align = std::max(align, t->alignment(a)); }
  return align;
}

template <typename Container>
std::vector<Type const *> MultiVarImpl(absl::Span<Container const> type_vecs) {
  if (type_vecs.empty()) { return {}; }

#if defined(ICARUS_DEBUG)
  size_t min_size = std::numeric_limits<size_t>::max(), max_size = 0;
  for (auto const &vec : type_vecs) {
    min_size = std::min(min_size, vec.size());
    max_size = std::max(max_size, vec.size());
  }

  ASSERT(min_size == max_size);
#endif  // defined(ICARUS_DEBUG)

  size_t num = type_vecs[0].size();
  std::vector<type::Type const *> vars;
  vars.reserve(num);
  for (size_t i = 0; i < num; ++i) {
    std::vector<type::Type const *> var_entry;
    var_entry.reserve(type_vecs.size());
    for (auto const &v : type_vecs) { var_entry.push_back(v[i]); }
    vars.push_back(type::Var(std::move(var_entry)));
  }

  return vars;
}

std::vector<Type const *> MultiVar(
    absl::Span<std::vector<Type const *> const> type_vecs) {
  return MultiVarImpl(type_vecs);
}

std::vector<Type const *> MultiVar(
    absl::Span<absl::Span<Type const *const> const> type_vecs) {
  return MultiVarImpl(type_vecs);
}

}  // namespace type
