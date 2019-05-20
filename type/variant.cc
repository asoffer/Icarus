#include "type/variant.h"

#include <algorithm>

#include "absl/algorithm/container.h"
#include "base/guarded.h"

namespace type {

bool Variant::contains(Type const *t) const {
  // TODO can do a binary search.
  for (auto *v : variants_) {
    if (v == t) { return true; }
  }
  return false;
}

static base::guarded<std::map<std::vector<Type const *>, Variant>>
    all_variants_;
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

bool Variant::needs_destroy() const {
  return std::any_of(variants_.begin(), variants_.end(),
                     [](Type const *t) { return t->needs_destroy(); });
}

void Variant::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  for (auto *v : variants_) { v->defining_modules(modules); }
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
  auto align = core::Alignment{1};
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

bool Variant::ReinterpretAs(Type const *t) const {
  auto *v = t->if_as<Variant>();
  if (!v) { return false; }
  // Every type in this variant needs to be reinterprettable as a type in v
  // exactly once. The problem is this isn't quite enough because the to-type
  // could have another member that's much larger. This violates the
  // size-doesnt-chnage invariant.
  for (auto *this_v : variants_) {
    if (absl::c_count_if(v->variants_, [this_v](Type const *to) {
          return this_v->ReinterpretAs(to);
        }) == 1) {
      continue;
    } else {
      return false;
    }
  }
  return true;
}

}  // namespace type
