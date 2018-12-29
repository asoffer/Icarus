#include "type/incomplete_flags.h"

#include "module.h"
#include "type/flags.h"

namespace type {
IncompleteFlags::IncompleteFlags(::Module const *mod) : mod_(mod) {}

void IncompleteFlags::add(std::string_view s) {
  entries_.emplace_back(std::string(s), std::nullopt);
}
void IncompleteFlags::set_last_value(i32 value) {
  entries_.back().second = value;
}

Flags const *IncompleteFlags::finalize() && {
  std::vector<i32> available;
  available.reserve(32);
  for (i32 i = 0; i < 32; ++i) { available.push_back(i); }

  auto *f = new Flags(mod_);
  for (auto const & [ s, v ] : entries_) {
    if (v.has_value()) {
      f->vals_.emplace(s, ir::FlagsVal(size_t{1} << *v));
      f->members_.emplace(size_t{1} << *v, s);
      available[*v] = available.back();
      available.pop_back();
      f->All |= size_t{1} << *v;
    }
  }
  // TODO we can highly optimize this in a number of ways. One simple thing is
  // removing members as we used them above.

  std::random_device rd;
  std::shuffle(available.begin(), available.end(), std::mt19937(rd()));
  for (auto const & [ s, v ] : entries_) {
    if (v.has_value()) { continue; }
    i32 x = available.back();
    f->vals_.emplace(s, ir::FlagsVal(size_t{1} << x));
    f->members_.emplace(size_t{1} << x, s);
    available.pop_back();
    f->All |= size_t{1} << x;
  }
  return f;
}

}  // namespace type
