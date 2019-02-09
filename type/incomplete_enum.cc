#include "type/incomplete_enum.h"

#include <unordered_set>
#include <random>
#include <utility>

#include "misc/module.h"
#include "type/enum.h"

namespace type {

IncompleteEnum::IncompleteEnum(::Module const *mod) : mod_(mod) {}

void IncompleteEnum::add(std::string_view s) {
  entries_.emplace_back(std::string(s), std::nullopt);
}
void IncompleteEnum::set_last_value(int32_t value) {
  entries_.back().second = value;
}

Enum const *IncompleteEnum::finalize() && {
  std::unordered_set<int32_t> taken;
  auto *e = new Enum(mod_);
  for (auto const & [ s, v ] : entries_) {
    if (v.has_value()) {
      e->vals_.emplace(s, ir::EnumVal(*v));
      e->members_.emplace(*v, s);
    }
    taken.insert(*v);
  }
  // TODO we can highly optimize this in a number of ways. One simple thing is
  // removing members as we used them above.
  for (auto const & [ s, v ] : entries_) {
    if (v.has_value()) { continue; }
    std::random_device rd;
    std::uniform_int_distribution<int> dist(std::numeric_limits<int32_t>::lowest(),
                                            std::numeric_limits<int32_t>::max());
    int32_t x;
    {
    try_again:
      x            = dist(rd);
      bool success = taken.insert(x).second;
      if (!success) { goto try_again; }
    }
    e->vals_.emplace(s, ir::EnumVal(x));
    e->members_.emplace(x, s);
  }
  return e;
}

}  // namespace type
