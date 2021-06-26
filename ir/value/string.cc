#include "ir/value/string.h"

#include "absl/container/node_hash_map.h"
#include "base/guarded.h"
#include "core/alignment.h"
#include "core/arch.h"
#include "core/bytes.h"
#include "ir/read_only_data.h"
#include "ir/value/char.h"

namespace ir {
namespace {

base::guarded<absl::node_hash_map<std::string, Slice>> GlobalStringSet;

addr_t SaveStringGlobally(std::string const& str) {
  auto handle = GlobalStringSet.lock();

  // TODO: This means we're storing strings during lexing even if we never
  // intend to use them later. I doubt that's a huge performance loss, but it's
  // worth remembering and seeing if there's an easy way to fix it.
  auto [iter, success] = handle->try_emplace(str);
  if (success) { iter->second = Slice(Addr(iter->first.data()), str.size()); }
  return Addr(&iter->second);
}

}  // namespace

String::String(char const* str) : String(std::string(str)) {}
String::String(std::string_view str) : String(std::string(str)) {}
String::String(std::string const& str) : addr_(SaveStringGlobally(str)) {}

std::string String::get() const { return std::string(slice()); }

Slice String::slice() const { return *reinterpret_cast<Slice const*>(addr_); }

std::ostream& operator<<(std::ostream& os, String s) { return os << s.get(); }

}  // namespace ir
