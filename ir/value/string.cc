#include "ir/value/string.h"

#include "absl/container/node_hash_map.h"
#include "ir/read_only_data.h"

namespace ir {
namespace {

base::guarded<absl::node_hash_map<std::string, Addr>> GlobalStringSet;
Addr SaveStringGlobally(std::string const &str) {
  auto handle          = GlobalStringSet.lock();
  auto [iter, success] = handle->emplace(str, Addr::ReadOnly(0));
  if (not success) { return iter->second ; }

  // TODO This means we're storing strings during lexing even if we never intend
  // to use them later. I doubt that's a huge performance loss, but it's worth
  // remembering and seeing if there's an easy way to fix it.
  size_t buf_end = ReadOnlyData.size();
  ReadOnlyData.append_bytes(str.size() + 1);  // +1 for the null terminator.
  std::memcpy(ReadOnlyData.raw(buf_end), str.data(), str.size() + 1);
  iter->second = Addr::ReadOnly(buf_end);

  return iter->second;
}

}  // namespace

String::String(char const* str) : String(std::string(str)) {}
String::String(std::string_view str) : String(std::string(str)) {}
String::String(std::string const& str)
    : addr_(SaveStringGlobally(str)) {}

std::string String::get() const { return ReadOnlyData.raw(addr_.rodata()); }

std::ostream& operator<<(std::ostream& os, String s) { return os << s.get(); }

}  // namespace ir
