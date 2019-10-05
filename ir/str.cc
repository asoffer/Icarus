#include "ir/str.h"

#include "absl/container/node_hash_map.h"
#include "base/guarded.h"
#include "base/untyped_buffer.h"
#include "ir/read_only_data.h"

namespace ir {

// TODO avoid double-storing the string.
static base::guarded<absl::node_hash_map<std::string, Addr>> GlobalStringSet;
std::string_view SaveStringGlobally(std::string const &str) {
  auto handle         = GlobalStringSet.lock();
  auto[iter, success] = handle->emplace(str, Addr::ReadOnly(0));
  if (!success) { return iter->first; }

  // TODO This means we're storing strings during lexing even if we never intend
  // to use them later. I doubt that's a huge performance loss, but it's worth
  // remembering and seeing if there's an easy way to fix it.
  size_t buf_end = ReadOnlyData.size();
  ReadOnlyData.append_bytes(
      str.size() + 1,  // +1 for the null terminator.
      alignof(char));
  std::memcpy(ReadOnlyData.raw(buf_end), str.data(), str.size() + 1);
  iter->second = Addr::ReadOnly(buf_end);

  return iter->first;
}

Addr GetString(std::string const &str) {
  return GlobalStringSet.lock()->find(str)->second;
}

Addr GetString(std::string_view str) { return GetString(std::string{str}); }

}  // namespace ir
