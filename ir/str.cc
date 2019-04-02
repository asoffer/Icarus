#include "ir/str.h"

#include "absl/container/node_hash_map.h"
#include "base/guarded.h"

namespace backend {
extern base::untyped_buffer ReadOnlyData;
}  // namespace backend

namespace ir {

// TODO avoid double-storing the string.
static base::guarded<absl::node_hash_map<std::string, Addr>> GlobalStringSet;
std::string_view SaveStringGlobally(std::string const &str) {
  auto handle         = GlobalStringSet.lock();
  auto[iter, success] = handle->emplace(str, Addr::ReadOnly(0));
  if (!success) { return iter->first; }

  size_t buf_end = backend::ReadOnlyData.size();
  backend::ReadOnlyData.append_bytes(
      str.size() + 1,  // +1 for the null terminator.
      alignof(char));
  std::memcpy(backend::ReadOnlyData.raw(buf_end), str.data(), str.size() + 1);
  iter->second = Addr::ReadOnly(buf_end);

  return iter->first;
}

Addr GetString(std::string const &str) {
  return GlobalStringSet.lock()->find(str)->second;
}

}  // namespace ir
