#include "ir/str.h"

#include "absl/container/node_hash_map.h"
#include "base/guarded.h"
#include "base/untyped_buffer.h"

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
namespace backend {
extern base::untyped_buffer ReadOnlyData;
}  // namespace backend
#endif // ICARUS_VISITOR_EMIT_IR 

namespace ir {

// TODO avoid double-storing the string.
static base::guarded<absl::node_hash_map<std::string, Addr>> GlobalStringSet;
std::string_view SaveStringGlobally(std::string const &str) {
  auto handle         = GlobalStringSet.lock();
  auto[iter, success] = handle->emplace(str, Addr::ReadOnly(0));
  if (!success) { return iter->first; }

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
  size_t buf_end = backend::ReadOnlyData.size();
  backend::ReadOnlyData.append_bytes(
      str.size() + 1,  // +1 for the null terminator.
      alignof(char));
  std::memcpy(backend::ReadOnlyData.raw(buf_end), str.data(), str.size() + 1);
  iter->second = Addr::ReadOnly(buf_end);
#endif // ICARUS_VISITOR_EMIT_IR 

  return iter->first;
}

Addr GetString(std::string const &str) {
  return GlobalStringSet.lock()->find(str)->second;
}

Addr GetString(std::string_view str) { return GetString(std::string{str}); }

}  // namespace ir
