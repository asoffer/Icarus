#include "ir/value/string.h"

#include "absl/container/node_hash_map.h"
#include "base/no_destructor.h"
#include "ir/read_only_data.h"

namespace ir {
namespace {

// Lock is not necessary, as all accesess are guarded by the lock on
// `ReadOnlyData`.
base::NoDestructor<absl::node_hash_map<std::string, Addr>> GlobalStringSet;

Addr SaveStringGlobally(std::string const& str) {
  auto rodata_handle   = ReadOnlyData.lock();
  auto [iter, success] = GlobalStringSet->emplace(str, Addr::ReadOnly(0));
  if (not success) { return iter->second; }

  // TODO This means we're storing strings during lexing even if we never intend
  // to use them later. I doubt that's a huge performance loss, but it's worth
  // remembering and seeing if there's an easy way to fix it.
  size_t buf_end = rodata_handle->size();
  rodata_handle->append_bytes(str.size() + 1);  // +1 for the null terminator.
  std::memcpy(rodata_handle->raw(buf_end), str.data(), str.size() + 1);
  iter->second = Addr::ReadOnly(buf_end);

  return iter->second;
}

}  // namespace

String::String(char const* str) : String(std::string(str)) {}
String::String(std::string_view str) : String(std::string(str)) {}
String::String(std::string const& str)
    : addr_(SaveStringGlobally(str)) {}

std::string String::get() const {
  std::string result;
  {
    auto handle = ReadOnlyData.lock();
    result      = handle->raw(addr_.rodata());
  }
  return result;
}

std::ostream& operator<<(std::ostream& os, String s) { return os << s.get(); }

}  // namespace ir
