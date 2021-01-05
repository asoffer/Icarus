#include "ir/value/string.h"

#include "absl/container/node_hash_map.h"
#include "base/no_destructor.h"
#include "core/alignment.h"
#include "core/arch.h"
#include "core/bytes.h"
#include "ir/read_only_data.h"
#include "ir/value/char.h"

namespace ir {
namespace {

// Lock is not necessary, as all accesess are guarded by the lock on
// `ReadOnlyData`.
base::NoDestructor<absl::node_hash_map<std::string, Addr>> GlobalStringSet;

Addr SaveStringGlobally(std::string const& str) {
  auto rodata_handle   = ReadOnlyData.lock();
  auto [iter, success] = GlobalStringSet->emplace(str, Addr::ReadOnly(0));
  if (not success) { return iter->second; }

  // TODO: This means we're storing strings during lexing even if we never
  // intend to use them later. I doubt that's a huge performance loss, but it's
  // worth remembering and seeing if there's an easy way to fix it.
  size_t buf_end = rodata_handle->size();
  core::Bytes slice_start =
      core::FwdAlign(core::Bytes(buf_end), core::Alignment::Get<Slice>());
  core::Bytes data_start = slice_start + core::Bytes::Get<Slice>();
  core::Bytes data_end =
      data_start + core::Bytes(str.size() + 1);  // +1 for the null terminator.

  rodata_handle->append_bytes(data_end.value() - buf_end);
  rodata_handle->set(slice_start.value(),
                     Slice(Addr::ReadOnly(data_start.value()), str.size()));

  size_t i = data_start.value();
  for (char c : str) {
    Char ch(c);
    std::memcpy(rodata_handle->raw(i++), &ch, sizeof(ch));
  }
  Char ch('\0');
  std::memcpy(rodata_handle->raw(i++), &ch, sizeof(ch));

  return iter->second = Addr::ReadOnly(slice_start.value());
}

}  // namespace

String::String(char const* str) : String(std::string(str)) {}
String::String(std::string_view str) : String(std::string(str)) {}
String::String(std::string const& str) : addr_(SaveStringGlobally(str)) {}

std::string String::get() const {
  std::string result;
  {
    auto handle = ReadOnlyData.lock();
    result      = handle->raw((addr_ + core::Bytes::Get<Slice>()).rodata());
  }
  return result;
}

Slice String::slice() const {
  return ReadOnlyData.lock()->get<ir::Slice>(addr_.rodata());
}

std::ostream& operator<<(std::ostream& os, String s) { return os << s.get(); }

}  // namespace ir
