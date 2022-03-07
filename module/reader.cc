#include "module/reader.h"

#include "base/debug.h"
#include "type/primitive.h"
#include "type/serialize.h"

namespace module {

absl::Span<std::byte const> ModuleReader::read_bytes(size_t num_bytes) {
  ASSERT(head_ + num_bytes < end_);
  std::byte const* p = head_;
  head_ += num_bytes;
  return absl::MakeConstSpan(p, num_bytes);
}

bool ModuleReader::read(std::string& s) {
  size_t n;
  if (not base::Deserialize(*this, n)) { return false; }
  s.reserve(n);
  auto span = read_bytes(n);
  s         = std::string_view(reinterpret_cast<char const*>(span.data()), n);
  return true;
}

bool ModuleReader::read(Module::SymbolInformation& info) {
  if (not base::Deserialize(*this, info.qualified_type)) { return false; }
  ssize_t num_read = type::DeserializeValue(
      info.qualified_type.type(), absl::MakeConstSpan(head_, end_ - head_),
      info.value, context_.foreign_function_map(), system_);
  if (num_read < 0) { return false; }
  head_ += num_read;
  return true;
}

bool ModuleReader::read(type::QualType& qt) {
  auto quals = type::Quals::Unqualified();
  size_t type_index;
  if (not base::Deserialize(*this, quals, type_index)) { return false; }
  qt = type::QualType(type::GlobalTypeSystem.from_index(type_index), quals);
  return true;
}

bool ModuleReader::read(type::Type& t) { NOT_YET(); }

}  // namespace module