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

bool ModuleReader::read(Module::SymbolInformation& info) {
  if (not base::Deserialize(*this, info.qualified_type)) { return false; }
  ssize_t num_read = type::DeserializeValue(
      info.qualified_type.type(), absl::MakeConstSpan(head_, end_ - head_),
      info.value);
  if (num_read < 0) { return false; }
  head_ += num_read;
  return true;
}

bool ModuleReader::read(type::QualType& qt) {
  auto quals = type::Quals::Unqualified();
  if (not base::Deserialize(*this, quals)) { return false; }
  ir::CompleteResultBuffer buffer;

  ssize_t num_read = type::DeserializeValue(
      type::Type_, absl::MakeConstSpan(head_, end_ - head_), buffer);
  if (num_read < 0) { return false; }
  head_ += num_read;
  ASSERT(buffer.num_entries() == 1);
  qt = type::QualType(buffer[0].get<type::Type>(), quals);
  return true;
}

}  // namespace module
