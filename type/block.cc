#include "type/block.h"

#include "absl/container/node_hash_map.h"
#include "base/global.h"

namespace type {

static base::Global<absl::node_hash_map<core::Parameters<QualType>, Block>>
    blocks_;
Block const *Blk(core::Parameters<QualType> in) {
  auto f      = Block(in);
  auto handle = blocks_.lock();
  auto const &[iter, inserted] =
      handle->try_emplace(std::move(in), std::move(f));
  return &iter->second;
}

void Block::WriteTo(std::string *result) const {
  result->append("block (");
  std::string_view sep = "";
  for (auto const &param : parameters()) {
    result->append(sep);
    if (not param.name.empty()) {
      absl::StrAppend(result, param.name,
                      param.value.constant() ? " :: " : ": ");
    }
    param.value.type().get()->WriteTo(result);
    sep = ", ";
  }
  result->append(")");
}

core::Bytes Block::bytes(core::Arch const &a) const {
  return a.pointer().bytes();
}

core::Alignment Block::alignment(core::Arch const &a) const {
  return a.pointer().alignment();
}

void Block::ShowValue(std::ostream &os,
                      ir::CompleteResultRef const &value) const {
  os << "<<block>>";
}

}  // namespace type
