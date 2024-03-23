#include "common/string_literal.h"

#include "common/constant/category.h"
#include "common/constant/entry.h"
#include "common/constant/manifest.h"
#include "common/internal/strings.h"

namespace ic {

StringLiteral::StringLiteral() : StringLiteral(std::string("")) {}

StringLiteral::StringLiteral(std::string &&s) {
  auto [iter, inserted] =
      internal_common::Strings().try_emplace(std::move(s), ConstantManifest::Global().NextSlot());
  if (inserted) {
    InsertIntoGlobalConstantManifest(ConstantCategory::String,
                                     internal_common::Strings().index(iter));
  }
  mutable_value() = iter->second.value();
}

StringLiteral::StringLiteral(std::string const &s) {
  auto [iter, inserted] = internal_common::Strings().try_emplace(
      s, ConstantManifest::Global().NextSlot());
  if (inserted) {
    InsertIntoGlobalConstantManifest(ConstantCategory::String,
                                     internal_common::Strings().index(iter));
  }
  mutable_value() = iter->second.value();
}

StringLiteral::StringLiteral(std::string_view s)
    : StringLiteral(std::string(s)) {}

StringLiteral::operator std::string const &() const {
  return internal_common::Strings()
      .from_index(ConstantManifest::Global()[value()].value())
      .first;
}

}  // namespace ic
