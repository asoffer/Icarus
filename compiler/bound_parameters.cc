#include "compiler/bound_parameters.h"

namespace compiler {

BoundParameters::BoundParameters(
    std::vector<core::Param<type::QualType>> qts,
    absl::Span<ir::CompleteResultBuffer const> buffers)
    : types_(std::move(qts)) {
  for (auto const &buffer : buffers) {
    if (buffer.empty()) {
      buffer_.append();
    } else {
      buffer_.append(buffer);
    }
  }
  ASSERT(types_.size() == buffer_.num_entries());

  for (size_t i = 0; i < types_.size(); ++i) {
    ASSERT(types_[i].value.constant() != buffer_[i].empty());
  }

}

void BoundParameters::append(ir::CompleteResultRef const &ref,
                             core::Param<type::QualType> const &param) {
  ASSERT(param.value.constant() != ref.empty());
  types_.append(param);
  buffer_.append(ref);
}

bool operator==(BoundParameters const &lhs, BoundParameters const &rhs) {
  if (lhs.types_ != rhs.types_) { return false; }
  for (size_t i = 0; i < lhs.types_.size(); ++i) {
    auto qt = lhs.types_[i].value;
    if (not qt.constant()) { continue; }
    if (not qt.type().EqualsValue(lhs.buffer_[i], rhs.buffer_[i])) {
      return false;
    }
  }
  return true;
}

}  // namespace compiler
