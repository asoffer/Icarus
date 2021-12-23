#include "compiler/bound_parameters.h"

#include "base/debug.h"

namespace compiler {
void BoundParameters::bind_type(ast::Declaration::Id const *id,
                                type::QualType qt) {
  [[maybe_unused]] auto [iter, inserted] =
      bindings_.emplace(id, BindingData{.parameter_type = qt});
  ASSERT(inserted == true);
}

void BoundParameters::bind_value(ast::Declaration::Id const *id,
                                 ir::CompleteResultRef const &ref) {
  auto iter = bindings_.find(id);
  ASSERT(iter != bindings_.end());
  auto &data = iter->second;
  ASSERT(data.index == std::numeric_limits<size_t>::max());
  data.bind(buffer_, ref);
}

bool operator==(BoundParameters const &lhs, BoundParameters const &rhs) {
  if (lhs.bindings_.size() != rhs.bindings_.size()) { return false; }
  auto lhs_iter = lhs.bindings_.begin();
  auto rhs_iter = rhs.bindings_.begin();
  for (; lhs_iter != lhs.bindings_.end(); ++lhs_iter, ++rhs_iter) {
    auto const &[lhs_id, lhs_data] = *lhs_iter;
    auto const &[rhs_id, rhs_data] = *rhs_iter;
    if (lhs_id != rhs_id) { return false; }
    if (lhs_data.parameter_type != rhs_data.parameter_type) { return false; }
    auto qt = lhs_data.parameter_type;
    if (lhs_data.index != rhs_data.index) { return false; }
    if (lhs_data.index == std::numeric_limits<size_t>::max()) { continue; }
    if (not qt.type().EqualsValue(lhs.buffer_[lhs_data.index],
                                  rhs.buffer_[rhs_data.index])) {
      return false;
    }
  }
  return true;
}

BoundParameters::BoundParameterReference BoundParameters::binding(
    ast::Declaration::Id const *id) const {
  auto iter = bindings_.find(id);
  if (iter == bindings_.end()) {
    return BoundParameterReference(nullptr, ir::CompleteResultRef());
  } else if (iter->second.index == std::numeric_limits<size_t>::max()) {
    return BoundParameterReference(&iter->second, ir::CompleteResultRef());
  } else {
    return BoundParameterReference(&iter->second, buffer_[iter->second.index]);
  }
}

}  // namespace compiler
