#include "common/integer.h"

#include "common/constant/category.h"
#include "common/constant/entry.h"
#include "common/constant/manifest.h"
#include "common/internal/integers.h"

namespace ic {

// Integer::Integer(nth::integer const &n) {
//   auto [iter, inserted] = internal_common::Integers().try_emplace(
//       n, ConstantManifest::Global().NextSlot());
//   if (inserted) {
//     InsertIntoGlobalConstantManifest(ConstantCategory::Integer,
//                                      internal_common::Integers().index(iter));
//   }
//   mutable_value() = iter->second.value();
// }
// 
// Integer::Integer(nth::integer &&n) {
//   auto [iter, inserted] = internal_common::Integers().try_emplace(
//       std::move(n), ConstantManifest::Global().NextSlot());
//   if (inserted) {
//     InsertIntoGlobalConstantManifest(ConstantCategory::Integer,
//                                      internal_common::Integers().index(iter));
//   }
//   mutable_value() = iter->second.value();
// }
// 
// bool Integer::LessThan(Integer lhs, Integer rhs) {
//   return internal_common::Integers().from_index(lhs.value()).first <
//          internal_common::Integers().from_index(rhs.value()).first;
// }
// 
// Integer Integer::operator-() const {
//   return Integer(-static_cast<nth::integer const &>(*this));
// }
// 
// Integer::operator nth::integer const &() const {
//   return internal_common::Integers()
//       .from_index(ConstantManifest::Global()[value()].value())
//       .first;
// }

}  // namespace ic
