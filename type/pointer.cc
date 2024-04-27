#include "type/pointer.h"

#include "absl/container/flat_hash_map.h"
#include "common/constant/category.h"
#include "common/constant/entry.h"
#include "common/constant/manifest.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic::type {
namespace {

nth::NoDestructor<absl::flat_hash_map<Type, Constant>> pointers_;
nth::NoDestructor<absl::flat_hash_map<Type, Constant>> buffer_pointers_;
nth::NoDestructor<absl::flat_hash_map<Type, Constant>> slices_;

}  // namespace

absl::flat_hash_map<Type, Constant> const& Pointers() { return *pointers_; }
absl::flat_hash_map<Type, Constant> const& BufferPointers() {
  return *buffer_pointers_;
}
absl::flat_hash_map<Type, Constant> const& Slices() { return *slices_; }

// Type PointerType::pointee() const {
//   return Type(Type::from_index, ConstantManifest::Global()[index()].value());
// }
// 
// Type SliceType::element_type() const {
//   return Type(Type::from_index, ConstantManifest::Global()[index()].value());
// }
// 
// Type BufferPointerType::pointee() const {
//   return Type(Type::from_index, ConstantManifest::Global()[index()].value());
// }
// 
// PointerType Ptr(Type t) {
//   auto [iter, inserted] =
//       pointers_->emplace(t, ConstantManifest::Global().NextSlot());
//   if (inserted) {
//     InsertIntoGlobalConstantManifest(ConstantCategory::PointerType, t.index());
//   }
//   return PointerType(iter->second.value());
// }
// 
// BufferPointerType BufPtr(Type t) {
//   auto [iter, inserted] =
//       buffer_pointers_->emplace(t, ConstantManifest::Global().NextSlot());
//   if (inserted) {
//     InsertIntoGlobalConstantManifest(ConstantCategory::BufferPointerType,
//                                      t.index());
//   }
//   return BufferPointerType(iter->second.value());
// }
// 
// SliceType Slice(Type t) {
//   auto [iter, inserted] =
//       slices_->emplace(t, ConstantManifest::Global().NextSlot());
//   if (inserted) {
//     InsertIntoGlobalConstantManifest(ConstantCategory::SliceType, t.index());
//   }
//   return SliceType(iter->second.value());
// }

}  // namespace ic::type
