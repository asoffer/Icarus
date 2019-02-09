#include "type/cast.h"

#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/interface.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"


namespace feature {
extern bool loose_casting;
}  // namespace feature

namespace type {
static u8 CastMask(Type const *t) {
  if (feature::loose_casting) {
    // Loose casting enables casting between any integral types, and between any
    // floating-point types, and from integral to floating-point, even if it
    // results in precision loss.
    if (t == Nat8 || t == Nat16 || t == Nat32 || t == Nat64 || t == Int8 ||
        t == Int16 || t == Int32 || t == Int64) {
      return 0x00;
    } else if (t == Float32 || t == Float64) {
      return 0x01;
    } else {
      UNREACHABLE(t);
    }

  } else {
    //              f32 (0x31) -> f64 (0x33)
    //               ^             ^
    // i8 (0x10) -> i16 (0x11) -> i32 (0x13) -> i64 (0x17)
    //               ^             ^             ^
    //              u8  (0x01) -> u16 (0x03) -> u32 (0x07) -> u64 (0x0f)
    if (t == Nat8) { return 0x01; }
    if (t == Nat16) { return 0x03; }
    if (t == Nat32) { return 0x07; }
    if (t == Nat64) { return 0x1f; }
    if (t == Int8) { return 0x10; }
    if (t == Int16) { return 0x11; }
    if (t == Int32) { return 0x13; }
    if (t == Int64) { return 0x17; }
    if (t == Float32) { return 0x31; }
    if (t == Float64) { return 0x33; }
    UNREACHABLE();
  }
}

bool CanCast(Type const *from, Type const *to) {
  if (from == to) { return true; }
  if (from->is<Tuple>() && to == Type_) {
    auto const &entries = from->as<Tuple>().entries_;
    return std::all_of(entries.begin(), entries.end(),
                       [](Type const *t) { return t == Type_; });
  }
  if (from->is<BufferPointer>() && to->is<Pointer>()) {
    return to->as<BufferPointer>().pointee == from->as<Pointer>().pointee;
  }
  if (from == NullPtr && to->is<Pointer>()) { return true; }
  if (from == EmptyArray && to->is<Array>()) { return true; }

  // TODO other integer types.
  if (from == Int32 && (to->is<Enum>() || to->is<Flags>())) { return true; }

  auto from_mask = CastMask(from);
  auto to_mask   = CastMask(to);
  return ((from_mask & to_mask) == from_mask);
}

// TODO optimize (early exists. don't check lhs->is<> && rhs->is<>. If they
// don't match you can early exit.
Type const *Meet(Type const *lhs, Type const *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == nullptr || rhs == nullptr) { return nullptr; }

  // TODO should this be a part of Meet or done externally first?
  if (auto *intf = rhs->if_as<type::Interface>()) {
    if (intf->matches(lhs)) {
      return lhs;
    } else {
      return nullptr;
    }
  }

  if (lhs == NullPtr || rhs == NullPtr) {
    // TODO It's not obvious to me that this is what I want to do.
    return nullptr;
  }
  if (lhs->is<Pointer>()) {
    return rhs->is<Pointer>() ? Ptr(Meet(lhs->as<Pointer>().pointee,
                                         rhs->as<Pointer>().pointee))
                              : nullptr;
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
    auto *result = Meet(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
    return result ? Arr(result, lhs->as<Array>().len) : result;
  } else if (lhs->is<Variant>()) {
    // TODO this feels very fishy, cf. ([3; int] | [4; int]) with [--; int]
    std::vector<Type const *> results;
    if (rhs->is<Variant>()) {
      for (Type const *l_type : lhs->as<Variant>().variants_) {
        for (Type const *r_type : rhs->as<Variant>().variants_) {
          Type const *result = Meet(l_type, r_type);
          if (result != nullptr) { results.push_back(result); }
        }
      }
    } else {
      for (Type const *t : lhs->as<Variant>().variants_) {
        if (Type const *result = Meet(t, rhs)) { results.push_back(result); }
      }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  } else if (rhs->is<Variant>()) {  // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give a
    // better interface.
    std::vector<Type const *> results;
    for (Type const *t : rhs->as<Variant>().variants_) {
      if (Type const *result = Meet(t, lhs)) { results.push_back(result); }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  }

  return nullptr;
}

}  // namespace type
