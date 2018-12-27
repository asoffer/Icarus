#include "type/cast.h"

#include "type/all.h"

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

  // TODO other integer types.
  if (from == Int32 && (to->is<Enum>() || to->is<Flags>())) { return true; }

  auto from_mask = CastMask(from);
  auto to_mask   = CastMask(to);
  return ((from_mask & to_mask) == from_mask) || CanCastImplicitly(from, to);
}

bool CanCastImplicitly(type::Type const *from, type::Type const *to) {
  return Join(from, to) == to;
}

Type const *Join(Type const *lhs, Type const *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == nullptr) { return rhs; }  // Ignore errors
  if (rhs == nullptr) { return lhs; }  // Ignore errors
  if ((lhs == Block && rhs == OptBlock) || (lhs == OptBlock && rhs == Block)) {
    return Block;
  }
  if (lhs->is<Primitive>() && rhs->is<Primitive>()) {
    return lhs == rhs ? lhs : nullptr;
  }
  if (lhs == NullPtr && rhs->is<Pointer>()) { return rhs; }
  if (rhs == NullPtr && lhs->is<Pointer>()) { return lhs; }
  if (lhs->is<Pointer>() && rhs->is<Pointer>()) {
    return Join(lhs->as<Pointer>().pointee, rhs->as<Pointer>().pointee);
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
    auto *result = Join(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
    return result ? Arr(result, lhs->as<Array>().len) : result;
  } else if (lhs->is<Variant>()) {
    base::vector<Type const *> rhs_types;
    if (rhs->is<Variant>()) {
      rhs_types = rhs->as<Variant>().variants_;
    } else {
      rhs_types = {rhs};
    }

    auto vars = lhs->as<Variant>().variants_;
    vars.insert(vars.end(), rhs_types.begin(), rhs_types.end());
    return Var(std::move(vars));
  } else if (rhs->is<Variant>()) {  // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give
    // a better interface.
    for (Type const *v : rhs->as<Variant>().variants_) {
      if (lhs == v) { return rhs; }
    }
    return nullptr;
  }
  UNREACHABLE(lhs, rhs);
}

// TODO optimize (early exists. don't check lhs->is<> && rhs->is<>. If they
// don't match you can early exit.
Type const *Meet(Type const *lhs, Type const *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == nullptr || rhs == nullptr) { return nullptr; }

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
    base::vector<Type const *> results;
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
    base::vector<Type const *> results;
    for (Type const *t : rhs->as<Variant>().variants_) {
      if (Type const *result = Meet(t, lhs)) { results.push_back(result); }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  }

  return nullptr;
}

}  // namespace type
