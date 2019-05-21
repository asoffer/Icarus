#include "type/cast.h"

#include <numeric>

#include "absl/algorithm/container.h"
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


namespace type {

// TODO much of this should be moved to virtual methods.
bool CanCast(Type const *from, Type const *to) {
  if (from->ReinterpretableAs(to)) { return true; }

  if (IsIntegral(from) && IsNumeric(to)) { return true; }
  if (IsFloatingPoint(from) && IsFloatingPoint(to)) { return true; }

  if (from->is<Tuple>() && to == Type_) {
    // TODO remove this hack for expressing the type of tuples
    auto const &entries = from->as<Tuple>().entries_;
    return std::all_of(entries.begin(), entries.end(),
                       [](Type const *t) { return t == Type_; });
  }

  // TODO other integer types.
  if (from == Int32 && (to->is<Enum>() || to->is<Flags>())) { return true; }

  if (auto *from_var = from->if_as<Variant>()) {
    if (auto *to_var = to->if_as<Variant>()) {
      return absl::c_all_of(from_var->variants_, [to_var](Type const *from_v) {
        return absl::c_count_if(to_var->variants_, [from_v](Type const *to_v) {
                 return CanCast(from_v, to_v);
               }) == 1;
      });
    } else {
      return absl::c_all_of(from_var->variants_, [to](Type const *from_v) {
        return CanCast(from_v, to);
      });
    }
  } else if (auto *to_var = to->if_as<Variant>()) {
    return absl::c_count_if(to_var->variants_, [from](Type const *to_v) {
             return CanCast(from, to_v);
           }) == 1;
  }

  if (auto *from_tup = from->if_as<Tuple>()) {
    if (auto *to_tup = to->if_as<Tuple>()) {
      if (from_tup->size() != to_tup->size()) { return false; }
      for (size_t i = 0; i < from_tup->size(); ++i) {
        if (!CanCast(from_tup->entries_.at(i), to_tup->entries_.at(i))) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }

  return false;
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
    return result ? Arr(lhs->as<Array>().len, result) : result;
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
