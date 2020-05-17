#include "type/cast.h"

#include <numeric>

#include "absl/algorithm/container.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace type {

static bool CanCastPointer(Pointer const *from, Pointer const *to) {
  if (from == to) { return true; }
  if (to->is<BufferPointer>() and not from->is<BufferPointer>()) {
    return false;
  }
  if (auto *from_p = from->pointee()->if_as<Pointer>()) {
    if (auto *to_p = to->pointee()->if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }
  return from->pointee() == to->pointee();
}

// TODO much of this should be moved to virtual methods.
bool CanCast(Type const *from, Type const *to) {
  if (to == from) { return true; }
  // TODO handle reinterpretation

  if (IsIntegral(from) and IsNumeric(to)) { return true; }
  if (IsFloatingPoint(from) and IsFloatingPoint(to)) { return true; }

  if (from->is<Tuple>() and to == Type_) {
    // TODO remove this hack for expressing the type of tuples
    auto const &entries = from->as<Tuple>().entries_;
    return std::all_of(entries.begin(), entries.end(),
                       [](Type const *t) { return t == Type_; });
  }

  // TODO other integer types.
  if (from == Int32 and (to->is<Enum>() or to->is<Flags>())) { return true; }

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
        if (not CanCast(from_tup->entries_[i], to_tup->entries_[i])) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }

  if (from == NullPtr) {
    if (auto *to_p = to->if_as<Pointer>()) { return true; }
  }
  if (auto *to_p = to->if_as<Pointer>()) {
    if (auto *from_p = from->if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }

  if (from == EmptyArray) {
    if (auto *to_arr = to->if_as<Array>()) {
      return to_arr->length() == 0;
    } else {
      return false;
    }
  }

  if (auto *from_arr = from->if_as<Array>()) {
    if (auto *to_arr = to->if_as<Array>()) {
      if (from_arr->length() != to_arr->length()) { return false; }
      if (auto *from_p = from_arr->data_type()->if_as<Pointer>()) {
        if (auto *to_p = to_arr->data_type()->if_as<Pointer>()) {
          return CanCastPointer(from_p, to_p);
        }
      }
    }
  }

  if (auto *from_fn = from->if_as<Function>()) {
    if (auto *to_fn = to->if_as<Function>()) {
      if (from_fn->params().size() != to_fn->params().size()) { return false; }

      if (from_fn->output() != to_fn->output()) { return false; }

      size_t num_params = from_fn->params().size();
      for (size_t i = 0; i < num_params; ++i) {
        auto const &from_param = from_fn->params()[i];
        auto const &to_param   = to_fn->params()[i];

        if (not CanCast(from_param.value, to_param.value)) { return false; }
        if (from_param.flags & core::MUST_NOT_NAME) {
          if (not(to_param.flags & core::MUST_NOT_NAME)) { return false; }
        } else if (from_param.flags & core::MUST_NAME) {
          if (not(to_param.flags & core::MUST_NAME) or
              from_param.name != to_param.name) {
            return false;
          }
        } else {
          if (not(to_param.flags & core::MUST_NOT_NAME) and
              from_param.name != to_param.name) {
            return false;
          }
        }
      }
      return true;
    } else {
      return false;
    }
  }

  return false;
}

// TODO optimize (early exists. don't check lhs->is<> and rhs->is<>. If they
// don't match you can early exit.
Type const *Meet(Type const *lhs, Type const *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == nullptr or rhs == nullptr) { return nullptr; }

  if (lhs == NullPtr or rhs == NullPtr) {
    // TODO It's not obvious to me that this is what I want to do.
    return nullptr;
  }
  if (lhs->is<Pointer>()) {
    return rhs->is<Pointer>() ? Ptr(Meet(lhs->as<Pointer>().pointee(),
                                         rhs->as<Pointer>().pointee()))
                              : nullptr;
  } else if (lhs->is<Array>() and rhs->is<Array>()) {
    if (lhs->as<Array>().length() != rhs->as<Array>().length()) {
      return nullptr;
    }
    auto *result =
        Meet(lhs->as<Array>().data_type(), rhs->as<Array>().data_type());
    return result ? Arr(lhs->as<Array>().length(), result) : result;
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
