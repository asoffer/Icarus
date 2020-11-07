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

namespace type {

static bool CanCastPointer(Pointer const *from, Pointer const *to) {
  if (from == to) { return true; }
  if (to->is<BufferPointer>() and not from->is<BufferPointer>()) {
    return false;
  }
  if (auto *from_p = from->pointee().if_as<Pointer>()) {
    if (auto *to_p = to->pointee().if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }
  return from->pointee() == to->pointee();
}

bool CanCastImplicitly(Type from, Type to) {
  if (to == from) { return true; }
  auto const *buf_ptr = from.if_as<BufferPointer>();
  if (buf_ptr and to == Type(Ptr(buf_ptr->pointee()))) { return true; }
  return false;
}

// TODO much of this should be moved to virtual methods.
bool CanCast(Type from, Type to) {
  if (to == from) { return true; }
  // TODO handle reinterpretation

  if (IsIntegral(from) and IsNumeric(to)) { return true; }
  if (IsFloatingPoint(from) and IsFloatingPoint(to)) { return true; }

  if (from.is<Tuple>() and to == Type_) {
    // TODO remove this hack for expressing the type of tuples
    auto const &entries = from.as<Tuple>().entries_;
    return std::all_of(entries.begin(), entries.end(),
                       [](Type t) { return t == Type_; });
  }

  // TODO other integer types.
  if (from == Int32 and (to.is<Enum>() or to.is<Flags>())) { return true; }

  if (auto *from_tup = from.if_as<Tuple>()) {
    if (auto *to_tup = to.if_as<Tuple>()) {
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
    if (auto *to_p = to.if_as<Pointer>()) { return true; }
  }
  if (auto *to_p = to.if_as<Pointer>()) {
    if (auto *from_p = from.if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }

  if (from == EmptyArray) {
    if (auto *to_arr = to.if_as<Array>()) {
      return to_arr->length() == 0;
    } else {
      return false;
    }
  }

  if (auto *from_arr = from.if_as<Array>()) {
    if (auto *to_arr = to.if_as<Array>()) {
      if (from_arr->length() != to_arr->length()) { return false; }
      if (auto *from_p = from_arr->data_type().if_as<Pointer>()) {
        if (auto *to_p = to_arr->data_type().if_as<Pointer>()) {
          return CanCastPointer(from_p, to_p);
        }
      }
    }
  }

  if (auto *from_fn = from.if_as<Function>()) {
    if (auto *to_fn = to.if_as<Function>()) {
      if (from_fn->params().size() != to_fn->params().size()) { return false; }

      if (from_fn->output() != to_fn->output()) { return false; }

      size_t num_params = from_fn->params().size();
      for (size_t i = 0; i < num_params; ++i) {
        auto const &from_param = from_fn->params()[i];
        auto const &to_param   = to_fn->params()[i];

        if (not CanCast(from_param.value.type(), to_param.value.type())) {
          return false;
        }
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

// TODO optimize (early exists. don't check lhs.is<> and rhs.is<>. If they
// don't match you can early exit.
Type Meet(Type lhs, Type rhs) {
  if (lhs == rhs) { return lhs; }
  if (not lhs or not rhs) { return nullptr; }

  if (lhs == NullPtr and rhs.is<Pointer>()) { return rhs; }
  if (rhs == NullPtr and lhs.is<Pointer>()) { return lhs; }

  if (lhs.is<Pointer>()) {
    // TODO: This is wrong.
    return rhs.is<Pointer>() ? Ptr(Meet(lhs.as<Pointer>().pointee(),
                                         rhs.as<Pointer>().pointee()))
                              : nullptr;
  } else if (lhs.is<Array>() and rhs.is<Array>()) {
    if (lhs.as<Array>().length() != rhs.as<Array>().length()) {
      return nullptr;
    }
    if (lhs.as<Array>().length() == 0) { return type::EmptyArray; }
    Type result =
        Meet(lhs.as<Array>().data_type(), rhs.as<Array>().data_type());
    return result ? Arr(lhs.as<Array>().length(), result) : result;
  }

  return nullptr;
}

}  // namespace type
