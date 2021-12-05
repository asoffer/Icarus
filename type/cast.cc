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
#include "type/slice.h"

namespace type {
namespace {

bool CanCastPointer(Pointer const *from, Pointer const *to) {
  if (from == to) { return true; }
  if (to->is<BufferPointer>() and not from->is<BufferPointer>()) {
    return false;
  }
  return CanCastInPlace(from->pointee(), to->pointee());
}

bool CanCastFunction(Function const *from, Function const *to) {
  if (from->params().size() != to->params().size()) { return false; }

  if (from->output() != to->output()) { return false; }

  size_t num_params = from->params().size();
  for (size_t i = 0; i < num_params; ++i) {
    auto const &from_param = from->params()[i];
    auto const &to_param   = to->params()[i];

    if (not CanCastImplicitly(from_param.value.type(), to_param.value.type())) {
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
}

// TODO: Much of this should be moved to virtual methods.
// TODO: Need the full qual-type to handle buffer-reference conversions.
template <bool IncludeExplicit>
bool CanCast(Type from, Type to) {
  // TODO: handle reinterpretation
  if (to == from) { return true; }
  if (to == Integer) { return false; }

  if (auto const *to_p = to.if_as<Pointer>()) {
    if (from == NullPtr or
        (to_p->pointee() == from and not to.is<BufferPointer>())) {
      return true;
    }

    if (auto const *from_p = from.if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }

  if (from == EmptyArray) {
    if (auto const *to_arr = to.if_as<Array>()) {
      return to_arr->length() == 0;
    } else {
      return to.is<Slice>();
    }
  }

  if (auto const *from_array = from.if_as<Array>()) {
    if (auto const *to_slice = to.if_as<Slice>()) {
      return CanCastInPlace(from_array->data_type(), to_slice->data_type());

    } else if constexpr (IncludeExplicit) {
      if (auto const *to_array = to.if_as<Array>()) {
        return from_array->length() == to_array->length() and
               CanCastInPlace(from_array->data_type(), to_array->data_type());
      }
    }
  } else if (auto const *from_slice = from.if_as<Slice>()) {
    if (auto const *to_slice = to.if_as<Slice>()) {
      return CanCastInPlace(from_slice->data_type(), to_slice->data_type());
    }
  }

  if constexpr (not IncludeExplicit) { 
    if (from == Integer and IsNumeric(to)) { return true; }
  }

  if constexpr (IncludeExplicit) {
    if (IsIntegral(from) and IsNumeric(to)) { return true; }
    if (IsFloatingPoint(from) and IsFloatingPoint(to)) { return true; }

    if (from == Char and IsIntegral(to)) { return true; }
    if ((from == I8 or from == U8 or from == Integer) and to == Char) {
      return true;
    }

    // TODO other integer types. This set of rules is weird and obviously wrong.
    if ((from == I32 or from == Integer) and
        (to.is<Enum>() or to.is<Flags>())) {
      return true;
    }
    if ((from.is<Enum>() or from.is<Flags>()) and to == U64) { return true; }

    if (auto const *from_fn = from.if_as<Function>()) {
      if (auto const *to_fn = to.if_as<Function>()) {
        return CanCastFunction(from_fn, to_fn);
      }
    }
  }

  return false;
}

}  // namespace

bool CanCastImplicitly(Type from, Type to) { return CanCast<false>(from, to); }
bool CanCastExplicitly(Type from, Type to) { return CanCast<true>(from, to); }

bool CanCastInPlace(Type from, Type to) {
  if (from == to or from == Byte or to == Byte) { return true; }

  if (auto const *from_p = from.if_as<Pointer>()) {
    if (auto const *to_p = to.if_as<Pointer>()) {
      return CanCastPointer(from_p, to_p);
    }
  }

  if (auto const *from_slice = from.if_as<Slice>()) {
    if (auto const *to_slice = to.if_as<Slice>()) {
      return CanCastInPlace(from_slice->data_type(), to_slice->data_type());
    }
  }

  if (auto const *from_fn = from.if_as<Function>()) {
    if (auto const *to_fn = to.if_as<Function>()) {
      return CanCastFunction(from_fn, to_fn);
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
  if (lhs == Integer and IsIntegral(rhs)) { return rhs; }
  if (rhs == Integer and IsIntegral(lhs)) { return lhs; }

  if (lhs.is<Pointer>()) {
    // TODO: This is wrong.
    return rhs.is<Pointer>() ? Ptr(Meet(lhs.as<Pointer>().pointee(),
                                        rhs.as<Pointer>().pointee()))
                             : nullptr;
  } else if (lhs.is<Array>() and rhs.is<Array>()) {
    if (lhs.as<Array>().length() != rhs.as<Array>().length()) {
      return nullptr;
    }
    if (lhs.as<Array>().length() == 0) { return EmptyArray; }
    Type result =
        Meet(lhs.as<Array>().data_type(), rhs.as<Array>().data_type());
    return result ? Arr(lhs.as<Array>().length(), result) : result;
  }

  return nullptr;
}

InferenceResult Inference(Type t) {
  if (t == NullPtr) {
    return InferenceResult::Kind::NullPointer;
  } else if (t == EmptyArray) {
    return InferenceResult::Kind::EmptyArray;
  } else if (auto *a = t.if_as<Array>()) {
    ASSIGN_OR(return _, Type result, Inference(a->data_type()));
    return type::Type(Arr(a->length(), result));
  } else if (auto *p = t.if_as<Pointer>()) {
    ASSIGN_OR(return _, Type result, Inference(p->pointee()));
    if (p->pointee() != result) {
      return InferenceResult::Kind::PointerMismatch;
    }
    return t;
  } else if (auto *f = t.if_as<Function>()) {
    for (auto const &param : f->params()) {
      ASSIGN_OR(return _, std::ignore, Inference(param.value.type()));
    }
    for (auto t : f->output()) {
      ASSIGN_OR(return _, std::ignore, Inference(t));
    }
    // TODO: Transformation.
    return t;
  } else if (t == Integer) {
    return I64;
  }

  return t;
}

diagnostic::DiagnosticMessage UninferrableType::ToMessage(
    frontend::Source const *src) const {
  char const *text = nullptr;
  switch (kind) {
    case InferenceResult::Kind::EmptyArray:
      text =
          "Unable to infer the type of the following expression because the "
          "type of an empty array cannot be inferred. Either specify the "
          "type explicitly, or cast it to a specific array type:";
      break;
    case InferenceResult::Kind::NullPointer:
      text =
          "Unable to infer the type of the following expression because the "
          "type of `null` cannot be inferred. Either specify the type "
          "explicitly, or cast it to a specific pointer type:";
      break;
    case InferenceResult::Kind::PointerMismatch:
      // TODO: Once you have a better understanding of what the issue is here,
      // improve the error message.
      text =
          "Unable to infer the type of the following expression because the "
          "type pointed-to type would decay incorrectly.";
      break;
    case InferenceResult::Kind::Uninitialized:
      text = "Unable to infer the type of a value that is uninitalized:";
      break;
  }

  return diagnostic::DiagnosticMessage(
      diagnostic::Text(text),
      diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
}

}  // namespace type
