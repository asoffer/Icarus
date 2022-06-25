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

// Indicates which kind of casting is being referred to.
enum class CastKind {
  // `Explicit` casts occur when the `as` token is present. Explicit casts are
  // more permissive than implicit casts; every allowed implicit cast is also
  // allowed explicitly, but there are several casts which are only allowed
  // explicitly.
  Explicit,

  // `Implicit` casts occur when there are no visible tokens indicating that a
  // cast occurs. Implicit casts are more restrictive than explicit casts.
  Implicit,

  // `InPlace` is the most restrictive form of cast. All InPlace casts are also
  // allowed implicitly or explicitly. InPlace casts are casts where not-only
  // `T` can be cast to `U`, but also pointers to `T` can be cast to pointers to
  // `U`. One such example of this would be a cast from a buffer pointer to a
  // pointer with the same pointed-to-type. These casts can be done in place
  // because the bit-representation is guaranteed to be the same.
  InPlace,
};

template <CastKind Kind>
struct CastVisitor {
  using signature = bool(Type);

  bool operator()(Type from, Type to) {
    if (to == from) { return true; }

    if constexpr (Kind == CastKind::InPlace) {
      if (to == Byte) { return true; }
    }

    if (auto const *to_ptr = to.if_as<Pointer>();
        to_ptr and to_ptr->pointee() == from and not to.is<BufferPointer>()) {
      return true;
    }

    return from.visit<CastVisitor>(*this, to);
  }

  bool operator()(auto const *from, Type to) {
    constexpr auto from_type = base::meta<std::decay_t<decltype(*from)>>;
    if constexpr (from_type == base::meta<type::Primitive>) {
      if (to == Integer) { return false; }
      switch (from->kind()) {
        case Primitive::Kind::Type_: return to == Interface;
        case Primitive::Kind::NullPtr:
          if constexpr (Kind == CastKind::InPlace) {
            return false;
          } else {
            return to.is<Pointer>();
          }
        case Primitive::Kind::Integer:
          if constexpr (Kind != CastKind::InPlace) {
            return IsNumeric(to);
          } else {
            return false;
          }
        case Primitive::Kind::EmptyArray:
          if (auto const *to_arr = to.if_as<Array>()) {
            return to_arr->length() == 0;
          } else {
            return to.is<Slice>();
          }
        case Primitive::Kind::Byte:
          if constexpr (Kind == CastKind::InPlace) {
            return true;
          } else {
            return false;
          }
        default:
          if constexpr (Kind == CastKind::Explicit) {
            if (IsIntegral(from)) {
              if (IsNumeric(to)) { return true; }
              if (auto const *to_enum = to.if_as<Enum>()) {
                return operator()(from, to_enum->UnderlyingType());
              } else if (auto const *to_flags = to.if_as<Flags>()) {
                return operator()(from, to_flags->UnderlyingType());
              }
            }
            if (IsFloatingPoint(from) and IsFloatingPoint(to)) { return true; }
          }
          break;
      }


    } else if constexpr (from_type == base::meta<type::Pointer> ) {
      if (auto const *to_ptr = to.if_as<Pointer>();
          to_ptr and not to.is<BufferPointer>()) {
        return CastVisitor<CastKind::InPlace>{}(from->pointee(),
                                                to_ptr->pointee());
      }

    } else if constexpr (from_type == base::meta<type::BufferPointer>) {
      if (auto const *to_ptr = to.if_as<Pointer>()) {
        return CastVisitor<CastKind::InPlace>{}(from->pointee(),
                                                to_ptr->pointee());
      }

    } else if constexpr (from_type == base::meta<type::Array>) {
      if (auto const *to_slice = to.if_as<Slice>()) {
        if constexpr (Kind == CastKind::InPlace) {
          return false;
        } else {
          return CastVisitor<CastKind::InPlace>{}(from->data_type(),
                                                  to_slice->data_type());
        }
      } else if (auto const *to_array = to.if_as<Array>()) {
        if (from->data_type() == Integer and
            CanCastImplicitly(Integer, to_array->data_type())) {
          return true;
        }

        if constexpr (Kind == CastKind::Explicit) {
          return from->length() == to_array->length() and
                 CastVisitor<CastKind::InPlace>{}(from->data_type(),
                                                  to_array->data_type());
        }
      }

    } else if constexpr (from_type == base::meta<type::Slice>) {
      if (auto const *to_slice = to.if_as<Slice>()) {
        return CastVisitor<CastKind::InPlace>{}(from->data_type(),
                                                to_slice->data_type());
      }

    } else if constexpr (from_type == base::meta<type::Function>) {
      if (auto const *to_fn = to.if_as<Function>()) {
        size_t num_params = from->parameters().size();
        if (num_params != to_fn->parameters().size() or
            from->return_types() != to_fn->return_types()) {
          return false;
        }

        for (size_t i = 0; i < num_params; ++i) {
          auto const &from_param = from->parameters()[i];
          auto const &to_param   = to_fn->parameters()[i];

          if (not CastVisitor<CastKind::InPlace>{}(from_param.value.type(),
                                                   to_param.value.type())) {
            return false;
          }
          if (from_param.flags >= core::ParameterFlags::MustNotName()) {
            if (not(to_param.flags >= core::ParameterFlags::MustNotName())) {
              return false;
            }
          } else {
            if (not(to_param.flags >= core::ParameterFlags::MustNotName()) and
                from_param.name != to_param.name) {
              return false;
            }
          }
        }

        return true;
      }

    } else if constexpr (requires { from->UnderlyingType(); }) {
      if constexpr (Kind == CastKind::Explicit) {
        return to == from->UnderlyingType();
      }
    }
    return false;
  }
};

}  // namespace

bool CanCastImplicitly(Type from, Type to) {
  return CastVisitor<CastKind::Implicit>{}(from, to);
}
bool CanCastExplicitly(Type from, Type to) {
  return CastVisitor<CastKind::Explicit>{}(from, to);
}
bool CanCastInPlace(Type from, Type to) {
  return CastVisitor<CastKind::InPlace>{}(from, to);
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
    for (auto const &param : f->parameters()) {
      ASSIGN_OR(return _, std::ignore, Inference(param.value.type()));
    }
    for (auto t : f->return_types()) {
      ASSIGN_OR(return _, std::ignore, Inference(t));
    }
    // TODO: Transformation.
    return t;
  } else if (t == Integer) {
    return I64;
  }

  return t;
}

diagnostic::DiagnosticMessage UninferrableType::ToMessage() const {
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
      diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
}

}  // namespace type
