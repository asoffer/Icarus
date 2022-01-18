#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

#include <string_view>
#include <variant>

#include "diagnostic/message.h"
#include "frontend/source/view.h"
#include "type/type.h"

namespace type {

// Returns whether `from` is a type that can be reinterpreted in place into
// `to`.
bool CanCastInPlace(Type from, Type to);

// Returns whether `from` is a type that can be cast implicitly to `to`.
bool CanCastImplicitly(Type from, Type to);

// Returns whether `from` is a type that can be cast explicitly to `to`.
bool CanCastExplicitly(Type from, Type to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no meet exists.
Type Meet(Type lhs, Type rhs);

struct InferenceResult {
  enum class Kind { EmptyArray, NullPointer, PointerMismatch, Uninitialized };

  InferenceResult(Kind k) : value_(k) {}
  InferenceResult(Type t) : value_(t) {}

  explicit operator bool() const {
    return std::holds_alternative<Type>(value_);
  }

  Type const &operator*() const { return std::get<Type>(value_); }
  Kind failure() const { return std::get<Kind>(value_); }

  bool operator==(InferenceResult const &) const = default;
  bool operator!=(InferenceResult const &) const = default;

 private:
  std::variant<Type, Kind> value_;
};

struct UninferrableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uninferrable-type";

  diagnostic::DiagnosticMessage ToMessage() const;

  InferenceResult::Kind kind;
  frontend::SourceView view;
};

// Given a type, returns a reason explaining why this type cannot be infered, or
// the type that would be infered from this one for non-constant values. This
// specifically comes up with expressions such as `[]` or `null`.
InferenceResult Inference(Type t);

}  // namespace type

#endif  // ICARUS_TYPE_CAST_H
