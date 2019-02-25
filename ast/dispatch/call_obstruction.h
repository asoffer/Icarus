#ifndef ICARUS_AST_DISPATCH_CALL_OBSTRUCTION_H
#define ICARUS_AST_DISPATCH_CALL_OBSTRUCTION_H

#include <iosfwd>
#include <variant>
#include <string>

#include "base/util.h"
#include "type/type.h"

namespace ast {
// Reason why the particular function could not be called.
struct CallObstruction {
  static CallObstruction None() { return CallObstruction(NoneData{}); }

  static CallObstruction NoParameterNamed(std::string name) {
    return CallObstruction(NoParameterNamedData{std::move(name)});
  }

  static CallObstruction NoDefault(std::string_view arg_name) {
    return CallObstruction(NoDefaultData{std::string(arg_name)});
  }

  static CallObstruction NoDefault(std::string arg_name) {
    return CallObstruction(NoDefaultData{std::move(arg_name)});
  }

  static CallObstruction NonConstantNamedArguments() {
    return CallObstruction(NonConstantNamedArgumentsData{});
  }

  static CallObstruction NonConstantDefaults() {
    return CallObstruction(NonConstantDefaultsData{});
  }

  static CallObstruction TypeMismatch(size_t pos, type::Type const *bound,
                                      type::Type const *input) {
    return CallObstruction(
        TypeMismatchData{pos, ASSERT_NOT_NULL(bound), ASSERT_NOT_NULL(input)});
  }

  static CallObstruction CascadingError() {
    return CallObstruction(CascadingErrorData{});
  }

  constexpr bool obstructed() const {
    return !std::holds_alternative<NoneData>(data_);
  }
  std::string to_string() const {
    return std::visit(
        base::overloaded{
            [](NoneData) -> std::string { return ""; },
            [](NoParameterNamedData const &d) -> std::string {
              return "Function has no argument named `" + d.name_ + "'";
            },
            [](NoDefaultData const &d) -> std::string {
              return "Overload ignored because no value provided for argument "
                     "`" +
                     d.name_ + "', which has no default.";
            },
            [](TypeMismatchData const &d) -> std::string {
              // TODO clarify that this is zero-indexed?
              return "Overload candidate ignored because parameter " +
                     std::to_string(d.position_) + " has type " +
                     d.input_->to_string() +
                     " which does not match what you provided (" +
                     d.bound_->to_string() + ")";
            },
            [](NonConstantNamedArgumentsData) -> std::string {
              return "Overload candidate ignored because non-constants cannot "
                     "be called with named arguments";
            },
            [](NonConstantDefaultsData) -> std::string {
              return "Overload candidate ignored because non-constants cannot "
                     "be called with default arguments";
            },
            [](CascadingErrorData) -> std::string { UNREACHABLE(); }},
        data_);
  }

  template <typename T>
  bool is() const {
    return std::holds_alternative<T>(data_);
  }

  struct NoneData {};
  struct NoParameterNamedData {
    std::string name_;
  };
  struct TypeMismatchData {
    size_t position_;
    type::Type const *bound_;
    type::Type const *input_;
  };
  struct NoDefaultData {
    std::string name_;
  };
  struct NonConstantNamedArgumentsData {};
  struct NonConstantDefaultsData {};
  struct CascadingErrorData {};

 private:
  template <typename T>
  CallObstruction(T &&data) : data_(std::forward<T>(data)) {}

  std::variant<NoneData, NoParameterNamedData, TypeMismatchData, NoDefaultData,
               NonConstantNamedArgumentsData, NonConstantDefaultsData,
               CascadingErrorData>
      data_;
};
}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_CALL_OBSTRUCTION_H
