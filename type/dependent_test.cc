#include "type/dependent.h"

#include "ir/function.h"
#include "ir/type_erased_value.h"
#include "jasmin/instructions/arithmetic.h"
#include "nth/test/test.h"
#include "type/function.h"
#include "type/parameters.h"
#include "type/primitive.h"

namespace ic::type::internal_dependent {
namespace {

using ::nth::debug::ElementsAreSequentially;

inline constexpr auto ValueIs = nth::debug::MakeProperty<"value-is">(
    [](auto const &value, jasmin::Value v) {
      return value.raw_value() == v.raw_value();
    });

NTH_TEST("dependent/basic/value", Type t, auto v) {
  auto value = Term::Value(TypeErasedValue(t, {v}));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == t);
  NTH_EXPECT(value.evaluate()->value() >>= ElementsAreSequentially(ValueIs(v)));
}

TypeErasedValue Identity(Type t) {
  static IrFunction id = [] {
    IrFunction f(1, 1);
    f.append<jasmin::Return>();
    return f;
  }();
  return TypeErasedValue(Function(Parameters({{.type = t}}), {t}), {&id});
}

NTH_TEST("dependent/basic/call", Type t, auto v) {
  auto value = Term::Call(Term::Value(TypeErasedValue(t, {v})),
                          Term::Value(Identity(t)));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == t);
  NTH_ASSERT(value.evaluate()->value() >>= ElementsAreSequentially(ValueIs(v)));
}

NTH_INVOKE_TEST("dependent/basic/*") {
  co_yield nth::TestArguments{Bool, true};
  co_yield nth::TestArguments{Bool, false};
  co_yield nth::TestArguments{I32, int32_t{3}};
  co_yield nth::TestArguments{I32, int32_t{4}};
  co_yield nth::TestArguments{I32, int32_t{-3}};
}

TypeErasedValue AddOne() {
  static IrFunction f = [] {
    IrFunction f(1, 1);
    f.append<jasmin::Push>(int64_t{1});
    f.append<jasmin::Add<int64_t>>();
    f.append<jasmin::Return>();
    return f;
  }();
  return TypeErasedValue(Function(Parameters({{.type = I64}}), {I64}), {&f});
}

NTH_TEST("dependent/call", int64_t v) {
  auto value =
      Term::Call(Term::Value(TypeErasedValue(I64, {v})), Term::Value(AddOne()));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(v + 1)));
}

NTH_INVOKE_TEST("dependent/call") {
  co_yield int64_t{3};
  co_yield int64_t{4};
  co_yield int64_t{-3};
}

NTH_TEST("dependent/simplify/trivial") {
  auto value = Term::Function(Term::Value(TypeErasedValue(Type_, {I64})),
                              Term::DeBruijnIndex(0));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(TypeErasedValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{3})));
}

NTH_TEST("dependent/simplify/basic") {
  auto value =
      Term::Function(Term::Value(TypeErasedValue(Type_, {I64})),
                     Term::Call(Term::DeBruijnIndex(0), Term::Value(AddOne())));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(TypeErasedValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{4})));
}

NTH_TEST("dependent/simplify/nested-call") {
  auto value = Term::Function(
      Term::Value(TypeErasedValue(Type_, {I64})),
      Term::Call(Term::Call(Term::DeBruijnIndex(0), Term::Value(AddOne())),
                 Term::Value(AddOne())));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(TypeErasedValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{5})));
}

NTH_TEST("dependent/simplify/dependent") {
  auto value = Term::Function(
      Term::Value(TypeErasedValue(Type_, {Type_})),
      Term::Function(Term::DeBruijnIndex(0), Term::DeBruijnIndex(0)));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(TypeErasedValue(Type_, {I64})));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(TypeErasedValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{3})));
}

NTH_TEST("dependent/bind/failure") {
  auto value = Term::Function(Term::Value(TypeErasedValue(Type_, {Bool})),
                              Term::DeBruijnIndex(0));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(not value.bind(TypeErasedValue(I64, {int64_t{3}})));
  NTH_EXPECT(value.evaluate() == nullptr);
}

}  // namespace
}  // namespace ic::type::internal_dependent
