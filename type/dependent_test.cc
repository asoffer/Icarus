#include "type/dependent.h"

#include "common/any_value.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/common.h"
#include "nth/test/test.h"
#include "type/function.h"
#include "type/parameters.h"
#include "type/primitive.h"
#include "type/type.h"

namespace ic::type {
namespace {

using ::nth::debug::ElementsAreSequentially;

using InstructionSet =
    jasmin::MakeInstructionSet<jasmin::Push, jasmin::Add<int64_t>>;

inline constexpr auto ValueIs = nth::debug::MakeProperty<"value-is">(
    [](auto const &value, jasmin::Value v) {
      return value.raw_value() == v.raw_value();
    });

NTH_TEST("term/basic/value", Type t, auto v) {
  auto value = DependentTerm::Value(AnyValue(t, {v}));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == t);
  NTH_EXPECT(value.evaluate()->value() >>= ElementsAreSequentially(ValueIs(v)));
}

AnyValue Identity(Type t) {
  static jasmin::Function<InstructionSet> id = [] {
    jasmin::Function<InstructionSet> f(1, 1);
    f.append<jasmin::Return>();
    return f;
  }();
  return AnyValue(Function(Parameters({{.type = t}}), {t}), {&id});
}

NTH_TEST("term/basic/call", Type t, auto v) {
  auto value = DependentTerm::Call(DependentTerm::Value(AnyValue(t, {v})),
                                   DependentTerm::Value(Identity(t)));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == t);
  NTH_ASSERT(value.evaluate()->value() >>= ElementsAreSequentially(ValueIs(v)));
}

NTH_INVOKE_TEST("term/basic/*") {
  co_yield nth::TestArguments{Bool, true};
  co_yield nth::TestArguments{Bool, false};
  co_yield nth::TestArguments{I32, int32_t{3}};
  co_yield nth::TestArguments{I32, int32_t{4}};
  co_yield nth::TestArguments{I32, int32_t{-3}};
}

AnyValue AddOne() {
  static jasmin::Function<InstructionSet> f = [] {
    jasmin::Function<InstructionSet> f(1, 1);
    f.append<jasmin::Push>(int64_t{1});
    f.append<jasmin::Add<int64_t>>();
    f.append<jasmin::Return>();
    return f;
  }();
  return AnyValue(Function(Parameters({{.type = I64}}), {I64}), {&f});
}

NTH_TEST("term/call", int64_t v) {
  auto value = DependentTerm::Call(DependentTerm::Value(AnyValue(I64, {v})),
                                   DependentTerm::Value(AddOne()));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(v + 1)));
}

NTH_INVOKE_TEST("term/call") {
  co_yield int64_t{3};
  co_yield int64_t{4};
  co_yield int64_t{-3};
}

NTH_TEST("term/simplify/trivial") {
  auto value =
      DependentTerm::Function(DependentTerm::Value(AnyValue(Type_, {I64})),
                              DependentTerm::DeBruijnIndex(0));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(AnyValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{3})));
}

NTH_TEST("term/simplify/basic") {
  auto value = DependentTerm::Function(
      DependentTerm::Value(AnyValue(Type_, {I64})),
      DependentTerm::Call(DependentTerm::DeBruijnIndex(0),
                          DependentTerm::Value(AddOne())));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(AnyValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{4})));
}

NTH_TEST("term/simplify/nested-call") {
  auto value = DependentTerm::Function(
      DependentTerm::Value(AnyValue(Type_, {I64})),
      DependentTerm::Call(DependentTerm::Call(DependentTerm::DeBruijnIndex(0),
                                              DependentTerm::Value(AddOne())),
                          DependentTerm::Value(AddOne())));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(AnyValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{5})));
}

NTH_TEST("term/simplify/dependent") {
  auto value = DependentTerm::Function(
      DependentTerm::Value(AnyValue(Type_, {Type_})),
      DependentTerm::Function(DependentTerm::DeBruijnIndex(0),
                              DependentTerm::DeBruijnIndex(0)));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(AnyValue(Type_, {I64})));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(value.bind(AnyValue(I64, {int64_t{3}})));
  NTH_ASSERT(value.evaluate() != nullptr);
  NTH_ASSERT(value.evaluate()->type() == I64);
  NTH_EXPECT(value.evaluate()->value() >>=
             ElementsAreSequentially(ValueIs(int64_t{3})));
}

NTH_TEST("term/bind/failure") {
  auto value =
      DependentTerm::Function(DependentTerm::Value(AnyValue(Type_, {Bool})),
                              DependentTerm::DeBruijnIndex(0));
  NTH_ASSERT(value.evaluate() == nullptr);
  NTH_ASSERT(not value.bind(AnyValue(I64, {int64_t{3}})));
  NTH_EXPECT(value.evaluate() == nullptr);
}

NTH_TEST("dependent/evaluation") {
  auto t = Dependent(
      DependentTerm::Function(DependentTerm::Value(AnyValue(Type_, {Type_})),
                              DependentTerm::DeBruijnIndex(0)),
      DependentParameterMapping({DependentParameterMapping::Index::Value(0)}));
  std::array inputs{AnyValue(Type_, {Char})};
  NTH_EXPECT(t(inputs) == Char);
}

NTH_TEST("dependent/argument-reordering") {
  auto t = Dependent(
      DependentTerm::Function(
          DependentTerm::Value(AnyValue(Type_, {Type_})),
          DependentTerm::Function(DependentTerm::Value(AnyValue(Type_, {I32})),
                                  DependentTerm::DeBruijnIndex(1))),
      DependentParameterMapping({DependentParameterMapping::Index::Value(1),
                                 DependentParameterMapping::Index::Value(0)}));

  // Incorrect type.
  std::array inputs{AnyValue(Type_, {Char}), AnyValue(Type_, {Char})};
  NTH_EXPECT(t(inputs) == std::nullopt);

  // Correct type. Reordered.
  inputs = std::array{AnyValue(I32, {int32_t{17}}), AnyValue(Type_, {Char})};
  NTH_EXPECT(t(inputs) == Char);
}

}  // namespace
}  // namespace ic::type
