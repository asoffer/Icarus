#include "ir/value/fn.h"

#include "gtest/gtest.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/function.h"
#include "type/type.h"

void TestFn() {}

namespace {

TEST(Fn, ForeignFn) {
  ir::ForeignFn f(TestFn, type::Func({}, {}));
  ir::Fn a(f);
  ASSERT_EQ(a.kind(), ir::Fn::Kind::Foreign);

  uintptr_t lhs, rhs;
  void (*fnptr)() = a.foreign().get();
  std::memcpy(&lhs, &fnptr, sizeof(uintptr_t));
  void (*test_fn)() = TestFn;
  std::memcpy(&rhs, &test_fn, sizeof(uintptr_t));

  ASSERT_EQ(lhs, rhs);
}

TEST(Fn, NativeFn) {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::Fn a(&f);
  ASSERT_EQ(a.kind(), ir::Fn::Kind::Native);
  ASSERT_EQ(a.native(), ir::NativeFn(&f));
}

}  // namespace
