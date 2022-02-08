#include "ir/value/overload_set.h"

#include <cstdio>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/foreign_fn.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace {
using testing::Optional;

void TestFn1() { std::puts("TestFn1"); }
void TestFn2() { std::puts("TestFn2"); }
void TestFn3() { std::puts("TestFn3"); }

TEST(OverloadSet, Construction) {
  base::flyweight_map<std::pair<std::string, type::Function const *>,
                      void (*)()>
      map = {
          {{"TestFn1", type::Func({}, {})}, TestFn1},
          {{"TestFn2", type::Func({core::AnonymousParam(
                                      type::QualType::NonConstant(type::I64))},
                                  {})},
           TestFn2},
          {{"TestFn3", type::Func({core::AnonymousParam(
                                      type::QualType::NonConstant(type::Bool))},
                                  {})},
           TestFn3}};

  std::vector<ir::Fn> fns;
  fns.reserve(map.size());
  for (auto &entry : map) { fns.emplace_back(ir::ForeignFn(&entry)); }

  ir::OverloadSet os(std::move(fns));

  {
    core::Arguments<type::QualType> a;
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn1));
  }
  {
    core::Arguments<type::QualType> a{{type::QualType::Constant(type::I64)},
                                      {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn2));
  }
  {
    core::Arguments<type::QualType> a{{type::QualType::Constant(type::Bool)},
                                      {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn3));
  }
}

TEST(OverloadSet, FailsToConstruct) {
  ir::OverloadSet os(std::vector<ir::Fn>{});

  // Check twice because `Lookup()` caches state.
  EXPECT_FALSE(os.Lookup(core::Arguments<type::QualType>()).has_value());
  EXPECT_FALSE(os.Lookup(core::Arguments<type::QualType>()).has_value());
}

}  // namespace
