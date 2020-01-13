#include "compiler/constant/binding_map.h"

#include "gtest/gtest.h"

namespace {

TEST(ConstantBindingMap, DefaultConstruction) {
  compiler::ConstantBindingMap<int> map;
  EXPECT_EQ(map.root_value(), 0);
}

TEST(ConstantBindingMap, Emplace) {
  compiler::ConstantBindingMap<int> map;
  map.emplace(map.root(), 17);
  EXPECT_EQ(map.root_value(), 17);
}
}  // namespace
