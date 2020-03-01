#include "match/binding_id.h"

#include "gtest/gtest.h"

namespace match {
namespace {

TEST(BindingId, Comparison) {
  std::string a = "a";
  std::string b = "b";
  EXPECT_EQ(BindingId(a), BindingId("a"));
  EXPECT_EQ(BindingId(b), BindingId("b"));
  EXPECT_NE(BindingId("a"), BindingId("b"));
  EXPECT_NE(BindingId("a"), BindingId(b));
}

}  // namespace
}  // namespace match
