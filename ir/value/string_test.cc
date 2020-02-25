#include "ir/value/string.h"

#include <sstream>

#include "gtest/gtest.h"

namespace {

TEST(String, Construction) {
  EXPECT_EQ(ir::String().get(), "");
  EXPECT_EQ(ir::String("abc").get(), "abc");
}

TEST(String, Deduplication) {
  ir::String s1("abc");
  ir::String s2("abc");
  ir::String s3("def");

  EXPECT_EQ(s1.addr(), s2.addr());
  EXPECT_NE(s1.addr(), s3.addr());
}

TEST(String, Ostream) {
  std::stringstream ss;
  ss << ir::String("abc");
  EXPECT_EQ(ss.str(), "abc");
}

}  // namespace
