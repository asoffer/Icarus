#include "ir/instruction/base.h"

#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {

template <int N>
struct MockInstruction {
  explicit MockInstruction(std::string s) : str_(std::move(s)) {}

  std::string to_string() const { return str_; }

  void Inline(ir::InstructionInliner const& inliner) {}

  void WriteByteCode(ir::ByteCodeWriter* writer) const {}

 private:
  std::string str_;
};

TEST(Inst, MoveConstructionAndAssignment) {
  ir::Inst inst = MockInstruction<1>("hello");
  ASSERT_NE(inst.if_as<MockInstruction<1>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.to_string(), "hello");

  inst = MockInstruction<2>("world");
  EXPECT_EQ(inst.to_string(), "world");
  ASSERT_NE(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<1>>(), nullptr);

  inst = MockInstruction<2>("world!");
  EXPECT_EQ(inst.to_string(), "world!");
  ASSERT_NE(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<1>>(), nullptr);
}

TEST(Inst, CopyConstructionAndAssignment) {
  MockInstruction<1> m1("hello");
  ir::Inst inst = m1;
  ASSERT_NE(inst.if_as<MockInstruction<1>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.to_string(), "hello");

  MockInstruction<2> m2("world");
  inst = m2;
  EXPECT_EQ(inst.to_string(), "world");
  ASSERT_NE(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<1>>(), nullptr);

  m2   = MockInstruction<2>("world!");
  inst = m2;
  EXPECT_EQ(inst.to_string(), "world!");
  ASSERT_NE(inst.if_as<MockInstruction<2>>(), nullptr);
  EXPECT_EQ(inst.if_as<MockInstruction<1>>(), nullptr);
}

}  // namespace
