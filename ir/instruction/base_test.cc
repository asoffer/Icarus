#include "ir/instruction/base.h"

#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {

struct MockInstruction {
  std::string to_string() const { return str; }

  void Inline(ir::InstructionInliner const& inliner) {}

  void WriteByteCode(ir::ByteCodeWriter* writer) const {}

  std::string str;
};

TEST(Inst, HoldsMockInstruction) {
  ir::Inst inst = MockInstruction{.str = "hello"};
  EXPECT_NE(inst.if_as<MockInstruction>(), nullptr);
  EXPECT_EQ(inst.to_string(), "hello");
  inst = MockInstruction{.str = "world"};
  EXPECT_EQ(inst.to_string(), "world");
}


}  // namespace
