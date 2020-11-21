#include "ir/instruction/inliner.h"

#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/blocks/basic.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/instruction/arithmetic.h"
#include "type/function.h"
#include "type/jump.h"
#include "type/type.h"

namespace {
using ::testing::ElementsAre;

void InitBlockGroupsForTest(ir::CompiledFn *f, ir::CompiledJump *j) {
  ir::Builder bldr;
  bldr.CurrentGroup() = f;
  bldr.CurrentBlock() = f->entry();
  for (int i = 0; i < 4; ++i) {
    bldr.CurrentBlock()->Append(ir::AddInstruction<int64_t>{
        .lhs    = ir::Reg::Arg(0),
        .rhs    = 1,
        .result = bldr.CurrentGroup()->Reserve(),
    });
  }
}

TEST(InstructionInliner, Reg) {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::CompiledJump j(type::Jmp(nullptr, {}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f,
                           ir::LocalBlockInterpretation({}, nullptr, nullptr));

  // Function we're inlining into has 4 registers already.
  ASSERT_EQ(f.num_regs(), 4);
  ir::Reg r(3);
  i.Inline(r);
  EXPECT_EQ(r, ir::Reg(7));
}

TEST(InstructionInliner, RegOr) {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::CompiledJump j(type::Jmp(nullptr, {}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f,
                           ir::LocalBlockInterpretation({}, nullptr, nullptr));

  // Function we're inlining into has 4 registers already.
  ASSERT_EQ(f.num_regs(), 4);

  ir::RegOr<int> r = 3;
  i.Inline(r);
  EXPECT_EQ(r, ir::RegOr<int>(3));

  r = ir::Reg(3);
  i.Inline(r);
  EXPECT_EQ(r, ir::RegOr<int>(ir::Reg(7)));
}

TEST(InstructionInliner, Container) {
  std::vector<ir::RegOr<double>> v = {
      ir::RegOr<double>(1.1),        ir::RegOr<double>(2.2),
      ir::RegOr<double>(ir::Reg(3)), ir::RegOr<double>(4.4),
      ir::RegOr<double>(ir::Reg(5)),
  };

  ir::CompiledFn f(type::Func({}, {}), {});
  ir::CompiledJump j(type::Jmp(nullptr, {}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f,
                           ir::LocalBlockInterpretation({}, nullptr, nullptr));

  // Function we're inlining into has 4 registers already.
  ASSERT_EQ(f.num_regs(), 4);

  i.Inline(v);
  EXPECT_THAT(v,
              ElementsAre(ir::RegOr<double>(1.1), ir::RegOr<double>(2.2),
                          ir::RegOr<double>(ir::Reg(7)), ir::RegOr<double>(4.4),
                          ir::RegOr<double>(ir::Reg(9))));
}

}  // namespace
