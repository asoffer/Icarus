#include "ir/instruction_inliner.h"

#include <vector>

#include "ir/basic_block.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "test/catch.h"
#include "test/ordered_elements_are.h"
#include "type/function.h"
#include "type/jump.h"
#include "type/type.h"

namespace {
using ::test::OrderedElementsAre;

void InitBlockGroupsForTest(ir::CompiledFn *f, ir::Jump *j) {
  ir::Builder bldr;
  bldr.CurrentGroup() = f;
  bldr.CurrentBlock() = f->entry();
  bldr.Add(ir::RegOr<int64_t>(ir::Reg::Arg(0)), 1);
  bldr.Add(ir::RegOr<int64_t>(ir::Reg::Arg(0)), 1);
  bldr.Add(ir::RegOr<int64_t>(ir::Reg::Arg(0)), 1);
  bldr.Add(ir::RegOr<int64_t>(ir::Reg::Arg(0)), 1);
}

TEST_CASE("Reg") {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::Jump j(type::Jmp({}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f, ir::LocalBlockInterpretation({}));

  // Function we're inlining into has 4 registers already.
  REQUIRE(f.num_regs() == 4);
  ir::Reg r(3);
  i.Inline(r);
  CHECK(r == ir::Reg(7));
}

TEST_CASE("RegOr") {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::Jump j(type::Jmp({}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f, ir::LocalBlockInterpretation({}));

  // Function we're inlining into has 4 registers already.
  REQUIRE(f.num_regs() == 4);

  ir::RegOr<int> r = 3;
  i.Inline(r);
  CHECK(r == ir::RegOr<int>(3));

  r = ir::Reg(3);
  i.Inline(r);
  CHECK(r == ir::RegOr<int>(ir::Reg(7)));
}

TEST_CASE("Container") {
  std::vector<ir::RegOr<double>> v = {
      ir::RegOr<double>(1.1),        ir::RegOr<double>(2.2),
      ir::RegOr<double>(ir::Reg(3)), ir::RegOr<double>(4.4),
      ir::RegOr<double>(ir::Reg(5)),
  };

  ir::CompiledFn f(type::Func({}, {}), {});
  ir::Jump j(type::Jmp({}), {});
  InitBlockGroupsForTest(&f, &j);

  ir::InstructionInliner i(&j, &f, ir::LocalBlockInterpretation({}));

  // Function we're inlining into has 4 registers already.
  REQUIRE(f.num_regs() == 4);

  i.Inline(v);
  CHECK_THAT(v, OrderedElementsAre(
                    ir::RegOr<double>(1.1), ir::RegOr<double>(2.2),
                    ir::RegOr<double>(ir::Reg(7)), ir::RegOr<double>(4.4),
                    ir::RegOr<double>(ir::Reg(9))));
}

}  // namespace
