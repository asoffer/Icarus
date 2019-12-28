#include "ir/new_inliner.h"

#include "ir/basic_block.h"
#include "test/catch.h"
#include "test/ordered_elements_are.h"

namespace {
using ::test::OrderedElementsAre;

TEST_CASE("Reg") {
  ir::Inliner i(10, {}, nullptr); 

  ir::Reg r(3);
  i.Inline(r);
  CHECK(r == ir::Reg(13));
}


TEST_CASE("RegOr") {
  ir::Inliner i(10, {}, nullptr); 

  ir::RegOr<int> r = 3;
  i.Inline(r);
  CHECK(r == ir::RegOr<int>(3));

  r = ir::Reg(3);
  i.Inline(r);
  CHECK(r == ir::RegOr<int>(ir::Reg(13)));
}

TEST_CASE("Container") {
  std::vector<ir::RegOr<double>> v = {
      ir::RegOr<double>(1.1),        ir::RegOr<double>(2.2),
      ir::RegOr<double>(ir::Reg(3)), ir::RegOr<double>(4.4),
      ir::RegOr<double>(ir::Reg(5)),
  };
  ir::Inliner i(10, {}, nullptr);

  i.Inline(v);
  CHECK_THAT(v, OrderedElementsAre(
                    ir::RegOr<double>(1.1), ir::RegOr<double>(2.2),
                    ir::RegOr<double>(ir::Reg(13)), ir::RegOr<double>(4.4),
                    ir::RegOr<double>(ir::Reg(15))));
}

TEST_CASE("BasicBlock") {
  ir::BasicBlock b1, b2, b3;

  ir::Inliner i(0, {{&b1, &b2}}, nullptr);

  ir::BasicBlock *b = &b1;
  i.Inline(b);
  CHECK(b == &b2);

  b = &b2;
  CHECK(b == &b2);

  b = &b3;
  CHECK(b == &b3);
}

}  // namespace
