#include "ir/results.h"

#include "gtest/gtest.h"
#include "ir/value/reg.h"

namespace ir {
namespace {

TEST(Results, Construction) {
  EXPECT_EQ(Results().size(), 0u);
  EXPECT_EQ(Results(3).size(), 1u);
  EXPECT_EQ(Results(3, true, 4).size(), 3u);
}

TEST(Results, ArgsPassThroughUnchanged) {
  EXPECT_EQ(Results(Reg::Arg(3)).get<Reg>(0), Reg::Arg(3));
}

TEST(Results, AccessValues) {
  Results r(3, true, 4);
  EXPECT_EQ(r.get<int>(0), RegOr<int>(3));
  EXPECT_EQ(r.get<bool>(1), RegOr<bool>(true));
  EXPECT_EQ(r.get<int>(2), RegOr<int>(4));
}

TEST(Results, AccessRegisters) {
  Results r;
  r.append(3);
  r.append(Reg(17));
  r.append(true);
  EXPECT_EQ(r.get<int>(1), RegOr<int>(Reg(17)));
  EXPECT_EQ(r.get<Reg>(1), Reg(17));
}

TEST(Results, IsReg) {
  Results r(3.14, Reg(4), Reg(17), true);
  EXPECT_FALSE(r.is_reg(0));
  EXPECT_TRUE(r.is_reg(1));
  EXPECT_TRUE(r.is_reg(2));
  EXPECT_FALSE(r.is_reg(0));
}

TEST(Results, GetResult) {
  Results r(3.14, Reg(4), Reg(17), true);
  ASSERT_EQ(r.GetResult(0).size(), 1u);
  EXPECT_EQ(r.GetResult(0).get<double>(0), RegOr<double>(3.14));
  ASSERT_EQ(r.GetResult(1).size(), 1u);
  EXPECT_EQ(r.GetResult(1).get<Reg>(0), Reg(4));
  ASSERT_EQ(r.GetResult(2).size(), 1u);
  EXPECT_EQ(r.GetResult(2).get<Reg>(0), Reg(17));
  ASSERT_EQ(r.GetResult(3).size(), 1u);
  EXPECT_EQ(r.GetResult(3).get<bool>(0), RegOr<bool>(true));
}

TEST(Results, GetResultStartingWithReg) {
  Results r(Reg(1), static_cast<int64_t>(4));
  ASSERT_EQ(r.GetResult(0).size(), 1u);
  EXPECT_EQ(r.GetResult(0).get<Reg>(0), Reg(1));
  ASSERT_EQ(r.GetResult(1).size(), 1u);
  EXPECT_EQ(r.GetResult(1).get<int64_t>(0),
            RegOr<int64_t>(static_cast<int64_t>(4)));
}

TEST(Results, AppendResults) {
  Results r1(true, Reg(17));
  Results r2(3, Reg(34));
  r1.append(r2);

  ASSERT_EQ(r1.size(), 4u);
  EXPECT_EQ(r1.get<bool>(0), RegOr<bool>(true));
  EXPECT_EQ(r1.get<Reg>(1), Reg(17));
  EXPECT_EQ(r1.get<int>(2), RegOr<int>(3));
  EXPECT_EQ(r1.get<Reg>(3), Reg(34));
}

TEST(Results, FromRaw) {
  int32_t n    = 0xdeadbeef;
  auto results = Results::FromRaw(static_cast<void*>(&n), core::Bytes(4));
  EXPECT_EQ(results.size(), 1);
  EXPECT_EQ(results.get<int32_t>(0), RegOr<int32_t>(n));
}

TEST(Results, FromUntypedBuffer) {
  base::untyped_buffer buf;
  buf.append(1234);
  buf.append(true);
  buf.append(5678);
  auto results = Results::FromUntypedBuffer({0, sizeof(int), sizeof(int) + 1},
                                            std::move(buf));
  EXPECT_EQ(results.size(), 3);
  EXPECT_EQ(results.get<int>(0), RegOr<int>(1234));
  EXPECT_EQ(results.get<bool>(1), RegOr<bool>(true));
  EXPECT_EQ(results.get<int>(2), RegOr<int>(5678));
}

TEST(Results, ToString) {
  EXPECT_EQ(Results().to_string(), "[]");
  EXPECT_EQ(Results(true).to_string(), "[offset(0)]");
  EXPECT_EQ(Results(uint16_t{3}, true, uint64_t{4}, Reg::Arg(4)).to_string(),
            "[offset(0), offset(2), offset(3), arg.4]");
}

TEST(Results, ForEachReg) {
  auto const f = [](Reg& r) { r = Reg(r.value() + 100); };

  {  // Empty
    Results r(1, 2, 3);
    r.for_each_reg(f);
    EXPECT_EQ(r.get<int>(0), RegOr<int>(1));
    EXPECT_EQ(r.get<int>(1), RegOr<int>(2));
    EXPECT_EQ(r.get<int>(2), RegOr<int>(3));
  }

  {  // Updates
    Results r(1, Reg(2), Reg(3));
    r.for_each_reg(f);
    EXPECT_EQ(r.get<int>(0), RegOr<int>(1));
    EXPECT_EQ(r.get<Reg>(1), Reg(102));
    EXPECT_EQ(r.get<Reg>(2), Reg(103));
  }
}
}  // namespace
}  // namespace ir
