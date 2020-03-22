#include "ir/blocks/register_allocator.h"
#include "type/primitive.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace ir {
namespace {
using testing::MockFunction;
using testing::Return;
using testing::_;

TEST(RegisterAllocator, NumRegs) {
  RegisterAllocator a(3);
  EXPECT_EQ(a.num_regs(), 3);
  EXPECT_EQ(a.num_args(), 3);

  EXPECT_EQ(a.Reserve(), Reg{3});
  EXPECT_EQ(a.Reserve(), Reg{4});
  EXPECT_EQ(a.num_regs(), 5);
  EXPECT_EQ(a.num_args(), 3);

  EXPECT_EQ(a.StackAllocate(type::Int32), Reg{5});
  EXPECT_EQ(a.StackAllocate(type::Int32), Reg{6});
  EXPECT_EQ(a.num_regs(), 7);
  EXPECT_EQ(a.num_args(), 3);
}


TEST(RegisterAllocator, ForEachAlloc) {
  RegisterAllocator a(3);
  a.StackAllocate(type::Int32);
  a.StackAllocate(type::Int64);

  MockFunction<void(type::Type const *, Reg)> mock_fn;
  EXPECT_CALL(mock_fn, Call(type::Int32, _)).Times(1);
  EXPECT_CALL(mock_fn, Call(type::Int64, _)).Times(1);
  a.for_each_alloc(mock_fn.AsStdFunction());
}

TEST(RegisterAllocator, MergeFrom) {
  RegisterAllocator a1(3);
  RegisterAllocator a2(4);
  a1.StackAllocate(type::Int32);
  a2.StackAllocate(type::Int64);
  a2.StackAllocate(type::Bool);

  MockFunction<Reg(Reg)> mock_fn;
  EXPECT_CALL(mock_fn, Call(Reg{4})).WillOnce(Return(Reg{10}));
  EXPECT_CALL(mock_fn, Call(Reg{5})).WillOnce(Return(Reg{11}));

  a1.MergeFrom(a2, mock_fn.AsStdFunction());
  EXPECT_EQ(a1.num_regs(), 10);
}

}  // namespace
}  // namespace ir
