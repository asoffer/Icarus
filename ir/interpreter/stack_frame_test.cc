#include "ir/interpreter/stack_frame.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace ir::interpreter {
namespace {

using ::testing::IsEmpty;

TEST(StackFrame, SetResolve) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);

  frame.set(Reg(1), uint64_t{17});
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(1)), uint64_t{17});

  frame.set(Reg::Parameter(1), 3.14);
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(1)), uint64_t{17});
  EXPECT_EQ(frame.resolve<double>(Reg::Parameter(1)), 3.14);

  frame.set(Reg(2), uint64_t{13});
  frame.set(Reg(1), uint64_t{0});
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(1)), uint64_t{0});
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(2)), uint64_t{13});
}

TEST(StackFrame, ResolveRegOr) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);

  frame.set(Reg(1), uint64_t{17});
  EXPECT_EQ(frame.resolve(RegOr<uint64_t>(Reg(1))), uint64_t{17});
  EXPECT_EQ(frame.resolve(RegOr<uint64_t>(1)), uint64_t{1});
}

TEST(StackFrame, SetRaw) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);

  uint64_t n = 17;
  frame.set_raw(Reg(1), &n, sizeof(uint64_t));
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(1)), uint64_t{17});
}

TEST(StackFrame, Load) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);

  uint64_t n = 17;
  frame.Load(core::Bytes::Get<uint64_t>(),
             RegOr<addr_t>(reinterpret_cast<addr_t>(&n)), Reg(1));
  EXPECT_EQ(frame.resolve<uint64_t>(Reg(1)), 17);
}

TEST(StackFrame, Store) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);

  uint64_t n = 17;
  frame.Store(RegOr<uint64_t>(4), RegOr<addr_t>(reinterpret_cast<addr_t>(&n)));
  EXPECT_EQ(n, 4);
  frame.set(Reg(1), uint64_t{0});
  frame.Store(RegOr<uint64_t>(Reg(1)), RegOr<addr_t>(reinterpret_cast<addr_t>(&n)));
  EXPECT_EQ(n, 0);
}

TEST(StackFrame, FatalError) {
  std::string fatal_error;
  StackFrame frame({.required_stack_space  = core::Bytes(32),
                    .num_parameters        = 2,
                    .num_registers         = 5,
                    .num_outputs           = 1,
                    .num_stack_allocations = 4},
                   fatal_error);
  EXPECT_THAT(fatal_error, IsEmpty());
  frame.FatalError("oh no!");
  EXPECT_EQ(fatal_error, "oh no!");
}

}  // namespace
}  // namespace ir::interpreter
