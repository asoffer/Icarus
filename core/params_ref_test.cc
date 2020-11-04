#include "core/params_ref.h"
#include "core/arguments.h"
#include "core/params.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

using ::testing::ElementsAre;

TEST(ParamsRef, Iteration) {
  Params<int> params{Param<int>{"a", 1}, Param<int>{"b", 2}, Param<int>{"", 3},
                     Param<int>{"", 4}, Param<int>{"e", 5}};
  ParamsRef<int> ref(params);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"a", 1}, Param<int>{"b", 2},
                               Param<int>{"", 3}, Param<int>{"", 4},
                               Param<int>{"e", 5}));
}

TEST(ParamsRef, RemovePrefix) {
  Params<int> params{Param<int>{"a", 1}, Param<int>{"b", 2}, Param<int>{"", 3},
                     Param<int>{"", 4}, Param<int>{"e", 5}};
  ParamsRef<int> ref(params);
  ref.remove_prefix(0);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"a", 1}, Param<int>{"b", 2},
                               Param<int>{"", 3}, Param<int>{"", 4},
                               Param<int>{"e", 5}));

  ref.remove_prefix(1);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"b", 2}, Param<int>{"", 3},
                               Param<int>{"", 4}, Param<int>{"e", 5}));
}

}  // namespace
}  // namespace core
