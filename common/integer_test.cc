#include "common/integer.h"

#include "nth/test/test.h"

namespace ic {

NTH_TEST("integer/equality", auto x, auto y) {
  if (x == y) {
    NTH_EXPECT(Integer(x) == Integer(y));
    NTH_EXPECT(not(Integer(x) != Integer(y)));
  } else {
    NTH_EXPECT(Integer(x) != Integer(y));
    NTH_EXPECT(not(Integer(x) == Integer(y)));
  }
}

NTH_INVOKE_TEST("integer/equality") {
  co_yield nth::TestArguments{0, 0};
  co_yield nth::TestArguments{0, 0u};
  co_yield nth::TestArguments{0u, 0u};
  co_yield nth::TestArguments{1, 0};
  co_yield nth::TestArguments{1, 1};
  co_yield nth::TestArguments{1, 1u};
  co_yield nth::TestArguments{-1, 1};
  co_yield nth::TestArguments{-1, 1u};
  co_yield nth::TestArguments{uint32_t{1} << 23, uint32_t{1} << 23};
  co_yield nth::TestArguments{uint64_t{1} << 23, uint32_t{1} << 23};
  co_yield nth::TestArguments{std::numeric_limits<int64_t>::min(),
                              std::numeric_limits<int64_t>::max()};
  co_yield nth::TestArguments{std::numeric_limits<int64_t>::max() - 1,
                              std::numeric_limits<int64_t>::max()};
  co_yield nth::TestArguments{std::numeric_limits<int64_t>::max(),
                              std::numeric_limits<int64_t>::max()};
}

NTH_TEST("integer/round-trip-representation", Integer n) {
  NTH_EXPECT(Integer::FromRepresentation(Integer::ToRepresentation(n)) == n);
}

NTH_INVOKE_TEST("integer/round-trip-representation") {
  co_yield 0;
  co_yield 1;
  co_yield -1;
  co_yield std::numeric_limits<uint16_t>::max();
  co_yield std::numeric_limits<uint32_t>::max();
  co_yield std::numeric_limits<uint64_t>::max();
}

}  // namespace ic
