#include "common/constant.h"

#include <vector>

#include "nth/debug/debug.h"
#include "nth/test/test.h"

namespace ic {
namespace {

using ::nth::debug::ElementsAreSequentially;

NTH_TEST("appender/default-construction") {
  std::vector<uint64_t> v;
  {
    ConstantComponentAppender a(v);
    NTH_EXPECT(v.empty());
  }

  NTH_EXPECT(v.empty());
}

NTH_TEST("appender/append") {
  std::vector<uint64_t> v;
  ConstantComponentAppender a(v);

  a.append(1);
  NTH_EXPECT(v >>= ElementsAreSequentially(1));
}

}  // namespace
}  // namespace ic
