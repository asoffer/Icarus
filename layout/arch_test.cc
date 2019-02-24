#include "layout/arch.h"

#include "test/test.h"

namespace layout {

TEST(FwdAlign) {
  CHECK(FwdAlign(Bytes{0}, Alignment{1}) == Bytes{0});

  CHECK(FwdAlign(Bytes{0}, Alignment{2}) == Bytes{0});
  CHECK(FwdAlign(Bytes{1}, Alignment{2}) == Bytes{2});

  CHECK(FwdAlign(Bytes{0}, Alignment{8}) == Bytes{0});
  CHECK(FwdAlign(Bytes{1}, Alignment{8}) == Bytes{8});
  CHECK(FwdAlign(Bytes{8}, Alignment{8}) == Bytes{8});
}

}  // namespace layout