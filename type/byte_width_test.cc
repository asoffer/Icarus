#include "type/byte_width.h"

#include "nth/test/test.h"

namespace ic::type {
namespace {

NTH_TEST("byte-width/construction") {
  ByteWidth b(4);
  NTH_EXPECT(b.value() == 4);
  NTH_EXPECT(b == ByteWidth(4));
}

NTH_TEST("byte-width/ordering") {
  ByteWidth b(2);
  NTH_EXPECT(b <= ByteWidth(4));
}

NTH_TEST("byte-width/aligned-forward-to") {
  ByteWidth b(6);
  NTH_EXPECT(b.aligned_forward_to(Alignment(1)) == ByteWidth(6));
  NTH_EXPECT(b.aligned_forward_to(Alignment(2)) == ByteWidth(6));
  NTH_EXPECT(b.aligned_forward_to(Alignment(4)) == ByteWidth(8));
  NTH_EXPECT(b.aligned_forward_to(Alignment(8)) == ByteWidth(8));
  NTH_EXPECT(b.aligned_forward_to(Alignment(16)) == ByteWidth(16));
}

NTH_TEST("byte-width/aligned-backward-to") {
  ByteWidth b(30);
  NTH_EXPECT(b.aligned_backward_to(Alignment(1)) == ByteWidth(30));
  NTH_EXPECT(b.aligned_backward_to(Alignment(2)) == ByteWidth(30));
  NTH_EXPECT(b.aligned_backward_to(Alignment(4)) == ByteWidth(28));
  NTH_EXPECT(b.aligned_backward_to(Alignment(8)) == ByteWidth(24));
  NTH_EXPECT(b.aligned_backward_to(Alignment(16)) == ByteWidth(16));
}

}  // namespace
}  // namespace ic::type
