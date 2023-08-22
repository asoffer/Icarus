#include "common/errno.h"

#include "nth/test/test.h"

namespace ic {

NTH_TEST("errno/success") {
  errno = 0;
  {
    errno_resetter e;
    errno = EACCES;
  }
  NTH_EXPECT(errno == 0);
}

NTH_TEST("errno/error") {
  errno = EACCES;
  {
    errno_resetter e;
    errno = 0;
  }
  NTH_EXPECT(errno == EACCES);
}

NTH_TEST("errno/nested") {
  errno = 0;
  {
    errno_resetter e;
    errno = EACCES;
    {
      errno_resetter e;
      errno = EAGAIN;
      NTH_EXPECT(errno == EAGAIN);
    }
    NTH_EXPECT(errno == EACCES);
  }
  NTH_EXPECT(errno == 0);
}

}  // namespace ic
