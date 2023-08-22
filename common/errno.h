#ifndef ICARUS_COMMON_ERRNO_H
#define ICARUS_COMMON_ERRNO_H

#include <cerrno>

namespace ic {

struct errno_resetter {
  ~errno_resetter() { errno = saved_; }
  int saved_ = errno;
};

}  // namespace ic
#endif  // ICARUS_COMMON_ERRNO_H
