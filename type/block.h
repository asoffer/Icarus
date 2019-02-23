#ifndef ICARUS_TYPE_BLOCK_H
#define ICARUS_TYPE_BLOCK_H

#include "type/callable.h"

namespace type {
struct Block : public Callable {
  ~Block() override {}
  BASIC_METHODS;
};

Block *Blk();
}  // namespace type

#endif // ICARUS_TYPE_BLOCK_H
