#ifndef ICARUS_TYPE_BLOCK_H
#define ICARUS_TYPE_BLOCK_H

#include "type.h"

namespace type {
struct ScopeBlock : public Type {
  TYPE_FNS(ScopeBlock);
  ScopeBlock(bool required) : required_(required) {}

  bool required_ = false;
};

const ScopeBlock* Blk(bool required);
}  // namespace type

#endif // ICARUS_TYPE_BLOCK_H
