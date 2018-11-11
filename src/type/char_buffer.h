#ifndef ICARUS_TYPE_CHAR_BUFFER_H
#define ICARUS_TYPE_CHAR_BUFFER_H

#include "type.h"

struct Context;

namespace type {
struct CharBuffer : public Type {
  TYPE_FNS(CharBuffer);
  CharBuffer(size_t len) : length_(len) {}

  static ir::Val Compare(const CharBuffer *lhs_type, ir::Val lhs_ir,
                         const CharBuffer *rhs_type, ir::Val rhs_ir,
                         bool equality, Context *ctx);

  virtual bool needs_destroy() const { return false; }

  size_t length_;
};

const CharBuffer *CharBuf(size_t len);
}  // namespace type
#endif  // ICARUS_TYPE_CHAR_BUFFER_H
