#ifndef ICARUS_TYPE_VISITOR_BASE_H
#define ICARUS_TYPE_VISITOR_BASE_H

#include "base/debug.h"
#include "type/type_fwd.h"

namespace type {

struct VisitorBase {
  virtual ~VisitorBase() {}

  virtual void ErasedVisit(LegacyType const *, void *, void *) {
    UNREACHABLE();
  }

  virtual void ErasedVisit(Generic<Function> const *, void *, void *) = 0;
  virtual void ErasedVisit(Generic<Struct> const *, void *, void *)   = 0;

#define ICARUS_TYPE_TYPE_X(subtype)                                            \
  virtual void ErasedVisit(subtype const *, void *, void *) = 0;
#include "type/type.xmacro.h"
#undef ICARUS_TYPE_TYPE_X
};

}  // namespace type

#endif  // ICARUS_TYPE_VISITOR_BASE_H
