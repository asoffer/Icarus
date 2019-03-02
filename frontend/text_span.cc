#include "frontend/text_span.h"

#include "base/debug.h"

TextSpan::TextSpan(TextSpan const &s, TextSpan const &f)
    : start(s.start), finish(f.finish), source(ASSERT_NOT_NULL(s.source)) {
  ASSERT(s.source == f.source);
}
