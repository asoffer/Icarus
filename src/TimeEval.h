#ifndef ICARUS_TIME_H
#define ICARUS_TIME_H

namespace Time {
  using Eval = int;
  constexpr Eval either  = 0x0;
  constexpr Eval compile = 0x1;
  constexpr Eval run     = 0x2;
  constexpr Eval mixed   = 0x3;
  constexpr Eval error   = 0x7;
}  // namespace Time

#endif  // ICARUS_TIME_H
