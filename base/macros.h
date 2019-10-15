#ifndef ICARUS_BASE_MACROS_H
#define ICARUS_BASE_MACROS_H

#define ICARUS_CAT(x, y, z) ICARUS_CAT_IMPL(x, y, z)
#define ICARUS_CAT_IMPL(x, y, z) x##y##z

#define ASSIGN_OR(action, var, expr)                                           \
  ASSIGN_OR_IMPL1(action, var, expr, ICARUS_CAT(expr__, __LINE__, __))
#define ASSIGN_OR_IMPL1(action, var, expr, tmp)                                \
  ASSIGN_OR_IMPL2(action, var, expr, tmp)

#define ASSIGN_OR_IMPL2(action, var, expr, temp)                               \
  auto &&temp = (expr);                                                        \
  if (not temp) {                                                              \
    auto &&_ = std::move(temp);                                                \
    action;                                                                    \
  }                                                                            \
  var = *std::move(temp)

#endif  // ICARUS_BASE_MACROS_H
