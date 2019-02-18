#ifndef ICARUS_BASE_MACROS_H
#define ICARUS_BASE_MACROS_H

#define CAT(x, y, z) CAT_(x, y, z)
#define CAT_(x, y, z) x##y##z

#define ASSIGN_OR(action, var, expr)                                           \
  ASSIGN_OR_(action, var, expr, CAT(expr__, __LINE__, __))
#define ASSIGN_OR_(action, var, expr, tmp) ASSIGN_OR__(action, var, expr, tmp)

#define ASSIGN_OR__(action, var, expr, temp)                                   \
  auto &&temp = (expr);                                                        \
  if (!temp) {                                                                 \
    auto &&_ = std::move(temp);                                                \
    action;                                                                    \
  }                                                                            \
  var = *std::move(temp)

#endif // ICARUS_BASE_MACROS_H
