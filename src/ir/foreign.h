#ifndef ICARUS_IR_FOREIGN_H
#define ICARUS_IR_FOREIGN_H

#include <string_view>

namespace ast {
struct Expression;
}  // namespace ast

namespace ir {

struct ForeignFn {
  explicit ForeignFn(std::string_view name, ast::Expression const *expr);

  std::string_view name() const { return handle_->first; }

 private:
  friend struct AnyFunc;
  ForeignFn() = default;
  std::pair<std::string_view const, ast::Expression const *> const *handle_ = nullptr;
};

inline bool operator==(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name() == rhs.name();
}
inline bool operator<(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name() < rhs.name();
}
inline bool operator>(ForeignFn lhs, ForeignFn rhs) {
  return rhs.name() < lhs.name();
}

}  // namespace ir

#endif  // ICARUS_IR_FOREIGN_H
