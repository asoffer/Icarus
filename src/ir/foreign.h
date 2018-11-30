#ifndef ICARUS_IR_FOREIGN_H
#define ICARUS_IR_FOREIGN_H

#include <string_view>

namespace type {
struct Function;
}  // namespace type

namespace ast {
struct Expression;
}  // namespace ast

namespace ir {

struct ForeignFn {
  explicit ForeignFn(std::string_view name, ast::Expression const *expr,
                     type::Function const *t);
  std::string_view name() const { return handle_->first; }
  type::Function const *type() const { return handle_->second.type_; }

  struct Data {
    ast::Expression const *expr_ = nullptr;
    type::Function const *type_  = nullptr;
  };

 private:
  friend struct AnyFunc;

  ForeignFn()                                            = default;
  std::pair<std::string_view const, Data> const *handle_ = nullptr;
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
