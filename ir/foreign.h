#ifndef ICARUS_IR_FOREIGN_H
#define ICARUS_IR_FOREIGN_H

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct AnyFunc;

struct Foreign {
  Foreign(void *obj, type::Type const *t);

  void *get() const { return obj_; }
  type::Type const *type() const;

 private:
  Foreign() {}

  friend struct AnyFunc;

  void *obj_ = nullptr;
};

inline bool operator==(Foreign lhs, Foreign rhs) {
  return lhs.get() == rhs.get();
}
inline bool operator<(Foreign lhs, Foreign rhs) {
  return lhs.get() < rhs.get();
}
inline bool operator>(Foreign lhs, Foreign rhs) {
  return rhs.get() < lhs.get();
}

}  // namespace ir

#endif  // ICARUS_IR_FOREIGN_H
