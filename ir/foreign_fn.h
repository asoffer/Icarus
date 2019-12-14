#ifndef ICARUS_IR_FOREIGN_FN_H
#define ICARUS_IR_FOREIGN_FN_H

// TODO depend on function directly, which involves moving core::Intepretter.
namespace type {
struct Function;
}  // namespace type

namespace ir {
struct AnyFunc;

struct ForeignFn {
  explicit ForeignFn(void (*fn)(), type::Function const *t);

  void (*get())() const { return fn_; }
  type::Function const *type() const;

 private:
  ForeignFn() {}

  friend struct AnyFunc;

  void (*fn_)() = nullptr;
};

inline bool operator==(ForeignFn lhs, ForeignFn rhs) {
  return lhs.get() == rhs.get();
}
inline bool operator<(ForeignFn lhs, ForeignFn rhs) {
  return lhs.get() < rhs.get();
}
inline bool operator>(ForeignFn lhs, ForeignFn rhs) {
  return rhs.get() < lhs.get();
}

}  // namespace ir

#endif  // ICARUS_IR_FOREIGN_FN_H
