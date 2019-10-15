#ifndef ICARUS_COMPILER_VERIFY_RESULT_H
#define ICARUS_COMPILER_VERIFY_RESULT_H

namespace type {
struct Type;
}  // namespace type

namespace compiler {
struct VerifyResult {
  type::Type const *type() const {
    return reinterpret_cast<type::Type const *>(data_ & ~uintptr_t{1});
  }

  constexpr bool constant() const { return data_ & uintptr_t{1}; }

  constexpr VerifyResult() {}
  VerifyResult(type::Type const *t, bool b)
      : data_(reinterpret_cast<uintptr_t>(t) | b) {}

  // TODO you could actually pass some information through successfully. Like
  // maybe there's a type error but you do at least know it's a constant.
  static constexpr VerifyResult Error() { return VerifyResult(); }
  static VerifyResult Constant(type::Type const *t) {
    return VerifyResult{t, true};
  }
  static VerifyResult NonConstant(type::Type const *t) {
    return VerifyResult{t, false};
  }

  explicit operator bool() const { return type() != nullptr; }
  bool ok() const { return static_cast<bool>(*this); }
  VerifyResult operator*() const { return *this; }

  friend constexpr bool operator==(VerifyResult lhs, VerifyResult rhs) {
    return lhs.data_ == rhs.data_;
  }

  friend constexpr bool operator!=(VerifyResult lhs, VerifyResult rhs) {
    return !(lhs == rhs);
  }

 private:
  uintptr_t data_ = 0;
};

std::ostream &operator<<(std::ostream &os, VerifyResult r);

}  // namespace compiler

#endif  // ICARUS_COMPILER_VERIFY_RESULT_H
