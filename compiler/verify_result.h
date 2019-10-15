#ifndef ICARUS_COMPILER_VERIFY_RESULT_H
#define ICARUS_COMPILER_VERIFY_RESULT_H

namespace type {
struct Type;
}  // namespace type

namespace compiler {
struct VerifyResult {
  type::Type const *type_;
  bool const_;

  constexpr VerifyResult() : type_(nullptr), const_(false) {}
  constexpr VerifyResult(type::Type const *t, bool b) : type_(t), const_(b) {}

  // TODO you could actually pass some information through successfully. Like
  // maybe there's a type error but you do at least know it's a constant.
  static constexpr VerifyResult Error() { return VerifyResult{nullptr, false}; }
  static constexpr VerifyResult Constant(type::Type const *t) {
    return VerifyResult{t, true};
  }
  static constexpr VerifyResult NonConstant(type::Type const *t) {
    return VerifyResult{t, false};
  }

  explicit operator bool() const { return type_ != nullptr; }
  bool ok() const { return type_ != nullptr; }
  VerifyResult operator*() const { return *this; }
};

std::ostream &operator<<(std::ostream &os, VerifyResult r);

constexpr bool operator==(VerifyResult lhs, VerifyResult rhs) {
  return lhs.type_ == rhs.type_ and lhs.const_ == rhs.const_;
}

constexpr bool operator!=(VerifyResult lhs, VerifyResult rhs) {
  return not (lhs == rhs);
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_VERIFY_RESULT_H
