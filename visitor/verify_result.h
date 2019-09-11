#ifndef ICARUS_VISITOR_VERIFY_RESULT_H
#define ICARUS_VISITOR_VERIFY_RESULT_H

namespace type {
struct Type;
}  // namespace type

namespace visitor {
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
}  // namespace visitor

#endif  // ICARUS_VISITOR_VERIFY_RESULT_H
