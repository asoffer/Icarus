#ifndef ICARUS_IR_NEW_INLINER_H
#define ICARUS_IR_NEW_INLINER_H

namespace ir {
struct Inliner {
  explicit Inliner(int register_offset) : register_offset_(register_offset) {}

  void Inline(Reg &r) const { r = Reg(r.value() + register_offset_); }

  template <typename T>
  void Inline(RegOr<T> &r) const {
    if (r.is_reg()) {
      Reg copy = r.reg();
      Inline(copy);
      r = copy;
    }
  }

  template <typename T>
  void Inline(std::vector<RegOr<T>> &rs) const {
    for (auto &r : rs) { Inline(r); }
  }

 private:
  int register_offset_;
};
}  // namespace ir

#endif  // ICARUS_IR_NEW_INLINER_H
