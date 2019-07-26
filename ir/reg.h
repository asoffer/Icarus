#ifndef ICARUS_IR_REG_H
#define ICARUS_IR_REG_H

#include <string>

namespace ir {
struct Reg {
 public:
  constexpr Reg() : val_(std::numeric_limits<uint64_t>::max()){};
  constexpr explicit Reg(uint64_t val) : val_(val) {}
  constexpr static Reg Arg(uint64_t val) { return MakeReg(val | arg_mask); }
  constexpr static Reg Out(uint64_t val) { return MakeReg(val | arg_mask); }

  constexpr bool is_arg() { return val_ & arg_mask; }
  constexpr bool is_out() { return val_ & out_mask; }
  constexpr auto value() const { return val_; }

  template <typename H>
  friend H AbslHashValue(H h, Reg r) {
    return H::combine(std::move(h), r.val_);
  }

  friend std::string stringify(Reg r) {
    using base::stringify;
    if (r.is_arg()) { return "arg." + stringify(r.value() & ~Reg::arg_mask); }
    if (r.is_out()) { return "out." + stringify(r.value() & ~Reg::out_mask); }
    return "r." + stringify(r.value());
  }

 private:
  constexpr static Reg MakeReg(uint64_t val) {
    Reg r;
    r.val_ = val;
    return r;
  }

  // NOTE: Do *not* use the top bit here. We need it in ir::Results
  constexpr static uint64_t arg_mask = 0x4000'0000'0000'0000;
  constexpr static uint64_t out_mask = 0x2000'0000'0000'0000;
  uint64_t val_;
};

constexpr bool operator==(Reg lhs, Reg rhs) {
  return lhs.value() == rhs.value();
}
constexpr bool operator!=(Reg lhs, Reg rhs) { return !(lhs == rhs); }

template <typename T>
struct RegOr {
  using type = T;
  static_assert(!std::is_same_v<Reg, T>);
  RegOr() : is_reg_(true) {}

  RegOr(Reg reg) : reg_(reg), is_reg_(true) {}
  RegOr(T val) : val_(val), is_reg_(false) {}

  union {
    Reg reg_;
    T val_;
  };
  bool is_reg_;

  inline friend std::ostream &operator<<(std::ostream &os, RegOr const &r) {
    if (r.is_reg_) {
      return os << stringify(r.reg_);
    } else {
      using base::stringify;
      return os << stringify(r.val_);
    }
  }
};

template <typename T>
bool operator==(RegOr<T> const &lhs, RegOr<T> const &rhs) {
  if (lhs.is_reg_) { return rhs.is_reg_ && lhs.reg_ == rhs.reg_; }
  return !rhs.is_reg_ && lhs.val_ == rhs.val_;
}

template <typename T>
std::string stringify(RegOr<T> const &r) {
  std::stringstream ss;
  ss << r;
  return ss.str();
}

template <typename T>
struct IsRegOr : public std::false_type {};
template <typename T>
struct IsRegOr<RegOr<T>> : public std::true_type {
  using type = T;
};


}  // namespace ir

#endif  // ICARUS_IR_REG_H
