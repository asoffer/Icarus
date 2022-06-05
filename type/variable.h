#ifndef ICARUS_TYPE_VARIABLE_H
#define ICARUS_TYPE_VARIABLE_H

#include "ir/value/interface.h"
#include "type/type.h"

namespace type {

struct Variable : LegacyType {
  friend Variable const *Var(ir::Interface);

  void WriteTo(std::string *buf) const override { buf->append("var(?)"); }
  core::Bytes bytes(core::Arch const &arch) const override { UNREACHABLE(); }
  core::Alignment alignment(core::Arch const &arch) const override { UNREACHABLE(); }

  bool is_big() const override { return false; }

  ir::Interface interface() const { return interface_; }

  bool EqualsValue(ir::CompleteResultRef const &lhs,
                   ir::CompleteResultRef const &rhs) const override {
    NOT_YET();
  }
  size_t HashValue(ir::CompleteResultRef const &value) const override {
    NOT_YET();
  }
  void ShowValue(std::ostream &os,
                 ir::CompleteResultRef const &value) const override {
    NOT_YET();
  }

  Completeness completeness() const override { return Completeness::Complete; }

  template <typename H>
  friend H AbslHashValue(H h, Variable const &v) {
    return H::combine(std::move(h), v.interface_);
  }
  friend bool operator==(Variable const &lhs, Variable const &rhs) {
    return lhs.interface_ == rhs.interface_;
  }

 private:
  Variable(ir::Interface intf)
      : LegacyType(IndexOf<Variable>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 0,
                                     .is_movable               = 0,
                                     .has_destructor           = 0}),
        interface_(intf) {}
  ir::Interface interface_;
};

}  // namespace type

#endif  // ICARUS_TYPE_VARIABLE_H
