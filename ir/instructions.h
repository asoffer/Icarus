#ifndef ICARUS_IR_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTIONS_H

#include <memory>

#include "absl/strings/str_cat.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jump.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/cmd/util.h"
#include "ir/reg_or.h"
#include "type/pointer.h"
#include "type/type.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {

struct Instruction {
  virtual ~Instruction() {}
  virtual std::string to_string() const { return "[[unknown]]"; }

  virtual void Serialize(base::untyped_buffer* buf) const {}
};

template <typename NumType>
struct UnaryInstruction : Instruction {
  UnaryInstruction(RegOr<NumType> const& operand) : operand(operand) {}

  template <typename Cmd>
  void SerializeUnary(base::untyped_buffer* buf) const {
    buf->append(Cmd::index);
    buf->append(Cmd::template MakeControlBits<NumType>(operand.is_reg()));
    this->operand.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  };

  RegOr<NumType> operand;
  Reg result;
};

template <typename NumType>
struct BinaryInstruction : Instruction {
  BinaryInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : lhs(lhs), rhs(rhs) {}

  template <typename Cmd>
  void SerializeBinary(base::untyped_buffer* buf) const {
    buf->append(Cmd::index);
    buf->append(
        Cmd::template MakeControlBits<NumType>(lhs.is_reg(), rhs.is_reg()));

    lhs.apply([&](auto v) { buf->append(v); });
    rhs.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename T>
std::string_view TypeToString() {
  if constexpr (std::is_same_v<T, bool>) {
    return "bool";
  } else if constexpr (std::is_same_v<T, int8_t>) {
    return "int8";
  } else if constexpr (std::is_same_v<T, int16_t>) {
    return "int16";
  } else if constexpr (std::is_same_v<T, int32_t>) {
    return "int32";
  } else if constexpr (std::is_same_v<T, int64_t>) {
    return "int64";
  } else if constexpr (std::is_same_v<T, uint8_t>) {
    return "nat8";
  } else if constexpr (std::is_same_v<T, uint16_t>) {
    return "nat16";
  } else if constexpr (std::is_same_v<T, uint32_t>) {
    return "nat32";
  } else if constexpr (std::is_same_v<T, uint64_t>) {
    return "nat64";
  } else if constexpr (std::is_same_v<T, float>) {
    return "float32";
  } else if constexpr (std::is_same_v<T, double>) {
    return "float64";
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    return "bytes";
  } else {
    return "[[unknown]]";
    // TODO enumerate all possibilities
    // static_assert(base::always_false<T>());
  }
}

template <typename NumType>
struct NegInstruction : UnaryInstruction<NumType> {
  NegInstruction(RegOr<NumType> const& operand)
      : UnaryInstruction<NumType>(operand) {}
  ~NegInstruction() override {}

  static NumType Apply(NumType operand) { return -operand; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = neg ", stringify(this->operand));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeUnary<NegCmd>(buf);
  }
};

struct NotInstruction : UnaryInstruction<bool> {
  NotInstruction(RegOr<bool> const& operand)
      : UnaryInstruction<bool>(operand) {}
  ~NotInstruction() override {}

  static bool Apply(bool operand) { return not operand; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<bool>(), " ", stringify(this->result),
                        " = not ", stringify(this->operand));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    SerializeUnary<NotCmd>(buf);
  }
};

struct PtrInstruction : UnaryInstruction<type::Type const*> {
  PtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~PtrInstruction() override {}

  static type::Pointer const* Apply(type::Type const* operand) {
    return type::Ptr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    SerializeUnary<PtrCmd>(buf);
  }
};

struct BufPtrInstruction : UnaryInstruction<type::Type const*> {
  BufPtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~BufPtrInstruction() override {}

  static type::BufferPointer const* Apply(type::Type const* operand) {
    return type::BufPtr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    SerializeUnary<BufPtrCmd>(buf);
  }
};

template <typename NumType>
struct AddInstruction : BinaryInstruction<NumType> {
  AddInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~AddInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs + rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = add ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<AddCmd>(buf);
  }
};

template <typename NumType>
struct SubInstruction : BinaryInstruction<NumType> {
  SubInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~SubInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs - rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = sub ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<SubCmd>(buf);
  }
};

template <typename NumType>
struct MulInstruction : BinaryInstruction<NumType> {
  MulInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~MulInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs * rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = mul ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<MulCmd>(buf);
  }
};

template <typename NumType>
struct DivInstruction : BinaryInstruction<NumType> {
  DivInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~DivInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs / rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = div ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<DivCmd>(buf);
  }
};

template <typename NumType>
struct ModInstruction : BinaryInstruction<NumType> {
  ModInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~ModInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs % rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = mod ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<ModCmd>(buf);
  }
};

template <typename NumType>
struct EqInstruction : BinaryInstruction<NumType> {
  EqInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~EqInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs == rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = eq ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<EqCmd>(buf);
  }
};

template <typename NumType>
struct NeInstruction : BinaryInstruction<NumType> {
  NeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~NeInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs != rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = ne ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<NeCmd>(buf);
  }
};

template <typename NumType>
struct LtInstruction : BinaryInstruction<NumType> {
  LtInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~LtInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs < rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = lt ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<LtCmd>(buf);
  }
};

template <typename NumType>
struct LeInstruction : BinaryInstruction<NumType> {
  LeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~LeInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs <= rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = le ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<LeCmd>(buf);
  }
};

template <typename T>
struct LoadInstruction : Instruction {
  LoadInstruction(RegOr<Addr> const& addr) : addr(addr) {}
  ~LoadInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<T>(), " ", stringify(this->result),
                        " = load ", stringify(addr));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(LoadCmd::index);
    buf->append(LoadCmd::MakeControlBits<T>(addr.is_reg()));
    addr.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  RegOr<Addr> addr;
  Reg result;
};

template <typename T>
struct StoreInstruction : Instruction {
  StoreInstruction(RegOr<T> const& value, RegOr<Addr> const& location)
      : value(value), location(location) {}
  ~StoreInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<T>(), " store ", stringify(this->value),
                        " -> ", stringify(location));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(StoreCmd::index);
    buf->append(
        StoreCmd::MakeControlBits<T>(value.is_reg(), location.is_reg()));
    value.apply([&](auto v) { buf->append(v); });
    location.apply([&](auto v) { buf->append(v); });
  }

  RegOr<T> value;
  RegOr<Addr> location;
};

template <typename T>
struct PrintInstruction : Instruction {
  PrintInstruction(RegOr<T> const& value) : value(value) {}
  ~PrintInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("print ", TypeToString<T>(), " ", stringify(value));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(PrintCmd::index);
    buf->append(PrintCmd::MakeControlBits<T>(value.is_reg()));
    value.apply([&](auto v) { buf->append(v); });
  }

  RegOr<T> value;
};

// TODO Morph this into interpretter break-point instructions.
struct DebugIrInstruction : Instruction {
  DebugIrInstruction() = default;
  ~DebugIrInstruction() override {}

  std::string to_string() const override { return "debug-ir"; }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(DebugIrCmd::index);
  }
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_H
