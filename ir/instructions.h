#ifndef ICARUS_IR_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTIONS_H

#include <memory>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
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
#include "ir/values.h"
#include "type/pointer.h"
#include "type/type.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {
namespace internal {

template <typename SizeType, typename T, typename Fn>
void WriteBits(base::untyped_buffer* buf, absl::Span<T const> span,
               Fn&& predicate) {
  ASSERT(span.size() < std::numeric_limits<SizeType>::max());
  buf->append<SizeType>(span.size());

  uint8_t reg_mask = 0;
  for (size_t i = 0; i < span.size(); ++i) {
    if (predicate(span[i])) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      buf->append(reg_mask);
      reg_mask = 0;
    }
  }
  if (span.size() % 8 != 0) { buf->append(reg_mask); }
}

}  // namespace internal

struct Instruction {
  virtual ~Instruction() {}
  virtual std::string to_string() const { return "[[unknown]]"; }

  virtual void Serialize(base::untyped_buffer* buf) const {}
};

template <typename NumType>
struct UnaryInstruction : Instruction {
  UnaryInstruction(RegOr<NumType> const& operand) : operand(operand) {}

  struct control_bits {
    uint8_t is_reg : 1;
    uint8_t primitive_type : 6;
  };

  template <cmd_index_t CmdIndex>
  void SerializeUnary(base::untyped_buffer* buf) const {
    buf->append(CmdIndex);
    buf->append(control_bits{.is_reg         = operand.is_reg(),
                             .primitive_type = PrimitiveIndex<NumType>()});
    operand.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  };

  RegOr<NumType> operand;
  Reg result;
};

template <typename NumType>
struct BinaryInstruction : Instruction {
  BinaryInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : lhs(lhs), rhs(rhs) {}

  struct control_bits {
    uint8_t lhs_is_reg : 1;
    uint8_t rhs_is_reg : 1;
    uint8_t primitive_type : 6;
  };

  template <cmd_index_t CmdIndex>
  void SerializeBinary(base::untyped_buffer* buf) const {
    buf->append(CmdIndex);
    buf->append(control_bits{.lhs_is_reg     = lhs.is_reg(),
                             .rhs_is_reg     = rhs.is_reg(),
                             .primitive_type = PrimitiveIndex<NumType>()});

    lhs.apply([&](auto v) { buf->append(v); });
    rhs.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename T>
struct VariadicInstruction : Instruction {
  VariadicInstruction(std::vector<RegOr<T>> values)
      : values(std::move(values)) {}

  template <typename Cmd>
  void SerializeVariadic(base::untyped_buffer* buf) const {
    buf->append(Cmd::index);
    internal::WriteBits<uint16_t, RegOr<T>>(
        buf, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append(result);
  };

  std::vector<RegOr<T>> values;
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
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return "enum";
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return "flags";
  } else if constexpr (std::is_same_v<T, type::Type const*>) {
    return "type";
  } else {
    DEBUG_LOG()(typeid(T).name());
    return "[[unknown]]";
    // TODO enumerate all possibilities
    // static_assert(base::always_false<T>());
  }
}

template <typename NumType>
struct NegInstruction : UnaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      NegCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeUnary<kIndex>(buf);
  }
};

struct NotInstruction : UnaryInstruction<bool> {
  using type                          = bool;
  static constexpr cmd_index_t kIndex = NotCmd::index;

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
    SerializeUnary<kIndex>(buf);
  }
};

struct PtrInstruction : UnaryInstruction<type::Type const*> {
  using type                          = type::Type const*;
  static constexpr cmd_index_t kIndex = PtrCmd::index;

  PtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~PtrInstruction() override {}

  static type::Pointer const* Apply(type::Type const* operand) {
    return type::Ptr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    SerializeUnary<kIndex>(buf);
  }
};

struct BufPtrInstruction : UnaryInstruction<type::Type const*> {
  using type                          = type::Type const*;
  static constexpr cmd_index_t kIndex = BufPtrCmd::index;

  BufPtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~BufPtrInstruction() override {}

  static type::BufferPointer const* Apply(type::Type const* operand) {
    return type::BufPtr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    SerializeUnary<kIndex>(buf);
  }
};

// This instruction is a bit strange sets a register to either another registor,
// or an immediate value. By the very nature of Single-Static-Assignment, every
// use of this instruction is an optimization opportunity. If a register is
// initialized with an immediate value, we can do constant propagation. If it is
// initialized with another register, the two registers can be folded into a
// single register.
//
// The benefit of such an instruction is that it enables us to inline code
// without worrying about rewriting register names immediately. This instruction
// should never be visible in the final code.
template <typename T>
struct RegisterInstruction : UnaryInstruction<T> {
  using type = T;
  static constexpr cmd_index_t kIndex =
      RegisterCmd::index | PrimitiveIndex<T>();

  RegisterInstruction(RegOr<T> const& operand) : UnaryInstruction<T>(operand) {}
  ~RegisterInstruction() override {}

  static T Apply(T val) { return val; }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeUnary<kIndex>(buf);
  }
};

template <typename NumType>
struct AddInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      AddCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct SubInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      SubCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct MulInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      MulCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct DivInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      DivCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct ModInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      ModCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct EqInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      EqCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct NeInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      NeCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct LtInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      LtCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
  }
};

template <typename NumType>
struct LeInstruction : BinaryInstruction<NumType> {
  using type = NumType;
  static constexpr cmd_index_t kIndex =
      LeCmd::index | PrimitiveIndex<NumType>();

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
    this->template SerializeBinary<kIndex>(buf);
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

struct PrintEnumInstruction : Instruction {
  PrintEnumInstruction(RegOr<EnumVal> const& value, type::Enum const* enum_type)
      : value(value), enum_type(enum_type) {}
  ~PrintEnumInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("print enum ", enum_type->to_string(), " ",
                        stringify(value));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(PrintCmd::index);
    buf->append(PrintCmd::MakeControlBits<EnumVal>(value.is_reg()));
    value.apply([&](auto v) { buf->append(v); });
    buf->append(enum_type);
  }

  RegOr<EnumVal> value;
  type::Enum const* enum_type;
};

struct PrintFlagsInstruction : Instruction {
  PrintFlagsInstruction(RegOr<FlagsVal> const& value,
                        type::Flags const* flags_type)
      : value(value), flags_type(flags_type) {}
  ~PrintFlagsInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("print flags ", flags_type->to_string(), " ",
                        stringify(value));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(PrintCmd::index);
    buf->append(PrintCmd::MakeControlBits<FlagsVal>(value.is_reg()));
    value.apply([&](auto v) { buf->append(v); });
    buf->append(flags_type);
  }

  RegOr<FlagsVal> value;
  type::Flags const* flags_type;
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

struct XorFlagsInstruction : BinaryInstruction<FlagsVal> {
  XorFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~XorFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs ^ rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = add ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<XorFlagsCmd::index>(buf);
  }
};

struct AndFlagsInstruction : BinaryInstruction<FlagsVal> {
  AndFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~AndFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs & rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = and ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<AndFlagsCmd::index>(buf);
  }
};

struct OrFlagsInstruction : BinaryInstruction<FlagsVal> {
  OrFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~OrFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs | rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = or ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeBinary<OrFlagsCmd::index>(buf);
  }
};

struct TupleInstruction : VariadicInstruction<type::Type const*> {
  TupleInstruction(std::vector<RegOr<type::Type const*>> values)
      : VariadicInstruction<type::Type const*>(std::move(values)) {}
  ~TupleInstruction() override {}

  static type::Type const* Apply(std::vector<type::Type const*> entries) {
    return type::Tup(std::move(entries));
  }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = tup ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeVariadic<TupleCmd>(buf);
  }
};

struct VariantInstruction : VariadicInstruction<type::Type const*> {
  VariantInstruction(std::vector<RegOr<type::Type const*>> values)
      : VariadicInstruction<type::Type const*>(std::move(values)) {}
  ~VariantInstruction() override {}

  static type::Type const* Apply(std::vector<type::Type const*> entries) {
    return type::Var(std::move(entries));
  }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = var ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->template SerializeVariadic<VariantCmd>(buf);
  }
};
}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_H
