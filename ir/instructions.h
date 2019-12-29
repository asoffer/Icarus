#ifndef ICARUS_IR_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTIONS_H

#include <memory>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/scope/scope.h"
#include "base/clone.h"
#include "ir/instructions_base.h"
#include "ir/new_inliner.h"
#include "ir/out_params.h"
#include "ir/reg_or.h"
#include "ir/struct_field.h"
#include "ir/values.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/variant.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {
using cmd_index_t = uint16_t;

namespace internal {

template <typename T>
constexpr uint8_t PrimitiveIndex() {
  if constexpr (std::is_integral_v<T> and not std::is_same_v<T, bool>) {
    return base::Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else if constexpr (std::is_same_v<T, bool>) {
    return 0x08;
  } else if constexpr (std::is_same_v<T, float>) {
    return 0x09;
  } else if constexpr (std::is_same_v<T, double>) {
    return 0x0a;
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    return 0x0b;
  } else if constexpr (std::is_same_v<T, type::Type const*>) {
    return 0x0c;
  } else if constexpr (std::is_same_v<T, Addr>) {
    return 0x0d;
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return 0x0e;
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return 0x0f;
  } else if constexpr (std::is_same_v<T, AnyFunc>) {
    return 0x10;
  } else if constexpr (std::is_same_v<T, core::Alignment>) {
    return 0x11;
  } else if constexpr (std::is_same_v<T, core::Bytes>) {
    return 0x12;
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral*> or
                       std::is_same_v<T, ast::FunctionLiteral const*>) {
    // TODO: FunctionLiteral is a short-term hack for generics. IR shouldn't
    // depend on it.
    return 0x13;
  } else if constexpr (std::is_same_v<T, BlockDef*> or
                       std::is_same_v<T, BlockDef const*>) {
    return 0x14;
  } else if constexpr (std::is_same_v<T, ScopeDef*> or
                       std::is_same_v<T, ScopeDef const*>) {
    return 0x15;
  } else if constexpr (std::is_same_v<T, module::BasicModule*> or
                       std::is_same_v<T, module::BasicModule const*>) {
    return 0x16;
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

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

template <typename NumType>
struct UnaryInstruction : base::Clone<UnaryInstruction<NumType>, Instruction> {
  using type = NumType;

  explicit UnaryInstruction(RegOr<NumType> const& operand) : operand(operand) {}
  ~UnaryInstruction() override {}

  void Serialize(base::untyped_buffer* buf) const override {
    UNREACHABLE("Should call a child class");
  }

  void SerializeUnary(cmd_index_t index, base::untyped_buffer* buf) const {
    buf->append(index);
    buf->append(operand.is_reg());
    operand.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  };

  void Inline(Inliner const& inliner) override {
    inliner.Inline(operand);
    inliner.Inline(result);
  }

  RegOr<NumType> operand;
  Reg result;
};

template <typename NumType>
struct BinaryInstruction
    : base::Clone<BinaryInstruction<NumType>, Instruction> {
  using type = NumType;

  BinaryInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : lhs(lhs), rhs(rhs) {}
  ~BinaryInstruction() override {}

  struct control_bits {
    uint8_t lhs_is_reg : 1;
    uint8_t rhs_is_reg : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    UNREACHABLE("Should call a child class");
  }

  void SerializeBinary(cmd_index_t index, base::untyped_buffer* buf) const {
    buf->append(index);
    buf->append(control_bits{
        .lhs_is_reg = lhs.is_reg(),
        .rhs_is_reg = rhs.is_reg(),
    });

    lhs.apply([&](auto v) { buf->append(v); });
    rhs.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename T>
struct VariadicInstruction : base::Clone<VariadicInstruction<T>, Instruction> {
  using type = T;

  VariadicInstruction(std::vector<RegOr<T>> values)
      : values(std::move(values)) {}
  ~VariadicInstruction() override {}

  void Serialize(base::untyped_buffer* buf) const override {
    UNREACHABLE("Should call a child class");
  }

  void SerializeVariadic(cmd_index_t index, base::untyped_buffer* buf) const {
    buf->append(index);
    internal::WriteBits<uint16_t, RegOr<T>>(
        buf, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append(result);
  };

  void Inline(Inliner const& inliner) override {
    inliner.Inline(values);
    inliner.Inline(result);
  }

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

namespace internal {
inline constexpr uint8_t kTypeBits    = 8;
inline constexpr uint16_t kAdHocStart = (18 << kTypeBits);

inline constexpr cmd_index_t kAddInstructionMask             = 0 << kTypeBits;
inline constexpr cmd_index_t kSubInstructionMask             = 1 << kTypeBits;
inline constexpr cmd_index_t kMulInstructionMask             = 2 << kTypeBits;
inline constexpr cmd_index_t kDivInstructionMask             = 3 << kTypeBits;
inline constexpr cmd_index_t kModInstructionMask             = 4 << kTypeBits;
inline constexpr cmd_index_t kLtInstructionMask              = 6 << kTypeBits;
inline constexpr cmd_index_t kLeInstructionMask              = 7 << kTypeBits;
inline constexpr cmd_index_t kEqInstructionMask              = 8 << kTypeBits;
inline constexpr cmd_index_t kNeInstructionMask              = 9 << kTypeBits;
inline constexpr cmd_index_t kNegInstructionMask             = 10 << kTypeBits;
inline constexpr cmd_index_t kPrintInstructionMask           = 11 << kTypeBits;
inline constexpr cmd_index_t kStoreInstructionMask           = 12 << kTypeBits;
inline constexpr cmd_index_t kLoadInstructionMask            = 13 << kTypeBits;
inline constexpr cmd_index_t kPhiInstructionMask             = 14 << kTypeBits;
inline constexpr cmd_index_t kRegisterInstructionMask        = 15 << kTypeBits;
inline constexpr cmd_index_t kSetReturnInstructionMask       = 16 << kTypeBits;
inline constexpr cmd_index_t kNotInstructionNumber           = kAdHocStart;
inline constexpr cmd_index_t kXorFlagsInstructionNumber      = kAdHocStart + 1;
inline constexpr cmd_index_t kAndFlagsInstructionNumber      = kAdHocStart + 2;
inline constexpr cmd_index_t kOrFlagsInstructionNumber       = kAdHocStart + 3;
inline constexpr cmd_index_t kPtrInstructionNumber           = kAdHocStart + 4;
inline constexpr cmd_index_t kBufPtrInstructionNumber        = kAdHocStart + 5;
inline constexpr cmd_index_t kGetReturnInstructionIndex      = kAdHocStart + 6;
inline constexpr cmd_index_t kOpaqueTypeInstructionNumber    = kAdHocStart + 7;
inline constexpr cmd_index_t kArrowInstructionNumber         = kAdHocStart + 8;
inline constexpr cmd_index_t kCallInstructionNumber          = kAdHocStart + 9;
inline constexpr cmd_index_t kLoadSymbolInstructionNumber    = kAdHocStart + 10;
inline constexpr cmd_index_t kArrayInstructionNumber         = kAdHocStart + 11;
inline constexpr cmd_index_t kStructInstructionNumber        = kAdHocStart + 12;
inline constexpr cmd_index_t kMakeBlockInstructionNumber     = kAdHocStart + 13;
inline constexpr cmd_index_t kMakeScopeInstructionNumber     = kAdHocStart + 14;
inline constexpr cmd_index_t kStructIndexInstructionNumber   = kAdHocStart + 15;
inline constexpr cmd_index_t kTupleIndexInstructionNumber    = kAdHocStart + 16;
inline constexpr cmd_index_t kPtrIncrInstructionNumber       = kAdHocStart + 17;
inline constexpr cmd_index_t kVariantAccessInstructionNumber = kAdHocStart + 18;
inline constexpr cmd_index_t kTupleInstructionNumber         = kAdHocStart + 19;
inline constexpr cmd_index_t kVariantInstructionNumber       = kAdHocStart + 20;
inline constexpr cmd_index_t kEnumerationInstructionNumber   = kAdHocStart + 21;
inline constexpr cmd_index_t kTypeInfoInstructionNumber      = kAdHocStart + 22;
inline constexpr cmd_index_t kStructManipulationInstructionNumber =
    kAdHocStart + 23;

inline constexpr cmd_index_t kCastInstructionIndex     = 17 << kTypeBits;
inline constexpr cmd_index_t kDebugIrInstructionNumber = 255 << kTypeBits;

}  // namespace internal

template <typename NumType>
struct AddInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kAddInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct SubInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kSubInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct MulInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kMulInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct DivInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kDivInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct ModInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kModInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct EqInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kEqInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct NeInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kNeInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct LtInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kLtInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

template <typename NumType>
struct LeInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kLeInstructionMask | internal::PrimitiveIndex<NumType>();

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
    this->SerializeBinary(kIndex, buf);
  }
};

// TODO is this a UnaryInstruction<Addr>?
template <typename T>
struct LoadInstruction : base::Clone<LoadInstruction<T>, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kLoadInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;

  LoadInstruction(RegOr<Addr> const& addr) : addr(addr) {}
  ~LoadInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<T>(), " ", stringify(this->result),
                        " = load ", stringify(addr));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(addr.is_reg());
    addr.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  Reg result;
};

template <typename Inst>
inline constexpr bool IsLoadInstruction =
    ((Inst::kIndex >> internal::kTypeBits) ==
     (internal::kLoadInstructionMask >> internal::kTypeBits));

template <typename T>
struct StoreInstruction : base::Clone<StoreInstruction<T>, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kStoreInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;
  StoreInstruction(RegOr<T> const& value, RegOr<Addr> const& location)
      : value(value), location(location) {}
  ~StoreInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<T>(), " store ", stringify(this->value),
                        " -> ", stringify(location));
  }

  struct control_bits {
    uint8_t value_is_reg : 1;
    uint8_t location_is_reg : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{.value_is_reg    = value.is_reg(),
                             .location_is_reg = location.is_reg()});
    value.apply([&](auto v) { buf->append(v); });
    location.apply([&](auto v) { buf->append(v); });
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(value);
    inliner.Inline(location);
  }

  RegOr<T> value;
  RegOr<Addr> location;
};

template <typename Inst>
inline constexpr bool IsStoreInstruction =
    ((Inst::kIndex >> internal::kTypeBits) ==
     (internal::kStoreInstructionMask >> internal::kTypeBits));

template <typename T>
struct PrintInstruction : base::Clone<PrintInstruction<T>, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kPrintInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;
  PrintInstruction(RegOr<T> const& value) : value(value) {}
  ~PrintInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("print ", TypeToString<T>(), " ", stringify(value));
  }

  struct control_bits {
    bool value_is_reg;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{.value_is_reg = value.is_reg()});
    value.apply([&](auto v) { buf->append(v); });
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(value); }

  RegOr<T> value;
};

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction : base::Clone<PhiInstruction<T>, Instruction> {
  constexpr static cmd_index_t kIndex =
      internal::kPhiInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;

  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}
  ~PhiInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    std::string s = absl::StrCat("phi ", TypeToString<T>());
    for (size_t i = 0; i < blocks.size(); ++i) {
      absl::StrAppend(&s, "\n      ", stringify(blocks[i]), ": ",
                      stringify(values[i]));
    }
    return s;
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append<uint16_t>(values.size());
    for (auto block : blocks) { buf->append(block); }
    internal::WriteBits<uint16_t, RegOr<T>>(
        buf, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(values);
    inliner.Inline(result);
  }

  std::vector<BasicBlock const*> blocks;
  std::vector<RegOr<T>> values;
  Reg result;
};

template <typename Inst>
inline constexpr bool IsPhiInstruction =
    ((Inst::kIndex >> internal::kTypeBits) ==
     (internal::kPhiInstructionMask >> internal::kTypeBits));

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
  static constexpr cmd_index_t kIndex =
      internal::kRegisterInstructionMask | internal::PrimitiveIndex<T>();

  explicit RegisterInstruction(RegOr<T> const& operand)
      : UnaryInstruction<T>(operand) {}
  ~RegisterInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<T>(), " ", stringify(this->result), " = ",
                        stringify(this->operand));
  }

  static T Apply(T val) { return val; }

  void Serialize(base::untyped_buffer* buf) const override {
    this->SerializeUnary(kIndex, buf);
  }
};

template <typename T>
struct SetReturnInstruction
    : base::Clone<SetReturnInstruction<T>, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kSetReturnInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;

  SetReturnInstruction(uint16_t index, RegOr<T> const& value)
      : index(index), value(value) {}
  ~SetReturnInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    if constexpr (std::is_same_v<T, ::type::Type const*>) {
      return absl::StrCat(
          "set-ret ", index, " = ", TypeToString<T>(), " ",
          value.is_reg() ? stringify(value) : value.value()->to_string());
    } else {
      return absl::StrCat("set-ret ", index, " = ", TypeToString<T>(), " ",
                          stringify(value));
    }
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(index);
    buf->append(value.is_reg());
    value.apply([&](auto v) { buf->append(v); });
  }

  void Inline(Inliner const& inliner) override { NOT_YET(); }

  uint16_t index;
  RegOr<T> value;
};

template <typename Inst>
inline constexpr bool IsSetReturnInstruction =
    ((Inst::kIndex >> internal::kTypeBits) ==
     (internal::kSetReturnInstructionMask >> internal::kTypeBits));

template <typename ToType, typename FromType>
struct CastInstruction : Instruction {
  static constexpr cmd_index_t kIndex = internal::kCastInstructionIndex;
  using to_type                       = ToType;
  using from_type                     = FromType;

  explicit CastInstruction(RegOr<FromType> const& value) : value(value) {}
  ~CastInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = cast ", stringify(value), " to ",
                        TypeToString<ToType>());
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(value.is_reg());
    value.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(value);
    inliner.Inline(result);
  }

  RegOr<FromType> value;
  Reg result;
};

template <typename NumType>
struct NegInstruction : UnaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kNegInstructionMask | internal::PrimitiveIndex<NumType>();

  explicit NegInstruction(RegOr<NumType> const& operand)
      : UnaryInstruction<NumType>(operand) {}
  ~NegInstruction() override {}

  static NumType Apply(NumType operand) { return -operand; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = neg ", stringify(this->operand));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->SerializeUnary(kIndex, buf);
  }
};

template <typename Inst>
inline constexpr bool IsCastInstruction = (Inst::kIndex ==
                                           internal::kCastInstructionIndex);

struct GetReturnInstruction : base::Clone<GetReturnInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kGetReturnInstructionIndex;

  explicit GetReturnInstruction(uint16_t index) : index(index) {}
  ~GetReturnInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = get-ret ", index);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(index);
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override { NOT_YET(); }

  uint16_t index;
  Reg result;
};

// TODO this should work for flags too.
struct NotInstruction : UnaryInstruction<bool> {
  static constexpr cmd_index_t kIndex = internal::kNotInstructionNumber;

  explicit NotInstruction(RegOr<bool> const& operand)
      : UnaryInstruction<bool>(operand) {}
  ~NotInstruction() override {}

  static bool Apply(bool operand) { return not operand; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<bool>(), " ", stringify(this->result),
                        " = not ", stringify(this->operand));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->SerializeUnary(kIndex, buf);
  }
};

struct PtrInstruction : UnaryInstruction<type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kPtrInstructionNumber;

  explicit PtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~PtrInstruction() override {}

  static type::Pointer const* Apply(type::Type const* operand) {
    return type::Ptr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->SerializeUnary(kIndex, buf);
  }
};

struct BufPtrInstruction : UnaryInstruction<type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kBufPtrInstructionNumber;

  explicit BufPtrInstruction(RegOr<type::Type const*> const& operand)
      : UnaryInstruction<type::Type const*>(operand) {}
  ~BufPtrInstruction() override {}

  static type::BufferPointer const* Apply(type::Type const* operand) {
    return type::BufPtr(operand);
  }

  void Serialize(base::untyped_buffer* buf) const override {
    this->SerializeUnary(kIndex, buf);
  }
};

struct PrintEnumInstruction : base::Clone<PrintEnumInstruction, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kPrintInstructionMask | internal::PrimitiveIndex<EnumVal>();

  PrintEnumInstruction(RegOr<EnumVal> const& value, type::Enum const* enum_type)
      : value(value), enum_type(enum_type) {}
  ~PrintEnumInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("print enum ", enum_type->to_string(), " ",
                        stringify(value));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(value.is_reg());
    value.apply([&](auto v) { buf->append(v); });
    buf->append(enum_type);
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(value); }

  RegOr<EnumVal> value;
  type::Enum const* enum_type;
};

struct PrintFlagsInstruction : base::Clone<PrintFlagsInstruction, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kPrintInstructionMask | internal::PrimitiveIndex<FlagsVal>();

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
    buf->append(kIndex);
    buf->append(value.is_reg());
    value.apply([&](auto v) { buf->append(v); });
    buf->append(flags_type);
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(value); }

  RegOr<FlagsVal> value;
  type::Flags const* flags_type;
};

// TODO Morph this into interpretter break-point instructions.
struct DebugIrInstruction : base::Clone<DebugIrInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kDebugIrInstructionNumber;

  DebugIrInstruction() = default;
  ~DebugIrInstruction() override {}

  std::string to_string() const override { return "debug-ir"; }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(internal::kDebugIrInstructionNumber);
  }

  void Inline(Inliner const& inliner) override {}
};

struct XorFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kXorFlagsInstructionNumber;
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
    this->SerializeBinary(kIndex, buf);
  }
};

struct AndFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kAndFlagsInstructionNumber;
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
    this->SerializeBinary(kIndex, buf);
  }
};

struct OrFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kOrFlagsInstructionNumber;
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
    this->SerializeBinary(kIndex, buf);
  }
};

struct TupleInstruction : VariadicInstruction<type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kTupleInstructionNumber;

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
    SerializeVariadic(kIndex, buf);
  }
};

struct VariantInstruction : VariadicInstruction<type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kVariantInstructionNumber;

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
    this->SerializeVariadic(kIndex, buf);
  }
};

struct EnumerationInstruction
    : base::Clone<EnumerationInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kEnumerationInstructionNumber;

  enum class Kind { Enum, Flags };
  EnumerationInstruction(
      Kind k, module::BasicModule* mod, std::vector<std::string_view> names,
      absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values)
      : kind_(k),
        mod_(mod),
        names_(std::move(names)),
        specified_values_(std::move(specified_values)) {}
  ~EnumerationInstruction() override {}

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(kind_ == Kind::Enum);
    buf->append<uint16_t>(names_.size());
    buf->append<uint16_t>(specified_values_.size());
    buf->append(mod_);
    for (auto name : names_) { buf->append(name); }

    for (auto const& [index, val] : specified_values_) {
      // TODO these could be packed much more efficiently.
      buf->append(index);
      buf->append<bool>(val.is_reg());
      val.apply([&](auto v) { buf->append(v); });
    }

    buf->append(result);
  };

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result),
                        kind_ == Kind::Enum ? " = enum (" : " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(result); }

  Kind kind_;
  module::BasicModule* mod_;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values_;
  Reg result;
};

struct OpaqueTypeInstruction : base::Clone<OpaqueTypeInstruction, Instruction> {
  constexpr static cmd_index_t kIndex = internal::kOpaqueTypeInstructionNumber;
  OpaqueTypeInstruction(module::BasicModule const* mod) : mod(mod) {}
  ~OpaqueTypeInstruction() override {}

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(mod);
    buf->append(result);
  }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = opaque ", stringify(mod));
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(result); }

  module::BasicModule const* mod;
  Reg result;
};

struct ArrowInstruction : base::Clone<ArrowInstruction, Instruction> {
  constexpr static cmd_index_t kIndex = internal::kArrowInstructionNumber;

  ArrowInstruction(std::vector<RegOr<type::Type const*>> lhs,
                   std::vector<RegOr<type::Type const*>> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  ~ArrowInstruction() override {}

  static type::Type const* Apply(std::vector<type::Type const*> lhs,
                                 std::vector<type::Type const*> rhs) {
    return type::Func(std::move(lhs), std::move(rhs));
  }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(
        stringify(result), " = (",
        absl::StrJoin(lhs, ", ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }),
        ") -> (",
        absl::StrJoin(rhs, ", ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }),
        ")");
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        buf, lhs, [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(lhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        buf, rhs, [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(rhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  std::vector<RegOr<type::Type const*>> lhs, rhs;
  Reg result;
};

// Oddly named to be sure, this instruction is used to do initializations,
// copies, moves, or destructions of the given type.
struct StructManipulationInstruction
    : base::Clone<StructManipulationInstruction, Instruction> {
  constexpr static cmd_index_t kIndex =
      internal::kStructManipulationInstructionNumber;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };
  StructManipulationInstruction(Kind k, type::Type const* type, Reg from,
                                RegOr<Addr> to = RegOr<Addr>(Reg(0)))
      : kind(k), type(type), r(from), to(to) {}
  ~StructManipulationInstruction() override {}

  std::string to_string() const override {
    char const* name;
    switch (kind) {
      case Kind::Init:
        return absl::StrCat("init ", type->to_string(), " ", stringify(r));
      case Kind::Destroy:
        return absl::StrCat("destroy ", type->to_string(), " ", stringify(r));
      case Kind::Copy:
        return absl::StrCat("copy ", type->to_string(), " ", stringify(r), " ",
                            stringify(to));
      case Kind::Move:
        return absl::StrCat("move ", type->to_string(), " ", stringify(r), " ",
                            stringify(to));
      default: UNREACHABLE();
    }
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(kind);
    buf->append(type);
    buf->append(r);
    if (kind == Kind::Copy or kind == Kind::Move) {
      buf->append(to.is_reg());
      to.apply([&](auto v) { buf->append(v); });
    }
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(r);
    if (kind == Kind::Copy or kind == Kind::Move) { inliner.Inline(to); }
  }

  Kind kind;
  type::Type const* type;
  Reg r;
  RegOr<Addr> to;  // Only meaningful for copy and move
};

struct CallInstruction : base::Clone<CallInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kCallInstructionNumber;

  CallInstruction(type::Function const* fn_type, RegOr<AnyFunc> const& fn,
                  std::vector<Results> args, OutParams outs)
      : fn_type(fn_type),
        fn(fn),
        args(std::move(args)),
        outs(std::move(outs)) {}
  ~CallInstruction() override {}

  std::string to_string() const {
    using base::stringify;
    std::string result = absl::StrCat("call ", stringify(fn));
    for (auto const& arg : args) {
      absl::StrAppend(&result, "\n      ", stringify(arg));
    }

    // TODO out params
    return result;
  }

  void Serialize(base::untyped_buffer* buf) const override {
    ASSERT(args.size() == fn_type->input.size());
    buf->append(kIndex);
    buf->append(fn.is_reg());
    internal::WriteBits<uint16_t, Results>(buf, args, [](Results const& r) {
      ASSERT(r.size() == 1u);
      return r.is_reg(0);
    });

    fn.apply([&](auto v) { buf->append(v); });
    size_t bytes_written_slot = buf->reserve<core::Bytes>();
    size_t arg_index          = 0;
    for (Results const& arg : args) {
      if (arg.is_reg(0)) {
        buf->append(arg.get<Reg>(0));
      } else {
        type::Apply(fn_type->input[arg_index], [&](auto tag) {
          using T = typename decltype(tag)::type;
          buf->append(arg.get<T>(0).value());
        });
      }
      ++arg_index;
    }
    buf->set(bytes_written_slot, core::Bytes{buf->size() - bytes_written_slot -
                                             sizeof(core::Bytes)});

    // TODO this is probably wrong.
    buf->append<uint16_t>(fn_type->output.size());
    for (Reg r : outs.regs_) { buf->append(r); }
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(fn);
    NOT_YET();  // Because we need to do this for args and out params too, it's
                // tricky.
  }

  type::Function const* fn_type;
  RegOr<AnyFunc> fn;
  std::vector<Results> args;
  OutParams outs;
};

struct LoadSymbolInstruction : base::Clone<LoadSymbolInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kLoadSymbolInstructionNumber;
  LoadSymbolInstruction(std::string_view name, type::Type const* type)
      : name(name), type(type) {}
  ~LoadSymbolInstruction() override {}

  std::string to_string() const override {
    return absl::StrCat("load-symbol ", name, ": ", type->to_string());
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(name);
    buf->append(type);
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(result); }

  std::string_view name;
  type::Type const* type;
  Reg result;
};

struct ArrayInstruction : base::Clone<ArrayInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kArrayInstructionNumber;
  using length_t                      = int64_t;

  ArrayInstruction(RegOr<length_t> length, RegOr<type::Type const*> data_type)
      : length(length), data_type(data_type) {}
  ~ArrayInstruction() override {}

  std::string to_string() const override {
    return absl::StrCat(stringify(result), " = array ", stringify(length),
                        stringify(data_type));
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{.length_is_reg = length.is_reg(),
                             .type_is_reg   = data_type.is_reg()});

    length.apply([&](auto v) { buf->append(v); });
    data_type.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(length);
    inliner.Inline(data_type);
    inliner.Inline(result);
  }

  RegOr<length_t> length;
  RegOr<type::Type const*> data_type;
  Reg result;
};

struct StructInstruction : base::Clone<StructInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kStructInstructionNumber;
  StructInstruction(ast::Scope const* scope, std::vector<StructField> fields)
      : scope(scope), fields(std::move(fields)) {}
  ~StructInstruction() override {}

  std::string to_string() const override {
    // TODO
    return absl::StrCat(stringify(result), " = struct TODO");
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append<uint16_t>(fields.size());
    buf->append(scope);

    // TODO shuffling fields order?
    for (auto const& field : fields) { buf->append(field.name()); }

    std::vector<RegOr<type::Type const*>> types;
    types.reserve(fields.size());
    for (auto const& field : fields) { types.push_back(field.type()); }
    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        buf, types,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(types, [&](RegOr<type::Type const*> x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    for (auto& field : fields) { inliner.Inline(field.type()); }
  }

  ast::Scope const* scope;
  std::vector<StructField> fields;
  Reg result;
};

struct TypeInfoInstruction : base::Clone<TypeInfoInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kTypeInfoInstructionNumber;
  enum class Kind : uint8_t { Alignment = 0, Bytes = 2 };
  TypeInfoInstruction(Kind kind, RegOr<type::Type const*> type)
      : kind(kind), type(type) {}
  ~TypeInfoInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(
        stringify(result),
        kind == Kind::Alignment ? " = alignment " : " = bytes ",
        type.is_reg() ? stringify(type.reg()) : type.value()->to_string());
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append<uint8_t>(static_cast<uint8_t>(kind) |
                         static_cast<uint8_t>(type.is_reg()));
    type.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override { inliner.Inline(type); }

  Kind kind;
  RegOr<type::Type const*> type;
  Reg result;
};

struct MakeBlockInstruction : base::Clone<MakeBlockInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kMakeBlockInstructionNumber;

  MakeBlockInstruction(BlockDef* block_def, std::vector<RegOr<AnyFunc>> befores,
                       std::vector<RegOr<Jump*>> afters)
      : block_def(block_def),
        befores(std::move(befores)),
        afters(std::move(afters)) {}
  ~MakeBlockInstruction() override {}

  // TODO
  std::string to_string() const override { return "make-block "; }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(block_def);
    internal::WriteBits<uint16_t, RegOr<AnyFunc>>(
        buf, befores, [](RegOr<AnyFunc> const& r) { return r.is_reg(); });
    absl::c_for_each(befores, [&](RegOr<AnyFunc> x) {
      x.apply([&](auto v) { buf->append(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        buf, afters, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(afters, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { buf->append(v); });
    });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(befores);
    inliner.Inline(afters);
    inliner.Inline(result);
  }

  BlockDef* block_def;
  std::vector<RegOr<AnyFunc>> befores;
  std::vector<RegOr<Jump*>> afters;
  Reg result;
};

struct MakeScopeInstruction : base::Clone<MakeScopeInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kMakeScopeInstructionNumber;

  MakeScopeInstruction(ScopeDef* scope_def, std::vector<RegOr<Jump*>> inits,
                       std::vector<RegOr<AnyFunc>> dones,
                       absl::flat_hash_map<std::string_view, BlockDef*> blocks)
      : scope_def(scope_def),
        inits(std::move(inits)),
        dones(std::move(dones)),
        blocks(std::move(blocks)) {}
  ~MakeScopeInstruction() override {}

  // TODO
  std::string to_string() const override { return "make-scope"; }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(scope_def);

    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        buf, inits, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(inits, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { buf->append(v); });
    });
    internal::WriteBits<uint16_t, RegOr<AnyFunc>>(
        buf, dones, [](RegOr<AnyFunc> const& r) { return r.is_reg(); });
    absl::c_for_each(dones, [&](RegOr<AnyFunc> x) {
      x.apply([&](auto v) { buf->append(v); });
    });

    buf->append<uint16_t>(blocks.size());
    for (auto [name, block] : blocks) {
      buf->append(name);
      buf->append(block);
    }
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(inits);
    inliner.Inline(dones);
    inliner.Inline(result);
  }

  ScopeDef* scope_def;
  std::vector<RegOr<Jump*>> inits;
  std::vector<RegOr<AnyFunc>> dones;
  absl::flat_hash_map<std::string_view, BlockDef*> blocks;
  Reg result;
};

struct StructIndexInstruction
    : base::Clone<StructIndexInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kStructIndexInstructionNumber;
  using type                          = type::Struct const*;

  StructIndexInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                         type::Struct const* struct_type)
      : addr(addr), index(index), struct_type(struct_type) {}
  ~StructIndexInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (struct ",
                        struct_type->to_string(), ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    buf->append(struct_type);
    addr.apply([&](auto v) { buf->append(v); });
    index.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  type::Struct const* struct_type;
  Reg result;
};

struct TupleIndexInstruction : base::Clone<TupleIndexInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kTupleIndexInstructionNumber;
  using type                          = type::Tuple const*;

  TupleIndexInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                        type::Tuple const* tuple)
      : addr(addr), index(index), tuple(tuple) {}
  ~TupleIndexInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (tuple ", tuple->to_string(),
                        ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    buf->append(tuple);
    addr.apply([&](auto v) { buf->append(v); });
    index.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  type::Tuple const* tuple;
  Reg result;
};

struct PtrIncrInstruction : base::Clone<PtrIncrInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kPtrIncrInstructionNumber;
  using type                          = type::Pointer const*;

  PtrIncrInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                     type::Pointer const* ptr)
      : addr(addr), index(index), ptr(ptr) {}
  ~PtrIncrInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (ptr ", ptr->to_string(),
                        ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    buf->append(ptr);
    addr.apply([&](auto v) { buf->append(v); });
    index.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  type::Pointer const* ptr;
  Reg result;
};

struct VariantAccessInstruction
    : base::Clone<VariantAccessInstruction, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kVariantAccessInstructionNumber;
  VariantAccessInstruction(RegOr<Addr> const& var, bool get_value)
      : var(var), get_value(get_value) {}
  ~VariantAccessInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = variant-",
                        get_value ? "value " : "type ", stringify(var));
  }

  void Serialize(base::untyped_buffer* buf) const override {
    buf->append(kIndex);
    buf->append(get_value);
    buf->append(var.is_reg());
    var.apply([&](auto v) { buf->append(v); });
    buf->append(result);
  }

  void Inline(Inliner const& inliner) override {
    inliner.Inline(var);
    inliner.Inline(result);
  }

  RegOr<Addr> var;
  bool get_value;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_H
