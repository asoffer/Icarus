#ifndef ICARUS_IR_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTIONS_H

#include <memory>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/scope/scope.h"
#include "base/clone.h"
#include "ir/block_def.h"
#include "ir/instruction_inliner.h"
#include "ir/instruction_op_codes.h"
#include "ir/instructions_base.h"
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
void WriteBits(ByteCodeWriter* writer, absl::Span<T const> span,
               Fn&& predicate) {
  ASSERT(span.size() < std::numeric_limits<SizeType>::max());
  writer->Write<SizeType>(span.size());

  uint8_t reg_mask = 0;
  for (size_t i = 0; i < span.size(); ++i) {
    if (predicate(span[i])) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      writer->Write(reg_mask);
      reg_mask = 0;
    }
  }
  if (span.size() % 8 != 0) { writer->Write(reg_mask); }
}

}  // namespace internal

template <typename NumType>
struct UnaryInstruction : base::Clone<UnaryInstruction<NumType>, Instruction> {
  using type = NumType;

  explicit UnaryInstruction(RegOr<NumType> const& operand) : operand(operand) {}
  ~UnaryInstruction() override {}

  void WriteByteCode(ByteCodeWriter* writer) const override {
    UNREACHABLE("Should call a child class");
  }

  void WriteByteCodeUnary(cmd_index_t index, ByteCodeWriter* writer) const {
    writer->Write(index);
    writer->Write(operand.is_reg());
    operand.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  };

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    UNREACHABLE("Should call a child class");
  }

  void WriteByteCodeBinary(cmd_index_t index, ByteCodeWriter* writer) const {
    writer->Write(index);
    writer->Write(control_bits{
        .lhs_is_reg = lhs.is_reg(),
        .rhs_is_reg = rhs.is_reg(),
    });

    lhs.apply([&](auto v) { writer->Write(v); });
    rhs.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    UNREACHABLE("Should call a child class");
  }

  void WriteByteCodeVariadic(cmd_index_t index, ByteCodeWriter* writer) const {
    writer->Write(index);
    internal::WriteBits<uint16_t, RegOr<T>>(
        writer, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  };

  void Inline(InstructionInliner const& inliner) override {
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
  } else if constexpr (std::is_same_v<T, ir::Addr>) {
    return "addr";
  } else {
    DEBUG_LOG()(typeid(T).name());
    return "[[unknown]]";
    // TODO enumerate all possibilities
    // static_assert(base::always_false<T>());
  }
}

template <typename NumType>
struct AddInstruction
    : base::Clone<AddInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kAddInstructionMask | internal::PrimitiveIndex<NumType>();

  AddInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<AddInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                         rhs) {}
  ~AddInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs + rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = add ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct SubInstruction
    : base::Clone<SubInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kSubInstructionMask | internal::PrimitiveIndex<NumType>();

  SubInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<SubInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                         rhs) {}
  ~SubInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs - rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = sub ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct MulInstruction
    : base::Clone<MulInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kMulInstructionMask | internal::PrimitiveIndex<NumType>();

  MulInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<MulInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                         rhs) {}
  ~MulInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs * rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = mul ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct DivInstruction
    : base::Clone<DivInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kDivInstructionMask | internal::PrimitiveIndex<NumType>();

  DivInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<DivInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                         rhs) {}
  ~DivInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs / rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = div ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct ModInstruction
    : base::Clone<ModInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kModInstructionMask | internal::PrimitiveIndex<NumType>();

  ModInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<ModInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                         rhs) {}
  ~ModInstruction() override {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs % rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(TypeToString<NumType>(), " ", stringify(this->result),
                        " = mod ", stringify(this->lhs), " ",
                        stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct EqInstruction
    : base::Clone<EqInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kEqInstructionMask | internal::PrimitiveIndex<NumType>();

  EqInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<EqInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                        rhs) {}
  ~EqInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs == rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = eq ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct NeInstruction
    : base::Clone<NeInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kNeInstructionMask | internal::PrimitiveIndex<NumType>();

  NeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<NeInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                        rhs) {}
  ~NeInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs != rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = ne ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct LtInstruction
    : base::Clone<LtInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kLtInstructionMask | internal::PrimitiveIndex<NumType>();

  LtInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<LtInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                        rhs) {}
  ~LtInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs < rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = lt ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct LeInstruction
    : base::Clone<LeInstruction<NumType>, BinaryInstruction<NumType>> {
  static constexpr cmd_index_t kIndex =
      internal::kLeInstructionMask | internal::PrimitiveIndex<NumType>();

  LeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : base::Clone<LeInstruction<NumType>, BinaryInstruction<NumType>>(lhs,
                                                                        rhs) {}
  ~LeInstruction() override {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs <= rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = le ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct LoadInstruction : base::Clone<LoadInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kLoadInstructionNumber;

  explicit LoadInstruction(Reg addr, uint16_t num_bytes)
      : num_bytes(num_bytes), addr(addr) {}
  ~LoadInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = load ", stringify(addr));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(num_bytes);
    writer->Write(addr);
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(result);
  }

  uint16_t num_bytes;
  Reg addr, result;
};

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{.value_is_reg    = value.is_reg(),
                               .location_is_reg = location.is_reg()});
    value.apply([&](auto v) { writer->Write(v); });
    location.apply([&](auto v) { writer->Write(v); });
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{.value_is_reg = value.is_reg()});
    value.apply([&](auto v) { writer->Write(v); });
  }

  void Inline(InstructionInliner const& inliner) override { inliner.Inline(value); }

  RegOr<T> value;
};

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction : base::Clone<PhiInstruction<T>, Instruction> {
  constexpr static cmd_index_t kIndex =
      internal::kPhiInstructionMask | internal::PrimitiveIndex<T>();
  using type = T;

  PhiInstruction() = default;
  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}
  ~PhiInstruction() override {}

  void add(BasicBlock const* block, RegOr<T> value) {
    blocks.push_back(block);
    values.push_back(value);
  }

  std::string to_string() const override {
    using base::stringify;
    std::string s =
        absl::StrCat(stringify(result), " = phi ", TypeToString<T>());
    for (size_t i = 0; i < blocks.size(); ++i) {
      absl::StrAppend(&s, "\n      ", stringify(blocks[i]), ": ",
                      stringify(values[i]));
    }
    return s;
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write<uint16_t>(values.size());
    for (auto block : blocks) { writer->Write(block); }
    internal::WriteBits<uint16_t, RegOr<T>>(
        writer, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeUnary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(index);
    writer->Write(value.is_reg());
    value.apply([&](auto v) { writer->Write(v); });
  }

  void Inline(InstructionInliner const& inliner) override { NOT_YET(); }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(value.is_reg());
    value.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeUnary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(index);
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override { NOT_YET(); }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeUnary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeUnary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeUnary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(value.is_reg());
    value.apply([&](auto v) { writer->Write(v); });
    writer->Write(enum_type);
  }

  void Inline(InstructionInliner const& inliner) override { inliner.Inline(value); }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(value.is_reg());
    value.apply([&](auto v) { writer->Write(v); });
    writer->Write(flags_type);
  }

  void Inline(InstructionInliner const& inliner) override { inliner.Inline(value); }

  RegOr<FlagsVal> value;
  type::Flags const* flags_type;
};

// TODO Morph this into interpretter break-point instructions.
struct DebugIrInstruction : base::Clone<DebugIrInstruction, Instruction> {
  static constexpr cmd_index_t kIndex = internal::kDebugIrInstructionNumber;

  DebugIrInstruction() = default;
  ~DebugIrInstruction() override {}

  std::string to_string() const override { return "debug-ir"; }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(internal::kDebugIrInstructionNumber);
  }

  void Inline(InstructionInliner const& inliner) override {}
};

struct XorFlagsInstruction
    : base::Clone<XorFlagsInstruction, BinaryInstruction<FlagsVal>> {
  static constexpr cmd_index_t kIndex = internal::kXorFlagsInstructionNumber;
  XorFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      :base::Clone<XorFlagsInstruction, BinaryInstruction<FlagsVal>>(lhs, rhs) {}
  ~XorFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs ^ rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = add ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct AndFlagsInstruction
    : base::Clone<AndFlagsInstruction, BinaryInstruction<FlagsVal>> {
  static constexpr cmd_index_t kIndex = internal::kAndFlagsInstructionNumber;
  AndFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : base::Clone<AndFlagsInstruction, BinaryInstruction<FlagsVal>>(lhs,
                                                                      rhs) {}
  ~AndFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs & rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = and ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct OrFlagsInstruction
    : base::Clone<OrFlagsInstruction, BinaryInstruction<FlagsVal>> {
  static constexpr cmd_index_t kIndex = internal::kOrFlagsInstructionNumber;
  OrFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : base::Clone<OrFlagsInstruction, BinaryInstruction<FlagsVal>>(lhs,
                                                                      rhs) {}
  ~OrFlagsInstruction() override {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs | rhs; }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = or ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeBinary(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    WriteByteCodeVariadic(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    this->WriteByteCodeVariadic(kIndex, writer);
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(kind_ == Kind::Enum);
    writer->Write<uint16_t>(names_.size());
    writer->Write<uint16_t>(specified_values_.size());
    writer->Write(mod_);
    for (auto name : names_) { writer->Write(name); }

    for (auto const& [index, val] : specified_values_) {
      // TODO these could be packed much more efficiently.
      writer->Write(index);
      writer->Write<bool>(val.is_reg());
      val.apply([&](auto v) { writer->Write(v); });
    }

    writer->Write(result);
  };

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result),
                        kind_ == Kind::Enum ? " = enum (" : " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  void Inline(InstructionInliner const& inliner) override { inliner.Inline(result); }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(mod);
    writer->Write(result);
  }

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = opaque ", stringify(mod));
  }

  void Inline(InstructionInliner const& inliner) override { inliner.Inline(result); }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, lhs,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(lhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, rhs,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(rhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  std::vector<RegOr<type::Type const*>> lhs, rhs;
  Reg result;
};

// Oddly named to be sure, this instruction is used to do initializations,
// copies, moves, or destructions of the given type.
struct TypeManipulationInstruction
    : base::Clone<TypeManipulationInstruction, Instruction> {
  constexpr static cmd_index_t kIndex =
      internal::kTypeManipulationInstructionNumber;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };
  TypeManipulationInstruction(Kind k, type::Type const* type, Reg from,
                              RegOr<Addr> to = RegOr<Addr>(Reg(0)))
      : kind(k), type(type), r(from), to(to) {}
  ~TypeManipulationInstruction() override {}

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(kind);
    writer->Write(type);
    writer->Write(r);
    if (kind == Kind::Copy or kind == Kind::Move) {
      writer->Write(to.is_reg());
      to.apply([&](auto v) { writer->Write(v); });
    }
  }

  void Inline(InstructionInliner const& inliner) override {
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
      absl::StrAppend(&result, "\n      -> ", stringify(arg));
    }
    for (size_t i = 0; i < fn_type->output.size(); ++i) {
      absl::StrAppend(&result, "\n      <- ", stringify(outs[i]));
    }

    return result;
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    ASSERT(args.size() == fn_type->input.size());
    writer->Write(kIndex);
    writer->Write(fn.is_reg());
    fn.apply([&](auto v) { writer->Write(v); });
    size_t bytes_written_slot = writer->buf_->reserve<core::Bytes>();

    internal::WriteBits<uint16_t, Results>(writer, args, [](Results const& r) {
      ASSERT(r.size() == 1u);
      return r.is_reg(0);
    });

    size_t arg_index = 0;
    for (Results const& arg : args) {
      if (arg.is_reg(0)) {
        writer->Write(arg.get<Reg>(0));
      } else {
        type::Apply(fn_type->input[arg_index], [&](auto tag) {
          using T = typename decltype(tag)::type;
          writer->Write(arg.get<T>(0).value());
        });
      }
      ++arg_index;
    }

    outs.WriteByteCode(writer);

    writer->buf_->set(bytes_written_slot,
                      core::Bytes{writer->buf_->size() - bytes_written_slot -
                                  sizeof(core::Bytes)});
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(name);
    writer->Write(type);
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(result);
  }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{.length_is_reg = length.is_reg(),
                               .type_is_reg   = data_type.is_reg()});

    length.apply([&](auto v) { writer->Write(v); });
    data_type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write<uint16_t>(fields.size());
    writer->Write(scope);

    // TODO shuffling fields order?
    for (auto const& field : fields) { writer->Write(field.name()); }

    std::vector<RegOr<type::Type const*>> types;
    types.reserve(fields.size());
    for (auto const& field : fields) { types.push_back(field.type()); }
    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, types,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(types, [&](RegOr<type::Type const*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write<uint8_t>(static_cast<uint8_t>(kind) |
                           static_cast<uint8_t>(type.is_reg()));
    type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(type);
  }

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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(block_def);
    internal::WriteBits<uint16_t, RegOr<AnyFunc>>(
        writer, befores, [](RegOr<AnyFunc> const& r) { return r.is_reg(); });
    absl::c_for_each(befores, [&](RegOr<AnyFunc> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, afters, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(afters, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(scope_def);

    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, inits, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(inits, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<AnyFunc>>(
        writer, dones, [](RegOr<AnyFunc> const& r) { return r.is_reg(); });
    absl::c_for_each(dones, [&](RegOr<AnyFunc> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write<uint16_t>(blocks.size());
    for (auto [name, block] : blocks) {
      writer->Write(name);
      writer->Write(block);
    }
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    writer->Write(struct_type);
    addr.apply([&](auto v) { writer->Write(v); });
    index.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    writer->Write(tuple);
    addr.apply([&](auto v) { writer->Write(v); });
    index.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(control_bits{
        .reg_addr  = addr.is_reg(),
        .reg_index = index.is_reg(),
    });

    writer->Write(ptr);
    addr.apply([&](auto v) { writer->Write(v); });
    index.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  type::Pointer const* ptr;
  Reg result;
};

struct ByteViewLengthInstruction
    : base::Clone<ByteViewLengthInstruction, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewLengthInstructionNumber;

  explicit ByteViewLengthInstruction(Reg reg) : reg(reg) {}
  ~ByteViewLengthInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = byte-view length ",
                        stringify(reg));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(reg);
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(reg);
    inliner.Inline(result);
  }

  Reg reg;
  Reg result;
};

struct ByteViewDataInstruction
    : base::Clone<ByteViewDataInstruction, Instruction> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewDataInstructionNumber;

  explicit ByteViewDataInstruction(Reg reg) : reg(reg) {}
  ~ByteViewDataInstruction() override {}

  std::string to_string() const override {
    using base::stringify;
    return absl::StrCat(stringify(result), " = byte-view data ",
                        stringify(reg));
  }

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(reg);
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(reg);
    inliner.Inline(result);
  }

  Reg reg;
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

  void WriteByteCode(ByteCodeWriter* writer) const override {
    writer->Write(kIndex);
    writer->Write(get_value);
    writer->Write(var.is_reg());
    var.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) override {
    inliner.Inline(var);
    inliner.Inline(result);
  }

  RegOr<Addr> var;
  bool get_value;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_H
