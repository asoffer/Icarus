#ifndef ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H

#include <memory>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/scope/scope.h"
#include "base/extend.h"
#include "base/meta.h"
#include "ir/block_def.h"
#include "ir/instruction/base.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/util.h"
#include "ir/out_params.h"
#include "ir/struct_field.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg_or.h"
#include "ir/value/string.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/util.h"
#include "type/variant.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {
namespace internal {

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

template <typename T>
struct WriteByteCodeExtension {
  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(T::kIndex);
    auto write = [&](auto const& field) {
      using field_type = std::decay_t<decltype(field)>;
      if constexpr (IsRegOr<field_type>::value) {
        writer->Write(field.is_reg());
        field.apply([&](auto v) { writer->Write(v); });
      } else {
        writer->Write(field);
      }
    };
    std::apply([&](auto const&... field) { (write(field), ...); },
               static_cast<T const*>(this)->field_refs());
  }
};

template <typename T>
struct InlineExtension {
  void Inline(InstructionInliner const& inliner) {
    auto inline_register = [&](auto& field) {
      using field_type = std::decay_t<decltype(field)>;
      if constexpr (base::meta<field_type> == base::meta<Reg> or
                    IsRegOr<field_type>::value) {
        inliner.Inline(field);
      }
    };
    std::apply([&](auto&... field) { (inline_register(field), ...); },
               static_cast<T*>(this)->field_refs());
  }
};

template <typename NumType>
struct UnaryInstruction {
  using type = NumType;

  explicit UnaryInstruction(RegOr<NumType> const& operand) : operand(operand) {}
  ~UnaryInstruction() {}

  void WriteByteCode(ByteCodeWriter* writer) const {
    UNREACHABLE("Should call a child class");
  }

  void WriteByteCodeUnary(cmd_index_t index, ByteCodeWriter* writer) const {
    writer->Write(index);
    writer->Write(operand.is_reg());
    operand.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  };

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(operand);
    inliner.Inline(result);
  }

  RegOr<NumType> operand;
  Reg result;
};

template <typename NumType>
struct BinaryInstruction {
  using type = NumType;

  BinaryInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : lhs(lhs), rhs(rhs) {}
  ~BinaryInstruction() {}

  struct control_bits {
    uint8_t lhs_is_reg : 1;
    uint8_t rhs_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename T>
struct VariadicInstruction {
  using type = T;

  VariadicInstruction(std::vector<RegOr<T>> values)
      : values(std::move(values)) {}
  ~VariadicInstruction() {}

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(values);
    inliner.Inline(result);
  }

  std::vector<RegOr<T>> values;
  Reg result;
};

template <typename NumType>
struct AddInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kAddInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  AddInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~AddInstruction() {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs + rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = add ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct SubInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kSubInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  SubInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~SubInstruction() {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs - rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = sub ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct MulInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kMulInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  MulInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~MulInstruction() {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs * rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = mul ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct DivInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kDivInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  DivInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~DivInstruction() {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs / rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = div ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct ModInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kModInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  ModInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~ModInstruction() {}

  static NumType Apply(NumType lhs, NumType rhs) { return lhs % rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = mod ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct EqInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kEqInstructionRange.start + internal::PrimitiveIndex<NumType>();

  EqInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~EqInstruction() {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs == rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = eq ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct NeInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kNeInstructionRange.start + internal::PrimitiveIndex<NumType>();

  NeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~NeInstruction() {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs != rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = ne ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct LtInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kLtInstructionRange.start + internal::PrimitiveIndex<NumType>();

  LtInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~LtInstruction() {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs < rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = lt ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

template <typename NumType>
struct LeInstruction : BinaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex =
      internal::kLeInstructionRange.start + internal::PrimitiveIndex<NumType>();

  LeInstruction(RegOr<NumType> const& lhs, RegOr<NumType> const& rhs)
      : BinaryInstruction<NumType>(lhs, rhs) {}
  ~LeInstruction() {}

  static bool Apply(NumType lhs, NumType rhs) { return lhs <= rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("bool ", stringify(this->result), " = le ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct LoadInstruction {
  static constexpr cmd_index_t kIndex = internal::kLoadInstructionNumber;

  explicit LoadInstruction(RegOr<Addr> addr, uint16_t num_bytes)
      : num_bytes(num_bytes), addr(addr) {}
  ~LoadInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = load ", stringify(addr));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(num_bytes);
    writer->Write(addr.is_reg());
    addr.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(addr);
    inliner.Inline(result);
  }

  uint16_t num_bytes;
  RegOr<Addr> addr;
  Reg result;
};

template <typename T>
struct StoreInstruction {
  static constexpr cmd_index_t kIndex =
      internal::kStoreInstructionRange.start + internal::PrimitiveIndex<T>();
  using type = T;
  StoreInstruction(RegOr<T> const& value, RegOr<Addr> const& location)
      : value(value), location(location) {}
  ~StoreInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<T>(), " store ",
                        stringify(this->value), " -> [", stringify(location),
                        "]");
  }

  struct control_bits {
    uint8_t value_is_reg : 1;
    uint8_t location_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(control_bits{.value_is_reg    = value.is_reg(),
                               .location_is_reg = location.is_reg()});
    value.apply([&](auto v) { writer->Write(v); });
    location.apply([&](auto v) { writer->Write(v); });
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(value);
    inliner.Inline(location);
  }

  RegOr<T> value;
  RegOr<Addr> location;
};

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction {
  constexpr static cmd_index_t kIndex =
      internal::kPhiInstructionRange.start + internal::PrimitiveIndex<T>();
  using type = T;

  PhiInstruction() = default;
  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}
  ~PhiInstruction() {}

  void add(BasicBlock const* block, RegOr<T> value) {
    blocks.push_back(block);
    values.push_back(value);
  }

  std::string to_string() const {
    using base::stringify;
    std::string s =
        absl::StrCat(stringify(result), " = phi ", internal::TypeToString<T>());
    for (size_t i = 0; i < blocks.size(); ++i) {
      absl::StrAppend(&s, "\n      ", stringify(blocks[i]), ": ",
                      stringify(values[i]));
    }
    return s;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(values);
    inliner.Inline(result);
  }

  std::vector<BasicBlock const*> blocks;
  std::vector<RegOr<T>> values;
  Reg result;
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
  static constexpr cmd_index_t kIndex =
      internal::kRegisterInstructionRange.start + internal::PrimitiveIndex<T>();

  explicit RegisterInstruction(RegOr<T> const& operand)
      : UnaryInstruction<T>(operand) {}
  ~RegisterInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<T>(), " ",
                        stringify(this->result), " = ",
                        stringify(this->operand));
  }

  static T Apply(T val) { return val; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeUnary(kIndex, writer);
  }
};

template <typename T>
struct SetReturnInstruction
    : base::Extend<SetReturnInstruction<T>>::template With<
          WriteByteCodeExtension, InlineExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kSetReturnInstructionRange.start +
      internal::PrimitiveIndex<T>();
  using type = T;

  std::string to_string() const {
    using base::stringify;
    if constexpr (std::is_same_v<T, ::type::Type const*>) {
      return absl::StrCat(
          "set-ret ", index, " = ", internal::TypeToString<T>(), " ",
          value.is_reg() ? stringify(value) : value.value()->to_string());
    } else {
      return absl::StrCat("set-ret ", index, " = ", internal::TypeToString<T>(),
                          " ", stringify(value));
    }
  }

  uint16_t index;
  RegOr<T> value;
};

template <typename FromType>
struct CastInstruction : base::Extend<CastInstruction<FromType>>::template With<
                             WriteByteCodeExtension, InlineExtension> {
  static constexpr cmd_index_t kIndex = internal::kCastInstructionRange.start +
                                        internal::PrimitiveIndex<FromType>();
  using from_type = FromType;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = cast ", stringify(value),
                        " to type indexed by ", static_cast<int>(to_type_byte));
  }

  RegOr<FromType> value;
  uint8_t to_type_byte;
  Reg result;
};

template <typename NumType>
struct NegInstruction : UnaryInstruction<NumType> {
  static constexpr cmd_index_t kIndex = internal::kNegInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();

  explicit NegInstruction(RegOr<NumType> const& operand)
      : UnaryInstruction<NumType>(operand) {}
  ~NegInstruction() {}

  static NumType Apply(NumType operand) { return -operand; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<NumType>(), " ",
                        stringify(this->result), " = neg ",
                        stringify(this->operand));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeUnary(kIndex, writer);
  }
};

struct GetReturnInstruction
    : base::Extend<GetReturnInstruction>::With<WriteByteCodeExtension,
                                               InlineExtension> {
  static constexpr cmd_index_t kIndex = internal::kGetReturnInstructionIndex;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = get-ret ", index);
  }

  uint16_t index;
  Reg result;
};

// TODO this should work for flags too.
struct NotInstruction : UnaryInstruction<bool> {
  static constexpr cmd_index_t kIndex = internal::kNotInstructionNumber;

  explicit NotInstruction(RegOr<bool> const& operand)
      : UnaryInstruction<bool>(operand) {}
  ~NotInstruction() {}

  static bool Apply(bool operand) { return not operand; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(internal::TypeToString<bool>(), " ",
                        stringify(this->result), " = not ",
                        stringify(this->operand));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeUnary(kIndex, writer);
  }
};

struct PtrInstruction : UnaryInstruction<::type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kPtrInstructionNumber;

  explicit PtrInstruction(RegOr<::type::Type const*> const& operand)
      : UnaryInstruction<::type::Type const*>(operand) {}
  ~PtrInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("type ", stringify(this->result), " = ptr ",
                        stringify(this->operand));
  }

  static ::type::Pointer const* Apply(::type::Type const* operand) {
    return ::type::Ptr(operand);
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeUnary(kIndex, writer);
  }
};

struct BufPtrInstruction : UnaryInstruction<::type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kBufPtrInstructionNumber;

  explicit BufPtrInstruction(RegOr<::type::Type const*> const& operand)
      : UnaryInstruction<::type::Type const*>(operand) {}
  ~BufPtrInstruction() {}

  static ::type::BufferPointer const* Apply(::type::Type const* operand) {
    return ::type::BufPtr(operand);
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("type ", stringify(this->result), " = buf-ptr ",
                        stringify(this->operand));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeUnary(kIndex, writer);
  }
};

// TODO Morph this into interpretter break-point instructions.
struct DebugIrInstruction {
  static constexpr cmd_index_t kIndex = internal::kDebugIrInstructionNumber;

  DebugIrInstruction() = default;
  ~DebugIrInstruction() {}

  std::string to_string() const { return "debug-ir"; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(internal::kDebugIrInstructionNumber);
  }

  void Inline(InstructionInliner const& inliner) {}
};

struct XorFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kXorFlagsInstructionNumber;
  XorFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~XorFlagsInstruction() {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs ^ rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = add ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct AndFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kAndFlagsInstructionNumber;
  AndFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~AndFlagsInstruction() {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs & rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = and ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct OrFlagsInstruction : BinaryInstruction<FlagsVal> {
  static constexpr cmd_index_t kIndex = internal::kOrFlagsInstructionNumber;
  OrFlagsInstruction(RegOr<FlagsVal> const& lhs, RegOr<FlagsVal> const& rhs)
      : BinaryInstruction<FlagsVal>(lhs, rhs) {}
  ~OrFlagsInstruction() {}

  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs | rhs; }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("flags ", stringify(this->result), " = or ",
                        stringify(this->lhs), " ", stringify(this->rhs));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeBinary(kIndex, writer);
  }
};

struct TupleInstruction : VariadicInstruction<::type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kTupleInstructionNumber;

  TupleInstruction(std::vector<RegOr<::type::Type const*>> values)
      : VariadicInstruction<::type::Type const*>(std::move(values)) {}
  ~TupleInstruction() {}

  static ::type::Type const* Apply(std::vector<::type::Type const*> entries) {
    return ::type::Tup(std::move(entries));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("type ", stringify(this->result), " = tup ",
                        absl::StrJoin(this->values, " ",
                                      [](std::string* out,
                                         RegOr<::type::Type const*> const& r) {
                                        out->append(stringify(r));
                                      }));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    WriteByteCodeVariadic(kIndex, writer);
  }
};

struct VariantInstruction : VariadicInstruction<::type::Type const*> {
  static constexpr cmd_index_t kIndex = internal::kVariantInstructionNumber;

  VariantInstruction(std::vector<RegOr<::type::Type const*>> values)
      : VariadicInstruction<::type::Type const*>(std::move(values)) {}
  ~VariantInstruction() {}

  static ::type::Type const* Apply(std::vector<::type::Type const*> entries) {
    return ::type::Var(std::move(entries));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat("type ", stringify(this->result), " = var ",
                        absl::StrJoin(this->values, " ",
                                      [](std::string* out,
                                         RegOr<::type::Type const*> const& r) {
                                        out->append(stringify(r));
                                      }));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    this->WriteByteCodeVariadic(kIndex, writer);
  }
};

struct EnumerationInstruction {
  static constexpr cmd_index_t kIndex = internal::kEnumerationInstructionNumber;

  enum class Kind { Enum, Flags };
  EnumerationInstruction(
      Kind k, module::BasicModule* mod, std::vector<std::string_view> names,
      absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values)
      : kind_(k),
        mod_(mod),
        names_(std::move(names)),
        specified_values_(std::move(specified_values)) {}
  ~EnumerationInstruction() {}

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result),
                        kind_ == Kind::Enum ? " = enum (" : " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  void Inline(InstructionInliner const& inliner) { inliner.Inline(result); }

  Kind kind_;
  module::BasicModule* mod_;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values_;
  Reg result;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<WriteByteCodeExtension,
                                                InlineExtension> {
  constexpr static cmd_index_t kIndex = internal::kOpaqueTypeInstructionNumber;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = opaque ", stringify(mod));
  }

  module::BasicModule const* mod;
  Reg result;
};

struct ArrowInstruction {
  constexpr static cmd_index_t kIndex = internal::kArrowInstructionNumber;

  ArrowInstruction(std::vector<RegOr<type::Type const*>> lhs,
                   std::vector<RegOr<type::Type const*>> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  ~ArrowInstruction() {}

  // TODO take parameters, or a span if you don't want construction of names?
  static type::Type const* Apply(std::vector<type::Type const*> const& lhs,
                                 std::vector<type::Type const*> rhs) {
    // TODO named arguments
    core::Params<type::QualType> params;
    params.reserve(lhs.size());
    for (auto* t : lhs) {
      // TODO push qualtype into `Apply` parameters
      params.append(core::AnonymousParam(type::QualType::NonConstant(t)));
    }
    return type::Func(std::move(params), std::move(rhs));
  }

  std::string to_string() const {
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

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  std::vector<RegOr<type::Type const*>> lhs, rhs;
  Reg result;
};

// Oddly named to be sure, this instruction is used to do initializations,
// copies, moves, or destructions of the given type.
struct TypeManipulationInstruction {
  constexpr static cmd_index_t kIndex =
      internal::kTypeManipulationInstructionNumber;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };
  TypeManipulationInstruction(Kind k, type::Type const* type, Reg from,
                              RegOr<Addr> to = RegOr<Addr>(Reg(0)))
      : kind(k), type(type), r(from), to(to) {}
  ~TypeManipulationInstruction() {}

  std::string to_string() const {
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

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(kind);
    writer->Write(type);
    writer->Write(r);
    if (kind == Kind::Copy or kind == Kind::Move) {
      writer->Write(to.is_reg());
      to.apply([&](auto v) { writer->Write(v); });
    }
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(r);
    if (kind == Kind::Copy or kind == Kind::Move) { inliner.Inline(to); }
  }

  Kind kind;
  type::Type const* type;
  Reg r;
  RegOr<Addr> to;  // Only meaningful for copy and move
};

struct CallInstruction {
  static constexpr cmd_index_t kIndex = internal::kCallInstructionNumber;

  CallInstruction(type::Function const* fn_type, RegOr<Fn> const& fn,
                  std::vector<Value> args, OutParams outs)
      : fn_type_(fn_type),
        fn_(fn),
        args_(std::move(args)),
        outs_(std::move(outs)) {
    ASSERT(this->outs_.size() == fn_type_->output().size());
    ASSERT(args_.size() == fn_type_->params().size());
  }

  ~CallInstruction() {}

  std::string to_string() const {
    using base::stringify;
    std::string result = absl::StrCat("call ", stringify(fn_));
    for (auto const& arg : args_) {
      absl::StrAppend(&result, "\n      -> ", stringify(arg));
    }
    for (size_t i = 0; i < fn_type_->output().size(); ++i) {
      absl::StrAppend(&result, "\n      <- ", stringify(outs_[i]));
    }

    return result;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(fn_.is_reg());
    fn_.apply([&](auto v) { writer->Write(v); });
    size_t bytes_written_slot = writer->buf_->reserve<core::Bytes>();

    size_t arg_index = 0;
    for (Value const& arg : args_) {
      Reg const* r = arg.get_if<Reg>();
      writer->Write(static_cast<bool>(r));
      if (r) {
        writer->Write(*r);
      } else {
        type::Apply(fn_type_->params()[arg_index].value.type(), [&](auto tag) {
          using T = typename decltype(tag)::type;
          writer->Write(arg.get<T>());
        });
      }
      ++arg_index;
    }

    outs_.WriteByteCode(writer);

    writer->buf_->set(bytes_written_slot,
                      core::Bytes{writer->buf_->size() - bytes_written_slot -
                                  sizeof(core::Bytes)});
  }

  RegOr<Fn> func() const { return fn_; }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(fn_);
    for (auto& arg : args_) { inliner.Inline(arg); }
    for (auto& reg : outs_.regs()) { inliner.Inline(reg); }
  }

 private:
  type::Function const* fn_type_;
  RegOr<Fn> fn_;
  std::vector<Value> args_;
  OutParams outs_;
};

struct LoadSymbolInstruction
    : base::Extend<LoadSymbolInstruction>::With<WriteByteCodeExtension,
                                                InlineExtension> {
  static constexpr cmd_index_t kIndex = internal::kLoadSymbolInstructionNumber;

  std::string to_string() const {
    return absl::StrCat("load-symbol ", name.get(), ": ", type->to_string());
  }

  String name;
  type::Type const* type;
  Reg result;
};

struct ArrayInstruction {
  static constexpr cmd_index_t kIndex = internal::kArrayInstructionNumber;
  using length_t                      = int64_t;

  ArrayInstruction(RegOr<length_t> length, RegOr<type::Type const*> data_type)
      : length(length), data_type(data_type) {}
  ~ArrayInstruction() {}

  std::string to_string() const {
    return absl::StrCat(stringify(result), " = array ", stringify(length),
                        stringify(data_type));
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(control_bits{.length_is_reg = length.is_reg(),
                               .type_is_reg   = data_type.is_reg()});

    length.apply([&](auto v) { writer->Write(v); });
    data_type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(length);
    inliner.Inline(data_type);
    inliner.Inline(result);
  }

  RegOr<length_t> length;
  RegOr<type::Type const*> data_type;
  Reg result;
};

struct StructInstruction {
  static constexpr cmd_index_t kIndex = internal::kStructInstructionNumber;
  StructInstruction(module::BasicModule const* mod, type::Struct* s,
                    std::vector<StructField> fields, std::optional<ir::Fn> dtor)
      : mod(mod), struct_(s), fields(std::move(fields)), dtor(dtor) {}
  ~StructInstruction() {}

  std::string to_string() const {
    // TODO
    return absl::StrCat(stringify(result), " = struct TODO");
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write<uint16_t>(fields.size());
    writer->Write(mod);
    writer->Write(struct_);

    // TODO shuffling fields order?
    for (auto const& field : fields) {
      writer->Write(field.name());
      if (auto* v = field.initial_value()) {
        writer->Write(true);
        writer->Write(field.type().value());
        // TODO serialize a different way writer->Write(*v);
      } else {
        writer->Write(false);
        writer->Write(field.type());
      }
    }

    writer->Write(dtor.has_value());
    if (dtor.has_value()) { writer->Write(*dtor); }

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    for (auto& field : fields) {
      if (auto* r = field.type_reg()) { inliner.Inline(*r); }
      if (auto* v = field.initial_value()) {
        if (auto* r = v->get_if<Reg>()) { inliner.Inline(*r); }
      }
    }
  }

  module::BasicModule const* mod;
  type::Struct* struct_;
  std::vector<StructField> fields;
  std::optional<ir::Fn> dtor;
  Reg result;
};

struct TypeInfoInstruction {
  static constexpr cmd_index_t kIndex = internal::kTypeInfoInstructionNumber;
  enum class Kind : uint8_t { Alignment = 0, Bytes = 2 };
  TypeInfoInstruction(Kind kind, RegOr<type::Type const*> type)
      : kind(kind), type(type) {}
  ~TypeInfoInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result),
        kind == Kind::Alignment ? " = alignment " : " = bytes ",
        type.is_reg() ? stringify(type.reg()) : type.value()->to_string());
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write<uint8_t>(static_cast<uint8_t>(kind) |
                           static_cast<uint8_t>(type.is_reg()));
    type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) { inliner.Inline(type); }

  Kind kind;
  RegOr<type::Type const*> type;
  Reg result;
};

struct MakeBlockInstruction {
  static constexpr cmd_index_t kIndex = internal::kMakeBlockInstructionNumber;

  MakeBlockInstruction(BlockDef* block_def, std::vector<RegOr<Fn>> befores,
                       std::vector<RegOr<Jump*>> afters)
      : block_def(block_def),
        befores(std::move(befores)),
        afters(std::move(afters)) {}
  ~MakeBlockInstruction() {}

  // TODO
  std::string to_string() const { return "make-block "; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(block_def);
    internal::WriteBits<uint16_t, RegOr<Fn>>(
        writer, befores, [](RegOr<Fn> const& r) { return r.is_reg(); });
    absl::c_for_each(befores, [&](RegOr<Fn> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, afters, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(afters, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(befores);
    inliner.Inline(afters);
    inliner.Inline(result);
  }

  BlockDef* block_def;
  std::vector<RegOr<Fn>> befores;
  std::vector<RegOr<Jump*>> afters;
  Reg result;
};

struct MakeScopeInstruction {
  static constexpr cmd_index_t kIndex = internal::kMakeScopeInstructionNumber;

  MakeScopeInstruction(ScopeDef* scope_def, std::vector<RegOr<Jump*>> inits,
                       std::vector<RegOr<Fn>> dones,
                       absl::flat_hash_map<std::string_view, BlockDef*> blocks)
      : scope_def(scope_def),
        inits(std::move(inits)),
        dones(std::move(dones)),
        blocks(std::move(blocks)) {}
  ~MakeScopeInstruction() {}

  // TODO
  std::string to_string() const { return "make-scope"; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(scope_def);

    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, inits, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(inits, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Fn>>(
        writer, dones, [](RegOr<Fn> const& r) { return r.is_reg(); });
    absl::c_for_each(dones, [&](RegOr<Fn> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write<uint16_t>(blocks.size());
    for (auto [name, block] : blocks) {
      writer->Write(name);
      writer->Write(block);
    }
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(inits);
    inliner.Inline(dones);
    inliner.Inline(result);
  }

  ScopeDef* scope_def;
  std::vector<RegOr<Jump*>> inits;
  std::vector<RegOr<Fn>> dones;
  absl::flat_hash_map<std::string_view, BlockDef*> blocks;
  Reg result;
};

struct StructIndexInstruction {
  static constexpr cmd_index_t kIndex = internal::kStructIndexInstructionNumber;
  using type                          = type::Struct const*;

  StructIndexInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                         ::type::Struct const* struct_type)
      : addr(addr), index(index), struct_type(struct_type) {}
  ~StructIndexInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (struct ",
                        struct_type->to_string(), ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Struct const* struct_type;
  Reg result;
};

struct TupleIndexInstruction {
  static constexpr cmd_index_t kIndex = internal::kTupleIndexInstructionNumber;
  using type                          = type::Tuple const*;

  TupleIndexInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                        ::type::Tuple const* tuple)
      : addr(addr), index(index), tuple(tuple) {}
  ~TupleIndexInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (tuple ", tuple->to_string(),
                        ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Tuple const* tuple;
  Reg result;
};

struct PtrIncrInstruction {
  static constexpr cmd_index_t kIndex = internal::kPtrIncrInstructionNumber;
  using type                          = type::Pointer const*;

  PtrIncrInstruction(RegOr<Addr> const& addr, RegOr<int64_t> index,
                     ::type::Pointer const* ptr)
      : addr(addr), index(index), ptr(ptr) {}
  ~PtrIncrInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = index ", stringify(index),
                        " of ", stringify(addr), " (ptr ", ptr->to_string(),
                        ")");
  }

  struct control_bits {
    uint8_t reg_addr : 1;
    uint8_t reg_index : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
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

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(addr);
    inliner.Inline(index);
    inliner.Inline(result);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Pointer const* ptr;
  Reg result;
};

struct ByteViewLengthInstruction
    : base::Extend<ByteViewLengthInstruction>::With<WriteByteCodeExtension,
                                                    InlineExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewLengthInstructionNumber;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = byte-view length ",
                        stringify(reg));
  }

  Reg reg;
  Reg result;
};

struct ByteViewDataInstruction
    : base::Extend<ByteViewDataInstruction>::With<WriteByteCodeExtension,
                                                  InlineExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewDataInstructionNumber;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = byte-view data ",
                        stringify(reg));
  }

  Reg reg;
  Reg result;
};

struct VariantAccessInstruction
    : base::Extend<VariantAccessInstruction>::With<WriteByteCodeExtension,
                                                   InlineExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kVariantAccessInstructionNumber;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = variant-",
                        get_value ? "value " : "type ", stringify(var));
  }

  RegOr<Addr> var;
  bool get_value;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
